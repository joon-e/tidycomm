#' Compute one-way ANOVAs
#'
#' Computes one-way ANOVAs for one group variable and specified test variables.
#' If no variables are specified, all numeric (integer or double) variables are
#' used.
#'
#' @param data a [tibble][tibble::tibble-package] or a [tdcmm] model
#' @param group_var group variable (column name)
#' @param ... test variables (column names). Leave empty to compute ANOVAs for
#'   all numeric variables in data.
#' @param descriptives a logical indicating whether descriptive statistics (mean
#'   & standard deviation) for all group levels should be added to the returned
#'   tibble. Defaults to `FALSE`.
#' @param post_hoc a logical indicating whether post-hoc tests (Tukey's HSD)
#'   should be computed. Results of the post-hoc test will be added in a list
#'   column of result tibbles.
#'
#' @return a [tdcmm] model
#'
#' @family ANOVA
#'
#' @examples
#' WoJ %>% unianova(employment, autonomy_selection, autonomy_emphasis)
#' WoJ %>% unianova(employment)
#' WoJ %>% unianova(employment, descriptives = TRUE, post_hoc = TRUE)
#'
#' @export
unianova <- function(data, group_var, ..., descriptives = FALSE, post_hoc = FALSE) {

  # Get vars
  test_vars <- grab_vars(data, quos(...))
  test_vars_string <- purrr::map_chr(test_vars, as_label)

  # Get group var name
  group_var_str <- as_label(quo({{ group_var }}))
  if (group_var_str %in% test_vars_string) {
    test_vars <- syms(test_vars_string[test_vars_string != group_var_str])
  }

  # To factor if not factor
  if (!is.factor(dplyr::pull(data, {{ group_var }}))) {
    data <- data %>%
      dplyr::mutate_at(dplyr::vars({{ group_var }}), forcats::as_factor)
  }

  # Main function
  model_list <- list()
  out <- NULL
  for (test_var in test_vars) {

    # preparation
    group_var_string <- as_label(enquo(group_var))
    test_var_string <- as_label(enquo(test_var))
    formula <- as.formula(paste("`", test_var_string, "`",
                                " ~ ",
                                "`", group_var_string, "`",
                                sep = ""))

    # Compute and create output
    aov_model <- aov(formula, data)
    aov_model_row <- format_aov(aov_model, {{ test_var }}, data, {{ group_var }},
                                descriptives, post_hoc)

    # collect
    model_list[[length(model_list) + 1]] <- aov_model
    out <- out %>%
      dplyr::bind_rows(aov_model_row)
  }

  # Output
  return(new_tdcmm_nnv(
    new_tdcmm(out,
              func = "unianova",
              data = data,
              params = list(group_var = group_var_string,
                            vars = test_vars_string,
                            descriptives = descriptives,
                            post_hoc = post_hoc),
              model = model_list))
  )
}

#' @rdname visualize
#' @export
visualize.tdcmm_nnv<- function(x, ...) {
  if (attr(x, "func") == "unianova") {
    return(visualize_unianova(x))
  }

  return(warn_about_missing_visualization(x))
}

### Internal functions ###

## Format provided one-way ANOVA
##
## Outputs a one-way ANOVA for one test variable
##
## @inheritParams unianova
## @param aov_model aov model
## @param test_var Test variable
##
## @return a [tibble][tibble::tibble-package]
##
## @family ANOVA
##
## @keywords internal
format_aov <- function(aov_model, test_var, data, group_var, descriptives,
                       post_hoc) {

  test_var_string <- as_label(enquo(test_var))
  aov_s <- broom::tidy(aov_model)

  aov_df <- tibble(
    Var = test_var_string,
    `F` = aov_s$statistic[1],
    df_num = aov_s$df[1],
    df_denom = aov_s$df[2],
    p = aov_s$p.value[1],
    eta_squared = aov_s$sumsq[1] / sum(aov_s$sumsq)
  )

  if (descriptives) {
    desc_df <- data %>%
      dplyr::group_by({{ group_var }}) %>%
      dplyr::summarise(M = mean({{ test_var }}, na.rm = TRUE),
                       SD = sd({{ test_var }}, na.rm = TRUE)) %>%
      tidyr::gather("stat", "val", "M", "SD")

    desc_df <- desc_df %>%
      dplyr::group_by({{ group_var }}) %>%
      dplyr::mutate(order_var = dplyr::cur_group_id()) %>%
      dplyr::ungroup() %>%
      tidyr::unite("name", "order_var", "stat", {{ group_var }}) %>%
      dplyr::mutate(name = stringr::str_replace_all(.data$name, " ", "_")) %>%
      tidyr::spread(.data$name, .data$val) %>%
      dplyr::rename_all(stringr::str_replace, "\\d*_", "")

    aov_df <- aov_df %>%
      dplyr::bind_cols(desc_df)
  }

  if (post_hoc) {
    ph_df <- tibble::tibble(
      post_hoc = list(
        broom::tidy(TukeyHSD(aov_model))
      ))

    aov_df <- aov_df %>%
      dplyr::bind_cols(ph_df)
  }

  return(aov_df)
}

## Visualize `unianova()` as points with 95% CI ranges
##
## @param x a [tdcmm] model
##
## @return a [ggplot2] object
##
## @family tdcmm visualize
#
## @keywords internal
visualize_unianova <- function(x) {
  # get variables
  group_var_str <- attr(x, "params")$group_var
  group_var <- sym(group_var_str)

  test_vars_str <- attr(x, "params")$vars
  test_vars_str <- test_vars_str[test_vars_str != group_var_str]
  test_vars <- syms(test_vars_str)

  # if not inclusive of descriptives, re-do the call respectively
  if (!attr(x, "params")$descriptives) {
    x <- unianova(attr(x, "data"),
                  !!group_var,
                  !!!test_vars,
                  descriptives = TRUE,
                  post_hoc = FALSE)
  }

  # prepare data
  data <- x %>%
    dplyr::select("Var", tidyselect::starts_with(c("M_", "SD_")))

  n <- attr(x, "data") %>%
    dplyr::count({{ group_var }}, name = "N") %>%
    tidyr::pivot_wider(names_from = {{ group_var }},
                       values_from = "N",
                       names_prefix = "N_")

  data <- data %>%
    dplyr::bind_cols(n) %>%
    tidyr::pivot_longer(-c("Var"),
                        names_to = "level") %>%
    dplyr::mutate(var = stringr::str_split_i(.data$level, "_", 1),
                  level = stringr::str_split_i(.data$level, "_", 2)) %>%
    tidyr::pivot_wider(names_from = "var",
                       values_from = "value") %>%
    dplyr::mutate(ci_95_ll = calculate_ci_ll(.data$M, .data$SD, .data$N),
                  ci_95_ul = calculate_ci_ul(.data$M, .data$SD, .data$N))

  # last check
  if (length(dplyr::n_distinct(data$level)) > 12) {
    stop(glue("Cannot visualize ANOVAs with more than 12 levels of the ",
              "group variable ({group_var_str})."),
         call. = FALSE)
  }

  # visualize
  data %>%
    dplyr::mutate(Variable = forcats::as_factor(.data$Var),
                  Variable_desc = forcats::fct_rev(.data$Variable)) %>%
    ggplot2::ggplot(ggplot2::aes(xmin = .data$ci_95_ll,
                                 x = .data$M,
                                 xmax = .data$ci_95_ul,
                                 y = .data$Variable_desc,
                                 color = .data$level)) +
    ggplot2::geom_pointrange(stat = "identity",
                             position = ggplot2::position_dodge2(width = 0.9),
                             linewidth = tdcmm_visual_defaults()$main_size) +
    ggplot2::scale_x_continuous(NULL,
                                n.breaks = 8) +
    ggplot2::scale_y_discrete(NULL) +
    ggplot2::scale_color_manual(NULL,
                                values = tdcmm_visual_defaults()$main_colors,
                                guide = ggplot2::guide_legend(reverse = TRUE)) +
    tdcmm_visual_defaults()$theme() +
    ggplot2::theme(legend.position = "bottom")
}


# Constructors ----

new_tdcmm_nnv <- function(x) {
  stopifnot(is_tdcmm(x))

  structure(
    x,
    class = c("tdcmm_nnv", class(x))
  )
}
