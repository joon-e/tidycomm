#' Compute one-way ANOVAs
#'
#' Computes one-way ANOVAs for one group variable and specified test variables.
#' If no variables are specified, all numeric (integer or double) variables are
#' used. A Levene's test will automatically determine whether a classic ANOVA is used.
#' Otherwise Welch's ANOVA with a (Satterthwaite's) approximation to the degrees of freedom is used.
#'
#' @param data a [tibble][tibble::tibble-package] or a [tdcmm] model
#' @param group_var group variable (column name)
#' @param ... test variables (column names). Leave empty to compute ANOVAs for
#'   all numeric variables in data.
#' @param descriptives a logical indicating whether descriptive statistics (mean
#'   & standard deviation) for all group levels should be added to the returned
#'   tibble. Defaults to `FALSE`.
#' @param post_hoc a logical value indicating whether post-hoc tests should be performed.
#' Tukey's HSD is employed when the assumption of equal variances is met, whereas the Games-Howell test is
#' automatically applied when this assumption is violated. The results of the
#' post-hoc test will be added to a list column in the resulting tibble.
#'
#' @return a [tdcmm] model
#'
#' @family ANOVA
#'
#' @examples
#' WoJ %>% unianova(employment, autonomy_selection, autonomy_emphasis)
#' WoJ %>% unianova(employment, descriptives = TRUE, post_hoc = TRUE)
#' \dontrun{
#' WoJ %>% unianova(employment)
#' }
#'
#' @export
unianova <- function(data, group_var, ..., descriptives = FALSE, post_hoc = FALSE) {

  # Check if group_var is provided
  if (missing(group_var)) {
    stop("Please provide at least one variable.")
  }

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
  model_list_annova <- list()
  model_list_levene <- list()
  out_annova <- NULL
  out_levene <- NULL
  for (test_var in test_vars) {

    # Compute Levene test
    group_var_string <- as_label(enquo(group_var))
    test_var_string <- as_label(enquo(test_var))

    levene_test <- suppressWarnings(
      car::leveneTest(as.formula(
        paste0("`", test_var_string, "` ~ `", group_var_str, "`")),
        data = data)
    )

    levene_row <- data.frame(
      Variable = as.character(test_var),
      Levene_p = round(levene_test$`Pr(>F)`[1], digits = 3)
    )

    # preparation
    formula <- as.formula(paste("`", test_var_string, "`",
                                " ~ ",
                                "`", group_var_string, "`",
                                sep = ""))

    # Compute and create output based on Levene's test
    if (levene_row$Levene_p < 0.05) {
      # Unequal variances, use Welch's ANOVA
      equal_var_assumption <- FALSE
      data_frame <- (as.data.frame(data))
      aov_model <- misty::test.welch(formula, data_frame, effsize = TRUE, output = FALSE, posthoc = TRUE)
      } else {
      # Equal variances, use regular ANOVA
      equal_var_assumption <- TRUE
      data_frame <- (as.data.frame(data))
      aov_model <- misty::aov.b(formula, data_frame, effsize = TRUE, output = FALSE, posthoc = TRUE)
    }

    data <- tibble::as_tibble(data_frame)
    aov_model_row <- format_aov(aov_model, {{ test_var }}, data, {{ group_var }},
                                descriptives, post_hoc, equal_var_assumption)
    # Collect ANOVA
    model_list_annova[[length(model_list_annova) + 1]] <- aov_model
    out_annova <- out_annova %>%
      dplyr::bind_rows(aov_model_row)

    # Collect levene test
    if (equal_var_assumption == FALSE)
      levene_row <- levene_row %>%
      dplyr::mutate(var_equal = "FALSE")
    else {
      levene_row <- levene_row %>%
        dplyr::mutate(var_equal = "TRUE")
    }

    model_list_levene[[length(model_list_levene) + 1]] <- levene_row
    out_levene <- out_levene %>%
      dplyr::bind_rows(levene_row)
  }

  if (levene_row$Levene_p < 0.05) {
    # Unequal variances, Welch's ANOVA used
    message(glue("The significant result from Levene's test suggests unequal variances among the groups, violating standard ANOVA assumptions. This necessitates the use of Welch's ANOVA, which is robust against heteroscedasticity."))
  }

  out <- dplyr::full_join(out_annova, out_levene, by = "Variable")

  # Output
  return(new_tdcmm_nnv(
    new_tdcmm(out,
              func = "unianova",
              data = data,
              params = list(group_var = group_var_string,
                            vars = test_vars_string,
                            descriptives = descriptives,
                            post_hoc = post_hoc),
              model = model_list_annova))
  )
}

#' @rdname visualize
#' @export
visualize.tdcmm_nnv<- function(x, ..., .design = design_lmu()) {
  if (attr(x, "func") == "unianova") {
    return(visualize_unianova(x, .design))
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
                       post_hoc, equal_var_assumption) {

  test_var_string <- as_label(enquo(test_var))
  group_var_string <- as_label(enquo(group_var))

  if (equal_var_assumption == TRUE) {
    F_val <- aov_model$result[[2]][5]
    df_number <- aov_model$result[[2]][3]
    p <- aov_model$result[[2]][6]
    eta_squared <- aov_model$result[[2]][7]
    omega_squared <- aov_model$result[[2]][8]

    aov_df <- tibble::tibble(
      Variable = test_var_string,
      `F` = pillar::num(F_val$F[1], digits = 3),
      df_num = round(df_number[1,1], digits = 0),
      df_denom = round(df_number[2,1], digits = 0),
      p = pillar::num(p$pval[1], digits = 3),
      eta_squared = pillar::num(eta_squared$eta.sq[1], digits = 3)
    )
  }
  else {
    F_val <- aov_model$result[[2]][1]
    df_num <- aov_model$result[[2]][2]
    df_denom <- aov_model$result[[2]][3]
    p <- aov_model$result[[2]][4]
    eta_squared <- aov_model$result[[2]][5]
    omega_squared <- aov_model$result[[2]][6]

    aov_df <- tibble::tibble(
      Variable = test_var_string,
      `F` = pillar::num(F_val$F, digits = 3),
      df_num = round(df_num$df1, digits = 0),
      df_denom = round(df_denom$df2, digits = 0),
      p = pillar::num(p$pval, digits = 3),
      omega_squared = pillar::num(omega_squared$omega.sq, digits = 3)
    )
  }

  if (descriptives) {
    desc_df <- data %>%
      dplyr::group_by({{ group_var }}) %>%
      dplyr::summarise(M = mean({{ test_var }}, na.rm = TRUE),
                       SD = sd({{ test_var }}, na.rm = TRUE)
      ) %>%
      dplyr::mutate_if(is.numeric, format, 3) %>%
      dplyr::mutate(M = as.numeric(M),
                    SD = as.numeric(SD)) %>%
      tidyr::pivot_longer(cols = c("M", "SD"),
                          names_to = "stat",
                          values_to = "val")

    desc_df <- desc_df %>%
      dplyr::group_by({{ group_var }}) %>%
      dplyr::mutate(order_var = dplyr::cur_group_id()) %>%
      dplyr::ungroup() %>%
      tidyr::unite("name", "order_var", "stat", {{ group_var }}) %>%
      dplyr::mutate(name = stringr::str_replace_all(name, " ", "_")) %>%
      tidyr::spread(name, val) %>%
      dplyr::rename_all(stringr::str_replace, "\\d*_", "")

    aov_df <- aov_df %>%
      dplyr::bind_cols(desc_df)
  }

  if (post_hoc && equal_var_assumption == FALSE) {
    # Grab the post hoc tibble from the misty::test.welch object
    posthoc <- aov_model$result[3]
    posthoc <- posthoc$posthoc
    posthoc <- posthoc %>%
      dplyr::rename(Delta_M = m.diff,
                    p = pval,
                    conf_lower = m.low,
                    conf_upper = m.upp) %>%
      dplyr::mutate(Group_Var = group_var_string) %>%
      dplyr::mutate(contrast = stringr::str_c(group1, group2, sep = "-")) %>%
      dplyr::select(-c(group1, group2, d.low, d.upp)) %>%
      dplyr::select(Group_Var, contrast, Delta_M, conf_lower, conf_upper, p, d, tidyselect::everything()) %>%
      dplyr::mutate(df = as.double(as.integer(df)))

    ph_df <- tibble::tibble(
      post_hoc = list(posthoc)
    )
    aov_df <- aov_df %>%
      dplyr::bind_cols(ph_df)
  }

  if (post_hoc && equal_var_assumption == TRUE) {
    posthoc <- aov_model$result[3]
    posthoc <- posthoc$posthoc

    # Get descriptive statistics from aov.b output
    descript <- aov_model$result[1]
    groups <- descript[[1]][,1]

    # Initialize vectors to hold the t, se, and df values
    t_values <- vector("numeric", length = nrow(posthoc))
    se_values <- vector("numeric", length = nrow(posthoc))
    df_values <- vector("numeric", length = nrow(posthoc))

    counter <- 1

    # Loop over each group
    for (i in 1:(length(groups) - 1)) {
      for (j in (i + 1):length(groups)) {

        # Get the necessary statistics from the descript output
        n1 <- descript$descript[,2][i]
        n2 <- descript$descript[,2][j]
        sd1 <- descript$descript[,5][i]
        sd2 <- descript$descript[,5][j]
        M1 <- descript$descript[,4][i]
        M2 <- descript$descript[,4][j]

        # Calculate SE, df and t for each group comparison
        se <- sqrt((sd1^2 / n1) + (sd2^2 / n2))
        df <- n1 + n2 - 2
        t <- (M1 - M2) / se

        # Use the counter as the index
        se_values[counter] <- se
        df_values[counter] <- df
        t_values[counter] <- t

        # Increment the counter
        counter <- counter + 1
      }
    }

    # Add the calculated values to the posthoc tibble
    posthoc <- posthoc %>%
      dplyr::mutate(
        se = se_values,
        df = df_values,
        t = t_values
      )

    posthoc <- posthoc %>%
      dplyr::rename(Delta_M = m.diff,
                    p = pval,
                    conf_lower = m.low,
                    conf_upper = m.upp) %>%
      dplyr::mutate(Group_Var = group_var_string) %>%
      dplyr::mutate(contrast = stringr::str_c(group1, group2, sep = "-")) %>%
      dplyr::select(-c(group1, group2, d.low, d.upp)) %>%
      dplyr::select(Group_Var, contrast, Delta_M, conf_lower, conf_upper, p, d, se, t, df)


    # Add the updated posthoc tibble back to the ph_df tibble
    ph_df <- tibble::tibble(
      post_hoc = list(posthoc)
    )

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
visualize_unianova <- function(x, design = design_lmu()) {
  # get variables
  group_var_str <- attr(x, "params")$group_var
  group_var <- sym(group_var_str)

  test_vars_str <- attr(x, "params")$vars
  test_vars_str <- test_vars_str[test_vars_str != group_var_str]
  test_vars <- syms(test_vars_str)

  # if not inclusive of descriptives, re-do the call respectively
  if (!attr(x, "params")$descriptives) {
    x <- suppressMessages(unianova(attr(x, "data"),
                                   !!group_var,
                                   !!!test_vars,
                                   descriptives = TRUE,
                                   post_hoc = FALSE))
  }

  # prepare data
  data <- x %>%
    dplyr::select("Variable", tidyselect::starts_with(c("M_", "SD_")))

  n <- attr(x, "data") %>%
    dplyr::count({{ group_var }}, name = "N") %>%
    tidyr::pivot_wider(names_from = {{ group_var }},
                       values_from = "N",
                       names_prefix = "N_")

  data <- data %>%
    dplyr::bind_cols(n) %>%
    tidyr::pivot_longer(-c("Variable"),
                        names_to = "level") %>%
    dplyr::mutate(var = stringr::str_split_i(level, "_", 1),
                  level = stringr::str_split_i(level, "_", 2)) %>%
    tidyr::pivot_wider(names_from = "var",
                       values_from = "value") %>%
    dplyr::mutate(ci_95_ll = calculate_ci_ll(M, SD, N),
                  ci_95_ul = calculate_ci_ul(M, SD, N))

  # last check
  if (length(dplyr::n_distinct(data$level)) > 12) {
    stop(glue("Cannot visualize ANOVAs with more than 12 levels of the ",
              "group variable ({group_var_str})."),
         call. = FALSE)
  }

  # visualize
  data %>%
    dplyr::mutate(Variable = forcats::as_factor(Variable),
                  Variable_desc = forcats::fct_rev(Variable)) %>%
    ggplot2::ggplot(ggplot2::aes(xmin = ci_95_ll,
                                 x = M,
                                 xmax = ci_95_ul,
                                 y = Variable_desc,
                                 color = level)) +
    ggplot2::geom_pointrange(stat = "identity",
                             position = ggplot2::position_dodge2(width = 0.9),
                             linewidth = design$main_size) +
    ggplot2::scale_x_continuous(NULL,
                                n.breaks = 8) +
    ggplot2::scale_y_discrete(NULL) +
    ggplot2::scale_color_manual(NULL,
                                values = design$main_colors,
                                guide = ggplot2::guide_legend(reverse = TRUE)) +
    design$theme() +
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
