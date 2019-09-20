#' Compute one-way ANOVAs
#'
#' Computes one-way ANOVAS for one group variable and specified test variables.
#' If no variables are specified, all numeric (integer or double) variables are
#' used.
#'
#' @param data a [tibble][tibble::tibble-package]
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
#' @return a [tibble][tibble::tibble-package]
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
  purrr::map_dfr(test_vars, compute_aov, data, {{ group_var }},
                 descriptives, post_hoc)

}

### Internal functions ###

## Compute one-way ANOVA
##
## Computes and outputs a one-way ANOVA for one test variable
##
## @inheritParams unianova
## @param test_var Test variable
##
## @return a [tibble][tibble::tibble-package]
##
## @family ANOVA
##
## @keywords internal
compute_aov <- function(test_var, data, group_var, descriptives, post_hoc) {
  group_var_string <- as_label(enquo(group_var))
  test_var_string <- as_label(enquo(test_var))

  formula <- as.formula(paste(test_var_string, group_var_string, sep = " ~ "))

  aov_model <- aov(formula, data)

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
      tidyr::gather("stat", "val", .data$M, .data$SD)

    desc_df <- desc_df %>%
      dplyr::mutate(order_var = dplyr::group_indices(desc_df, {{ group_var }})) %>%
      tidyr::unite("name", .data$order_var, .data$stat, {{ group_var }}) %>%
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
