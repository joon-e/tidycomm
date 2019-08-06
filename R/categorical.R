# Contains functions for the description of categorical variables

#' Tabulate frequencies
#'
#' Tabulates frequencies for one or more categorical variable, including relative,
#' and cumulative frequencies.
#'
#' @param data a [tibble][tibble::tibble-package]
#' @param ... Variables to tabulate
#'
#' @return a [tibble][tibble::tibble-package]
#'
#' @examples
#' iris %>% tab_frequencies(Species)
#' mtcars %>% tab_frequencies(vs, am)
#'
#' @family Descriptives
#'
#' @export
tab_frequencies <- function(data, ...) {
  grouping <- dplyr::groups(data)

  d <- data %>%
    dplyr::group_by(..., !!!grouping) %>%
    dplyr::summarise(n = dplyr::n()) %>%
    dplyr::group_by(!!!grouping) %>%
    dplyr::mutate(percent = n / sum(n)) %>%
    dplyr::arrange(!!!grouping)

  d %>%
    dplyr::bind_cols(d %>%
                dplyr::select(!!!grouping,
                       cum_n = n,
                       cum_percent = percent) %>%
                dplyr::mutate_at(dplyr::vars(-dplyr::group_cols()), cumsum)
    )

}


#' Crosstab variables
#'
#' Computes contigency table for one independent (column) variable and one or
#' more dependent (row) variables.
#'
#' @param data a [tibble][tibble::tibble-package]
#' @param col_var Independent (column) variable.
#' @param ... Dependent (row) variables.
#' @param add_total Logical indicating whether a 'Total' column should be
#'   computed. Defaults to FALSE.
#' @param percentages Logical indicating whether to output column-wise
#'   percentages instead of absolute values. Defaults to FALSE.
#' @param chi_square Logical indicating whether a Chi-square test should be computed. Test
#'   results will be reported via message(). Defaults to FALSE.
#'
#' @return a [tibble][tibble::tibble-package]
#'
#' @examples
#' mtcars %>% crosstab(vs, am)
#' mtcars %>% crosstab(vs, am, add_total = TRUE, percentages = TRUE, chi_square = TRUE)
#'
#' @family descriptives
#'
#' @export
crosstab <- function(data, col_var, ..., add_total = FALSE,
                     percentages = FALSE, chi_square = FALSE) {

  if(dplyr::is_grouped_df(data)) {
    warning("Grouping variable(s) present in data will be ignored.",
            call. = FALSE)
  }

  cross_vars <- length(rlang::quos(...))

  if(cross_vars < 1) {
    stop("Must provide at least one variable to crosstabulate.")
  }

  xt <- data %>%
    dplyr::group_by({{ col_var }}, ...) %>%
    dplyr::count() %>%
    tidyr::spread({{ col_var }}, n) %>%
    dplyr::ungroup() %>%
    dplyr::mutate_if(is.numeric, ~replace(., is.na(.), 0))

  xt_cross_vars <- xt %>%
    dplyr::select(c(1:cross_vars))

  xt_col_vars <- xt %>%
    dplyr::select(-c(1:cross_vars))

  if(chi_square) {
    chi2 <- xt_col_vars %>%
      as.matrix() %>%
      chisq.test()

    test_string <- "Chi-square = %f, df = %f, p = %f"

    message(sprintf(test_string, chi2$statistic, chi2$parameter, chi2$p.value))
  }

  if(add_total) {
    xt_col_vars <- xt_col_vars %>%
      dplyr::mutate(Total = rowSums(.))
  }

  if(percentages) {
    xt_col_vars <- xt_col_vars %>%
      dplyr::mutate_all(~ . / sum(., na.rm = TRUE))
  }

  xt_cross_vars %>%
    dplyr::bind_cols(xt_col_vars)
}
