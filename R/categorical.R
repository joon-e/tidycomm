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
#' WoJ %>% tab_frequencies(employment)
#' WoJ %>% tab_frequencies(employment, country)
#'
#' @family categorical
#'
#' @export
tab_frequencies <- function(data, ...) {
  grouping <- dplyr::groups(data)

  d <- data %>%
    dplyr::group_by(..., !!!grouping) %>%
    dplyr::summarise(n = dplyr::n()) %>%
    dplyr::group_by(!!!grouping) %>%
    dplyr::mutate(percent = .data$n / sum(.data$n)) %>%
    dplyr::arrange(!!!grouping)

  d %>%
    dplyr::bind_cols(d %>%
                dplyr::select(!!!grouping,
                       cum_n = "n",
                       cum_percent = "percent") %>%
                dplyr::mutate_at(dplyr::vars(-dplyr::group_cols()), cumsum) %>%
                dplyr::ungroup() %>%
                dplyr::select("cum_n", "cum_percent")
    )

}

#' Crosstab variables
#'
#' Computes contingency table for one independent (column) variable and one or
#' more dependent (row) variables.
#'
#' @param data a [tibble][tibble::tibble-package]
#' @param col_var Independent (column) variable.
#' @param ... Dependent (row) variables.
#' @param add_total Logical indicating whether a 'Total' column should be
#'   computed. Defaults to `FALSE`.
#' @param percentages Logical indicating whether to output column-wise
#'   percentages instead of absolute values. Defaults to `FALSE`.
#' @param chi_square Logical indicating whether a Chi-square test should be computed.
#'   Test results will be reported via message(). Defaults to `FALSE`.
#'
#' @return a [tibble][tibble::tibble-package]
#'
#' @examples
#' WoJ %>% crosstab(reach, employment)
#' WoJ %>% crosstab(reach, employment, add_total = TRUE, percentages = TRUE, chi_square = TRUE)
#'
#' @family categorical
#'
#' @export
crosstab <- function(data, col_var, ..., add_total = FALSE,
                     percentages = FALSE, chi_square = FALSE) {

  # Checks
  if (dplyr::is_grouped_df(data)) {
    warning("Grouping variable(s) present in data will be ignored.",
            call. = FALSE)
  }

  cross_vars <- length(quos(...))

  if (cross_vars < 1) {
    stop("Must provide at least one variable to crosstabulate.")
  }

  # Prepare crosstab
  xt <- data %>%
    dplyr::group_by({{ col_var }}, ...) %>%
    dplyr::count() %>%
    tidyr::spread({{ col_var }}, .data$n, fill = 0) %>%
    dplyr::ungroup()

  xt_cross_vars <- xt %>%
    dplyr::select(1:tidyselect::all_of(cross_vars))

  xt_col_vars <- xt %>%
    dplyr::select(-(1:tidyselect::all_of(cross_vars)))

  # Estimate Chi-square test
  if (chi_square) {
    chi2 <- xt_col_vars %>%
      as.matrix() %>%
      chisq.test()
  }

  # Augment
  if (add_total) {
    xt_col_vars <- xt_col_vars %>%
      dplyr::mutate(Total = rowSums(xt_col_vars))
  }

  if (percentages) {
    xt_col_vars <- xt_col_vars %>%
      dplyr::mutate_all(col_percs)
  }

  # Output
  out <- xt_cross_vars %>%
    dplyr::bind_cols(xt_col_vars)

  if (chi_square) {
    return(new_tdcmm_chi2(
      new_tdcmm(out, model = list(chi2)))
      )
  } else {
    return(new_tdcmm(out))
  }
}

# Internal functions ----

## Compute Cramer's V
##
## Computes Cramer's V
##
## @param chi2 Output from a `chisq.test()`.
##
## @return a `dbl`
##
## @family categorical
##
## @keywords internal
cramer_V <- function(chi2) {

  X2 <- chi2$statistic
  N <- sum(chi2$observed)
  k = min(dim(chi2$observed))

  unname(sqrt(X2 / (N * (k - 1))))
}

## Compute column percentages
##
## Computes column percentages
##
## @param x Numeric vector
##
## @return a `dbl`
col_percs <- function(x) {
  x / sum(x, na.rm = TRUE)
}


# Constructors ----

new_tdcmm_chi2 <- function(x) {
  stopifnot(is_tdcmm(x))

  structure(
    x,
    class = c("tdcmm_chi2", class(x))
  )
}

# Formatting ----

#' @export
tbl_format_footer.tdcmm_chi2 <- function(x, ...) {
  default_footer <- NextMethod()

  # Get values
  chi2 <- model(x)

  # Format test string
  test_string <- glue("Chi-square = {format_testvalue(chi2$statistic)}, ",
                      "df = {format(chi2$parameter, digits = 4)}, ",
                      "{format_pvalue(chi2$p.value)}, ",
                      "V = {format_testvalue(cramer_V(chi2))}")

  # Add to footer and display
  test_footer <- style_subtle(glue("# {test_string}"))

  c(default_footer, test_footer)
}
