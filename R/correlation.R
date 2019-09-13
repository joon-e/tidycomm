#' Compute correlation test
#'
#' Computes a correlation test for a two variables
#'
#' @param var_comb A character vector containing the name of two variables
#' @param data a [tibble][tibble::tibble-package]
#' @param method a character string indicating which correlation coefficient
#'   is to be computed. One of "pearson" (default), "kendall", or "spearman"
#'
#' @return a [tibble][tibble::tibble-package]
#'
#' @family correlations
correlation_test <- function(var_comb, data, method) {
  x <- var_comb[[1]]
  y <- var_comb[[2]]
  xvar <- data[[x]]
  yvar <- data[[y]]

  if (any(!is.numeric(xvar), !is.numeric(yvar))) {
    warning(glue("At least one of {x} and {y} is not numeric, skipping computation."),
            call. = FALSE)
    return()
  }

  cor_test <- cor.test(xvar, yvar, method = method)

  if (method == "pearson") {
    name <- "r"
  } else if (method == "kendall") {
    name <- "tau"
  } else if (method == "spearman") {
    name <- "rho"
  }

  tibble(
    x = x,
    y = y,
    !!name := cor_test$estimate,
    df = ifelse(is.null(cor_test$parameter),
                NA, cor_test$parameter),
    p = cor_test$p.value
  )
}

#' Compute correlation coefficients
#'
#' Computes correlation coefficients for all combinations of the specified
#' variables. If no variables are specified, all numeric (integer or double)
#' variables are used.
#'
#' @param data a [tibble][tibble::tibble-package]
#' @param ... Variables to compute correlations for (column names). Leave empty
#'   to compute for all numeric variables in data.
#' @param method a character string indicating which correlation coefficient
#'   is to be computed. One of "pearson" (default), "kendall", or "spearman"
#'
#' @return a [tibble][tibble::tibble-package]
#'
#' @family correlations
#'
#' @examples
#' WoJ %>% correlate(ethics_1, ethics_2, ethics_3)
#' WoJ %>% correlate()
#'
#' @export
correlate <- function(data, ..., method = "pearson") {

  if (!method %in% c("pearson", "kendall", "spearman")) {
    stop('method must be one of "pearson", "kendall" or "spearman"',
         call. = FALSE)
  }

  vars <- grab_vars(data, enquos(...))

  var_strings <- data %>%
    dplyr::select(!!!vars) %>%
    names()
  var_combs <- combn(var_strings, 2, simplify = FALSE)
  purrr::map_dfr(var_combs, correlation_test, data, method)
}

#' Create correlation matrix
#'
#' Turns the tibble exported from \code{\link{correlate}} into a correlation
#' matrix.
#'
#' @param data a [tibble][tibble::tibble-package] returned from \code{\link{correlate}}
#'
#' @return a [tibble][tibble::tibble-package]
#'
#' @family correlation
#'
#' @examples
#' WoJ %>% correlate() %>% to_correlation_matrix()
#'
#' @export
to_correlation_matrix <- function(data) {

  estimate <- names(data)[3]

  var_order <- data %>%
    dplyr::pull(.data$x) %>%
    unique()

  data %>%
    dplyr::select(x = 1, y = 2, cor = 3) %>%
    dplyr::bind_rows(
      data %>%
        dplyr::select(x = 1, y = 2, cor = 3) %>%
        dplyr::rename(x = .data$y, y = .data$x)
    ) %>%
    tidyr::spread(.data$y, .data$cor, fill = 1) %>%
    dplyr::arrange(match(.data$x, var_order)) %>%
    dplyr::rename(!!estimate := .data$x) %>%
    dplyr::select(estimate, var_order, dplyr::everything())
}
