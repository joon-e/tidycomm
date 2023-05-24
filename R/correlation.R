#' Compute correlation coefficients
#'
#' Computes correlation coefficients for all combinations of the specified
#' variables. If no variables are specified, all numeric (integer or double)
#' variables are used.
#'
#' @param data a [tibble][tibble::tibble-package] or a [tdcmm] model
#' @param ... Variables to compute correlations for (column names). Leave empty
#'   to compute for all numeric variables in data.
#' @param method a character string indicating which correlation coefficient
#'   is to be computed. One of "pearson" (default), "kendall", or "spearman"
#'
#' @return a [tdcmm] model
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
  out <- purrr::map_dfr(var_combs, correlation_test, data, method)

  return(new_tdcmm_crrltn(new_tdcmm(out,
                                    func = "correlate",
                                    data = data,
                                    params = list(vars = var_strings,
                                                  method = method))))
}

#' Create correlation matrix
#'
#' Turns the tibble exported from \code{\link{correlate}} into a correlation
#' matrix.
#'
#' @param data a [tdcmm] model returned from \code{\link{correlate}}
#'
#' @return a [tdcmm] model
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

  out <- data %>%
    dplyr::select(x = 1, y = 2, cor = 3) %>%
    dplyr::bind_rows(
      data %>%
        dplyr::select(x = 1, y = 2, cor = 3) %>%
        dplyr::rename(x = "y", y = "x")
    ) %>%
    tidyr::spread(.data$y, .data$cor, fill = 1) %>%
    dplyr::arrange(match(.data$x, var_order)) %>%
    dplyr::rename(!!estimate := "x") %>%
    dplyr::select(tidyselect::all_of(estimate), tidyselect::all_of(var_order),
                  dplyr::everything())

  return(new_tdcmm_crrltn(
    new_tdcmm(out,
              data = attr(data, "data"),
              func = "to_correlation_matrix",
              params = attr(data, "params"),
              model = list(data)))
  )
}

#' @export
visualize.tdcmm_crrltn <- function(x, ...) {
  if (attr(x, "func") == "correlate") {
    return(visualize_correlate(x))
  }

  if (attr(x, "func") == "to_correlation_matrix") {
    return(visualize_to_correlation_matrix(x))
  }

  return(warn_about_missing_visualization(x))
}

### Internal functions ###

## Compute correlation test
##
## Computes a correlation test for a two variables
##
## @param var_comb A character vector containing the name of two variables
## @param data a [tibble][tibble::tibble-package]
## @param method a character string indicating which correlation coefficient
##   is to be computed. One of "pearson" (default), "kendall", or "spearman"
##
## @return a [tibble][tibble::tibble-package]
##
## @family correlations
##
## @keywords internal
correlation_test <- function(var_comb, data, method) {
  x <- var_comb[[1]]
  y <- var_comb[[2]]
  xvar <- data[[x]]
  yvar <- data[[y]]

  if (any(!is.numeric(xvar), !is.numeric(yvar))) {
    warning(glue("At least one of {x} and {y} is not numeric, ",
                 "skipping computation."),
            call. = FALSE)
    return()
  }

  cor_test <- stats::cor.test(xvar, yvar, method = method)

  if (method == "pearson") {
    name <- "r"
  } else if (method == "kendall") {
    name <- "tau"
  } else if (method == "spearman") {
    name <- "rho"
  }

  tibble::tibble(
    x = x,
    y = y,
    !!name := cor_test$estimate,
    df = ifelse(is.null(cor_test$parameter),
                NA, cor_test$parameter),
    p = cor_test$p.value
  )
}

## Visualize `correlate()` as scatter plot. For more than 2 variables, a
## [GGally::ggpairs] correlogram is plotted (just like when visualizing
## `to_correlation_matrix()`). Visualizations are plotted with a bit of
## "jitter" (random noise) to better reflect categorical values.
##
## @param x a [tdcmm] model
##
## @return a [ggplot2] object
##
## @family tdcmm visualize
#
## @keywords internal
visualize_correlate <- function(x) {
  if (nrow(x) > 1) {
    return(visualize(to_correlation_matrix(x)))
  }

  attr(x, "data") %>%
    ggplot2::ggplot(ggplot2::aes(x = !!sym(attr(x, "params")$vars[1]),
                                 y = !!sym(attr(x, "params")$vars[2]))) +
    ggplot2::geom_jitter(width = .3,
                         height = .3) +
    ggplot2::scale_x_continuous(attr(x, "params")$vars[1],
                                n.breaks = 8) +
    ggplot2::scale_y_continuous(attr(x, "params")$vars[2],
                                n.breaks = 8) +
    tdcmm_visual_defaults()$theme()
}

## Visualize `to_correlation_matrix()` as [GGally::ggpairs] correlogram.
##
## @param x a [tdcmm] model
##
## @return a [ggplot2] object
##
## @family tdcmm visualize
#
## @keywords internal
visualize_to_correlation_matrix <- function(x) {
  ggpairs_corrstats <- GGally::wrap(ggpairs_corrstats_helper,
                                    method = attr(x, "params")$method)
  attr(x, "data") %>%
    dplyr::select(!!!syms(attr(x, "params")$vars)) %>%
    GGally::ggpairs(cardinality_threshold = 12,
                    axisLabels = "none",
                    upper = list(continuous = ggpairs_corrstats,
                                 discrete = ggpairs_corrstats,
                                 combo = ggpairs_corrstats,
                                 na = "na"),
                    diag = list(continuous = "barDiag",
                                discrete = "barDiag",
                                na = "naDiag"),
                    lower = list(continuous = "dot_no_facet",
                                 discrete = "dot_no_facet",
                                 combo = "dot_no_facet",
                                 na = "na")) +
    tdcmm_visual_defaults()$theme()
}

## Helper function to print correlation coefficients together with CIs.
## Some inspiration taken from [GGally::ggally_cor] (CRAN version 2.1.2).
##
## @family tdcmm visualize
#
## @keywords internal
ggpairs_corrstats_helper <- function(data, mapping, ...,
                                     method = "pearson") {

  GGally::ggally_statistic(data = data,
                           mapping = mapping,
                           justify_text = "left",
                           title = "",
                           sep = "",
                           text_fn = function(x, y) {
                             cor_test <- stats::cor.test(x, y, method = method)
                             if (method == "pearson") {
                               glue("r = ",
                                    format_value(cor_test$estimate["cor"], 3),
                                    "\n",
                                    format_pvalue(cor_test$p.value),
                                    "\n",
                                    "95% CI [",
                                    format_value(cor_test$conf.int[1], 3),
                                    ", ",
                                    format_value(cor_test$conf.int[2], 3),
                                    "]")
                             } else if (method == "kendall") {
                               glue("tau = ",
                                    format_value(cor_test$estimate["tau"], 3),
                                    "\n",
                                    format_pvalue(cor_test$p.value))
                             } else if (method == "spearman") {
                               glue("rho = ",
                                    format_value(cor_test$estimate["rho"], 3),
                                    "\n",
                                    format_pvalue(cor_test$p.value))
                             }
                           })
}

# Constructors ----

new_tdcmm_crrltn <- function(x) {
  stopifnot(is_tdcmm(x))

  structure(
    x,
    class = c("tdcmm_crrltn", class(x))
  )
}
