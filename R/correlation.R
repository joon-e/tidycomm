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
#' @param partial Either a logical value or a character string. When set
#' to `TRUE`, the function computes the pairwise partial correlation for three
#' variables. If set to a character, it represents the variable to control for.
#' Defaults to `FALSE`.
#' @param with A character string specifying a focus variable to correlate all
#' other variables with. If used, `partial` must be set to `FALSE` (default).
#'
#' @return a [tdcmm] model
#'
#' @family correlations
#'
#' @examples
#' WoJ %>% correlate(ethics_1, ethics_2, ethics_3)
#' WoJ %>% correlate()
#' WoJ %>% correlate(autonomy_selection, autonomy_emphasis, work_experience, partial = TRUE)
#' WoJ %>% correlate(autonomy_selection, autonomy_emphasis, partial = "work_experience")
#'
#' @export
correlate <- function(data, ..., method = "pearson",
                      partial = FALSE, with = NULL) {

  input_vars_chr <- tidyselect::vars_select(names(data), ...)

  if (is.character(partial)) {
    zvar <- partial
    partial <- TRUE
  } else {
    zvar <- NULL
  }

  if (partial == TRUE & !is.null(with)) {
    stop("Cannot run a partial correlation and specify a focus variable simultaneously. Please choose one approach.",
         call. = FALSE)
  }

  if (partial == TRUE) {
    var_strings <- data %>%
      dplyr::select(tidyselect::all_of(input_vars_chr)) %>%
      names()

    method_string <- method
    result_correlate_partial <- correlate_partial(data, zvar, !!!rlang::syms(input_vars_chr), method = method_string)
    return(new_tdcmm_crrltn(new_tdcmm(result_correlate_partial,
                                      func = "correlate",
                                      data = data,
                                      params = list(vars = var_strings,
                                                    partial = TRUE,
                                                    method = method))))
  }

  if (!method %in% c("pearson", "kendall", "spearman")) {
    stop('Method must be one of "pearson", "kendall" or "spearman".',
         call. = FALSE)
  }

  vars <- grab_vars(data, enquos(...))

  var_strings <- data %>%
    dplyr::select(!!!vars) %>%
    names()

  if (!is.null(with)) {
    if (!(with %in% names(data))) {
      stop("Specified comparison variable not found in the data.", call. = FALSE)
    }
    var_strings <- unique(c(with, var_strings))
    var_combs <- purrr::map(setdiff(var_strings, with), ~c(with, .x))
  } else {
    var_combs <- combn(var_strings, 2, simplify = FALSE)
  }

  out <- purrr::map_dfr(var_combs, correlation_test, data, method)

  return(new_tdcmm_crrltn(new_tdcmm(out,
                                    func = "correlate",
                                    data = data,
                                    params = list(vars = var_strings,
                                                  partial = FALSE,
                                                  with = NULL,
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

  # Compute n if exactly two vars are being correlated
  if (nrow(data) == 1) {

    out <- data %>%
      dplyr::select(x = 1, y = 2, cor = 3, n = 4) %>%
      dplyr::bind_rows(
        data %>%
          dplyr::select(x = 1, y = 2, cor = 3, n = 4) %>%
          dplyr::rename(x = "y", y = "x")
      ) %>%
      tidyr::spread(.data$y, .data$cor, fill = 1) %>%
      dplyr::arrange(match(.data$x, var_order)) %>%
      dplyr::rename(!!estimate := "x") %>%
      dplyr::select(tidyselect::all_of(estimate), tidyselect::all_of(var_order),
                    dplyr::everything())

    if (!is.na(out$n[1])) {
      n_value <- out$n[1] + 2

      cat("\n", "Sample size after deletion of cases with missing values: n = ", n_value, "\n")
    }

    out <- out %>%
      dplyr::select(-.data$n)

  } else {
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

    check_for_with_parameter <- out[2,4]

    if (check_for_with_parameter == 1) {
      out <- out %>%
        dplyr::select(1:2) # Fixed the select function by adding the dplyr:: prefix
    }
  }

  return(new_tdcmm_crrltn(
    new_tdcmm(out,
              data = attr(data, "data"),
              func = "to_correlation_matrix",
              params = attr(data, "params"),
              model = list(data)))
  )
}


#' @rdname visualize
#' @param which string to specify type of point visualization. One of
#' "jitter" (default, random noise to better reflect categorical values ) or
#' "alpha" (points appear slightly transparent so that multiple points in the
#' same position are more easily visible); only affects regular correlation.
#'
#' @export
visualize.tdcmm_crrltn <- function(x,
                                   which = "jitter",
                                   ...,
                                   .design = design_lmu()) {
  if (attr(x, "func") == "correlate") {
    if (!which %in% c("jitter", "alpha")) {
      warning(glue::glue('which must be one of "jitter" or "alpha". Since none ',
                   'was provided, "jitter" is considered by default.'),
              call. = FALSE)
      which <- "jitter"
    }
    if (attr(x, "params")$partial == TRUE) {
      return(visualize_partial_correlation(x, which, .design))
    } else {
      return(visualize_correlate(x, which, .design))
    }
  }

  if (attr(x, "func") == "to_correlation_matrix") {
    return(visualize_to_correlation_matrix(x, .design))
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

  n_value <- nrow(data) - sum(is.na(data[[x]]) | is.na(data[[y]]))

  if (any(!is.numeric(xvar), !is.numeric(yvar))) {
    warning(glue::glue("At least one of {x} and {y} is not numeric, ",
                 "skipping computation."),
            call. = FALSE)
    return()
  }

  suppressWarnings({
    cor_test <- stats::cor.test(xvar, yvar, method = method)
  })

  if (method == "pearson") {
    name <- "r"
  } else if (method == "kendall") {
    name <- "tau"
    if (is.null(cor_test$parameter)) {
      message("When using Kendall's tau correlation, the df is not applicable. Kendall's tau is based on concordant and discordant pairs of data, rather than on a mathematical distribution that would require the calculation of df.")
      df <- NA
    } else {
      df <- cor_test$parameter
    }
  } else if (method == "spearman") {
    name <- "rho"
    if (is.null(cor_test$parameter)) {
      message("The Spearman correlation may involve tied values (they have the same rank), making it impossible to calculate an exact p-value and dfs. We suggest using Kendall's tau rank correlation, which is tailored to handle tied data.")
      df <- NA
    } else {
      df <- cor_test$parameter
    }
  }


  tibble::tibble(
    x = x,
    y = y,
    !!name := cor_test$estimate,
    df = ifelse(is.null(cor_test$parameter),
                NA, cor_test$parameter),
    p = cor_test$p.value,
    n = n_value
  )
}

## Visualize `correlate()` as scatter plot. For more than 2 variables, a
## [GGally::ggpairs] correlogram is plotted (just like when visualizing
## `to_correlation_matrix()`). Visualizations are plotted with a bit of
## "jitter" (random noise) to better reflect categorical values.
##
## @param x a [tdcmm] model
## @param which string to specify type of point visualization. One of
## "jitter" (default, random noise to better reflect categorical values ) or
## "alpha" (points appear slightly transparent so that multiple points in the
## same position are more easily visible); only affects regular correlation.
##
## @return a [ggplot2] object
##
## @family tdcmm visualize
#
## @keywords internal
visualize_correlate <- function(x, which = "jitter", design = design_lmu()) {
  if (nrow(x) > 1) {
    return(visualize(to_correlation_matrix(x), design))
  }

  g <- attr(x, "data") %>%
    ggplot2::ggplot(ggplot2::aes(x = !!sym(attr(x, "params")$vars[1]),
                                 y = !!sym(attr(x, "params")$vars[2])))

  if (which == "jitter") {
    g <- g +
      ggplot2::geom_jitter(width = .3,
                           height = .3,
                           na.rm = TRUE)
  } else {
    g <- g +
      ggplot2::geom_point(alpha = .25,
                          na.rm = TRUE)
  }

  g +
    ggplot2::scale_x_continuous(attr(x, "params")$vars[1],
                                n.breaks = 8) +
    ggplot2::scale_y_continuous(attr(x, "params")$vars[2],
                                n.breaks = 8) +
    design$theme()
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
visualize_to_correlation_matrix <- function(x, design = design_lmu()) {
  attr(x, "data") %>%
    dplyr::select(!!!syms(attr(x, "params")$vars)) %>%
    GGally::ggpairs(cardinality_threshold = 12,
                    axisLabels = "none",
                    progress = FALSE,
                    upper = list(continuous = GGally::wrap(ggpairs_corrstats_helper,
                                                           method = attr(x, "params")$method),
                                 discrete = GGally::wrap(ggpairs_corrstats_helper,
                                                         method = attr(x, "params")$method),
                                 combo = GGally::wrap(ggpairs_corrstats_helper,
                                                      method = attr(x, "params")$method),
                                 na = "na"),
                    diag = list(continuous = GGally::wrap("barDiag",
                                                          fill = design$main_color_1,
                                                          bins = 30,
                                                          na.rm = TRUE),
                                discrete = GGally::wrap("barDiag",
                                                        fill = design$main_color_1,
                                                        bins = 30,
                                                        na.rm = TRUE),
                                na = "naDiag"),
                    lower = list(continuous = GGally::wrap("dot_no_facet",
                                                           na.rm = TRUE),
                                 discrete = GGally::wrap("dot_no_facet",
                                                         na.rm = TRUE),
                                 combo = GGally::wrap("dot_no_facet",
                                                      na.rm = TRUE),
                                 na = "na")) +
    design$theme()
}

## Visualize `correlate(..., partial = TRUE)` as correlation between residuals
##
## @param x a [tdcmm] model
## @param which string to specify type of point visualization. One of
## "jitter" (default, random noise to better reflect categorical values ) or
## "alpha" (points appear slightly transparent so that multiple points in the
## same position are more easily visible); only affects regular correlation.
##
## @return a [ggplot2] object
##
## @family tdcmm visualize
#
## @keywords internal
visualize_partial_correlation <- function(x, which = "jitter", design = design_lmu()) {
  # prepare data
  data <- attr(x, "data") %>%
    stats::na.omit()

  # calculate individual models
  model1 <- data %>%
    regress(!!sym(attr(x, "params")$vars[1]),
            !!sym(attr(x, "params")$vars[3])) %>%
    model()

  model2 <- data %>%
    regress(!!sym(attr(x, "params")$vars[2]),
            !!sym(attr(x, "params")$vars[3])) %>%
    model()

  # extracts residuals
  model1_res <- stats::residuals(model1)
  model2_res <- stats::residuals(model2)

  # cbind and visualize
  model1_name <- paste0('Residuals ',
                        attr(x, "params")$vars[1], ' ~ ',
                        attr(x, "params")$vars[3])
  model2_name <- paste0('Residuals ',
                        attr(x, "params")$vars[2], ' ~ ',
                        attr(x, "params")$vars[3])
  data %>%
    dplyr::bind_cols(tibble::tibble(!!model1_name := model1_res,
                                    !!model2_name := model2_res)) %>%
    correlate(!!model1_name,
              !!model2_name) %>%
    visualize(which, design)
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
                           na.rm = TRUE,
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
