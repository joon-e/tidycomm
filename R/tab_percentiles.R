#' Tabulate percentiles for numeric variables
#'
#' This function tabulates specified percentiles for given numeric variables. If no variables are provided,
#' the function will attempt to describe all numeric (either integer or double) variables found within the input.
#' The percentiles are calculated based on the levels parameter, which defaults to every 10% from 10% to 90%.
#' NA values are always removed because the concept of a percentile is based on
#' ranking. As NA is not a value, it cannot be ordered in relation to actual numbers.
#'
#' @param data a [tibble][tibble::tibble-package] or a [tdcmm] model that contains the numeric data to be tabulated.
#' @param ... Variables within the data for which to tabulate the percentiles. If no variables are provided, all numeric variables are used.
#' @param levels a numeric vector specifying the percentiles to compute. Defaults to c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0).
#'
#' @return a [tdcmm] model
#'
#' @examples
#' WoJ %>% tab_percentiles(work_experience)
#' WoJ %>% tab_percentiles(work_experience, autonomy_emphasis)
#'
#' @family descriptives
#'
#' @export
tab_percentiles <- function(data, ..., levels = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)) {

  # Get current grouping
  grouping <- dplyr::groups(data)

  # Get vars for which to calculate percentiles
  vars <- grab_vars(data, enquos(...))
  vars_str <- purrr::map_chr(vars, as_label)

  # Check if vars is empty and all vars are numeric
  if (length(vars) == 0) {
    stop("No numeric variables found to calculate percentiles for.")
  }

  if (!all(purrr::map_lgl(data %>%
                          dplyr::ungroup() %>%
                          dplyr::select(!!!vars),
                          is.numeric))) {
    stop("... must only contain numeric variables.")
  }

  # Calculate percentiles
  out <- data %>%
    dplyr::select(!!!vars, !!!grouping) %>%
    tidyr::pivot_longer(c(!!!vars), names_to = "Variable", values_to = "Value") %>%
    dplyr::group_by(.data$Variable, .add = TRUE, .drop = TRUE) %>%
    dplyr::summarise(dplyr::across(
      .cols = tidyselect::everything(),
      .fns = purrr::set_names(purrr::map(levels, function(x) purrr::partial(calc_percentiles, percentile = x)), paste0("p", levels * 100)),
      .names = "{.fn}"
    )) %>%
    dplyr::arrange(match(.data$Variable, vars_str))

  values <- out %>%
    dplyr::select(-c(Variable)) %>%
    dplyr::slice(1) %>%
    as.numeric()

  # Output
  return(new_tdcmm_prcntl(new_tdcmm(out,
                                   func = "tab_percentiles",
                                   data = data,
                                   params = list(vars = vars_str,
                                                 levels = levels,
                                                 values = values))))
}

#' @rdname visualize
#' @export
visualize.tdcmm_prcntl <- function(x, ..., .design = design_lmu()) {
  if (attr(x, "func") == "tab_percentiles") {
    return(visualize_tab_percentiles(x, .design))
  }

  return(warn_about_missing_visualization(x))
}




### Internal functions ###

## Calculate percentiles
##
## This function calculates the percentiles for a numeric vector using the specified probability levels.
##
## @param x a numerical vector for which to calculate percentiles.
## @param p a numerical value indicating the percentile level (probability) to compute.
##
## @return a `dbl` containing the computed percentile.
##
## @keywords internal
## @keywords internal
calc_percentiles <- function(var, percentile) {
  # Create a tibble containing the variable
  tibble::tibble(var = var) %>%
    # Compute the percentile
    dplyr::summarise(percentile = quantile(var, probs = percentile, na.rm = TRUE)) %>%
    # Extract the computed percentile value from the tibble
    dplyr::pull(percentile)
}

## Visualize `tab_percentile()` as quantile plot
##
## @param x a [tdcmm] model
##
## @return a [ggplot2] object
##
## @family tdcmm visualize
#
## @keywords internal
visualize_tab_percentiles <- function(x, design = design_lmu()) {

  # Create x and y data for visualization
  x_var <- attr(x, "params")$levels
  y_var <- attr(x, "params")$values
  df <- cbind(x_var, y_var)

  data <- tibble::as_tibble(df)

  # visualize quartiles
  data %>%
    ggplot2::ggplot(ggplot2::aes(x = attr(x, "params")$levels * 100, y = attr(x, "params")$values)) +
    ggplot2::geom_vline(xintercept = 25, linetype="solid", color = design$comparison_color, size = 0.4) +
    ggplot2::geom_vline(xintercept = 50, linetype="solid", color = design$comparison_color, size = 0.4) +
    ggplot2::geom_vline(xintercept = 75, linetype="solid", color = design$comparison_color, size = 0.4) +
    ggplot2::geom_vline(xintercept = 100, linetype="solid", color = design$comparison_color, size = 0.4) +
    ggplot2::geom_point() +
    ggplot2::scale_x_continuous(labels = scales::percent_format(scale = 1),
                                breaks = attr(x, "params")$levels * 100) +
    ggplot2::labs(x = "Percentiles", y = attr(x, "params")$vars[1])  +
    ggplot2::annotate("text", x = 8.5, y = max(y_var), label = "Quartile 1", hjust = 0, vjust = -0.7, size = 1.8) +
    ggplot2::annotate("text", x = 33.5, y = max(y_var), label = "Quartile 2", hjust = 0, vjust = -0.7, size = 1.8) +
    ggplot2::annotate("text", x = 58.5, y = max(y_var), label = "Quartile 3", hjust = 0, vjust = -0.7, size = 1.8) +
    ggplot2::annotate("text", x = 83.5, y = max(y_var), label = "Quartile 4", hjust = 0, vjust = -0.7, size = 1.8) +
    design$theme()
}

# Constructors ----

new_tdcmm_prcntl <- function(x) {
  stopifnot(is_tdcmm(x))

  structure(
    x,
    class = c("tdcmm_prcntl", class(x))
  )
}
