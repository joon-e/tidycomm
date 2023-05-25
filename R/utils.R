### Internal functions ###

## tidycomm defaults for visualizations
##
## @return a list
##
## @family tdcmm visualize
##
## @keywords internal
tdcmm_visual_defaults <- function() {
  return(list(main_color_1 = "#00883A",
              main_colors = c("#00883A",
                              "#a6cee3",
                              "#1f78b4",
                              "#b2df8a",
                              #"#33a02c", #replaced by main_color_1
                              "#fb9a99",
                              "#e31a1c",
                              "#fdbf6f",
                              "#ff7f00",
                              "#cab2d6",
                              "#6a3d9a",
                              "#ffff99",
                              "#b15928"),
              main_contrast_1 = "#FFFFFF",
              main_contrasts = c("#FFFFFF",
                                 "#525252",
                                 "#FFFFFF",
                                 "#525252",
                                 "#525252",
                                 "#525252",
                                 "#525252",
                                 "#525252",
                                 "#525252",
                                 "#FFFFFF",
                                 "#525252",
                                 "#FFFFFF"),
              main_size = 0.95,
              comparison_linetype = "dashed",
              comparison_color = "#525252",
              comparison_size = 0.85,
              theme = function(...) {
                return(ggplot2::theme_minimal() +
                         ggplot2::theme(
                           axis.line = ggplot2::element_line(color = "#8f8f8f"),
                           axis.ticks = ggplot2::element_line(color = "#6b6b6b"),
                           axis.title = ggplot2::element_text(),
                           axis.title.x = ggplot2::element_text(vjust = -.5),
                           axis.title.y = ggplot2::element_text(vjust = 2.5),
                           strip.text = ggplot2::element_text(face = "bold"),
                           ...
                         ))
              }))
}


## Helper function for indicating a lack of visualization
##
## @param a [tdcmm] model
##
## @return NULL
##
## @family tdcmm visualize
##
## @keywords internal
warn_about_missing_visualization <- function(x) {
  warning(glue("No visualization implemented for this model."),
          call. = FALSE)
  return(NULL)
}


## Keep existing variables or get all numeric variables
##
## Keeps existing variables if they were specified in the function call or
## gets an alternative selection of variables if none were specified.
##
## @param data a [tibble][tibble::tibble-package]
## @param vars Variables passed to function with `...`, wrapped in `rlang::enquos`
## @param alternative Which variables to grab alternatively if no variables were
##   specified in the function call. Defaults to "numeric".
##
## @return Variables as symbols
##
## @keywords internal
grab_vars <- function(data, vars, alternative = "numeric",
                      exclude_vars = NULL) {
  if (length(vars) == 0) {
    if (alternative == "numeric") {
      vars <- data %>%
        dplyr::ungroup() %>%
        dplyr::select_if(is.numeric) %>%
        names() %>%
        syms()
    }

    if (alternative == "categorical") {

      vars <- data %>%
        dplyr::ungroup() %>%
        dplyr::select(-dplyr::group_vars(data)) %>%
        dplyr::select_if(function(col) is.factor(col) | is.character(col)) %>%
        names() %>%
        syms()
    }

    if (alternative == "all") {
      vars <- data %>%
        dplyr::ungroup() %>%
        dplyr::select(-tidyselect::all_of(exclude_vars)) %>%
        names() %>%
        syms()
    }

    if (alternative == "none") {
      return(vars)
    }
  } else {
    vars <- data %>%
      dplyr::ungroup() %>%
      dplyr::select(!!!vars, -tidyselect::all_of(exclude_vars)) %>%
      names() %>%
      syms()
  }
  return(vars)
}

## Helper function to calculate lower level of confidence interval
##
## @param m mean
## @param sd standard deviation
## @param n number of cases
## @param level level to calculate CI for (default is 95%)
##
## @return numeric
##
## @keywords internal
calculate_ci_ll <- function(m, sd, n, level = .95) {
  return(m - stats::qt(( 1 - (1-level)/2 ), df = n-1) * sd/sqrt(n))
}

## Helper function to calculate upper level of confidence interval
##
## @param m mean
## @param sd standard deviation
## @param n number of cases
## @param level level to calculate CI for (default is 95%)
##
## @return numeric
##
## @keywords internal
calculate_ci_ul <- function(m, sd, n, level = .95) {
  return(m + stats::qt(( 1 - (1-level)/2 ), df = n-1) * sd/sqrt(n))
}

# Formatters ----

## Format a vector of values to printable string p-values
format_pvalue <- function(x) {
  single_val <- function(x) {
    if (x < .001) {
      return("p < 0.001")
      } else {
      return(glue("p = {format(round(x, 3), nsmall = 3)}"))
      }
  }
  purrr::map_chr(x, single_val)
}

## Format a vector of values to printable string values with exact number of
## decimal places
format_value <- function(x, d) trimws(format(round(x, d), nsmall = d))

#' Helper function for labelling purposes
#'
#' @param numeric share between 0 and 1
#'
#' @return a string with formatted % (rounded and suffixed)
#'
#' @family tdcmm visualize
#'
#' @keywords internal
percentage_labeller <-  function(x) {
  return(paste0(round(100*x, 0), "%"))
}
