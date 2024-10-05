### Internal functions ###

## Using globalVariables function
# Use the globalVariables function to inform R CMD check that certain names are
# known and shouldn't be flagged.
utils::globalVariables(c(".", "Delta_M", "Group_Var", "M", "N", "SD",
                         "Variable", "Variable_desc", "ci_95_ll", "ci_95_ul",
                         "conf_lower", "conf_upper", "contrast", "d", "d.low",
                         "d.upp", "group1", "group2", "level", "m.diff", "m.low",
                         "m.upp", "name", "pval", "val", "z", "group", "x", "n",
                         "label_independent", "value", "label_independent_desc",
                         "percent", "disagreement", "Coder_Pair", "n_Coders"))


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
#' @param x share between 0 and 1
#'
#' @return a string with formatted % (rounded and suffixed)
#'
#' @family tdcmm visualize
#'
#' @keywords internal
percentage_labeller <-  function(x) {
  return(paste0(round(100*x, 0), "%"))
}
