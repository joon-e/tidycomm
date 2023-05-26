### Internal functions ###

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
