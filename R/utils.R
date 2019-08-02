#' Keep existing variables or get all numeric variables
#'
#' Keeps existing variables if they were specified in the function call or
#' gets all numeric variables.
#'
#' @param data a [tibble][tibble::tibble-package]
#' @param vars Variables passed to function with `...`, wrapped in `rlang::enquos`
#'
#' @return Variables as symbols
grab_vars <- function(data, vars) {
  if (length(vars) == 0) {
    vars <- data %>%
      dplyr::ungroup() %>%
      dplyr::select_if(is.numeric) %>%
      names() %>%
      rlang::syms()
  }
  return(vars)
}
