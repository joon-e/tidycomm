#' Describe numeric variables
#'
#' Describe numeric variables by several measures of
#' central tendency and variability. If no variables are specified,
#' all numeric (integer or double) variables are described.
#'
#' @param data a [tibble][tibble::tibble-package]
#' @param ... Variables to describe (column names). Leave empty to describe all
#'   numeric variables in data.
#' @param na.rm a logical value indicating whether `NA` values should be stripped
#'  before the computation proceeds. Defaults to `TRUE`.
#'
#' @return a [tibble][tibble::tibble-package]
#'
#' @examples
#' iris %>% describe()
#' mtcars %>% describe(mpg, am, cyl)
#'
#' @family descriptives
#'
#' @export
describe <- function(data, ..., na.rm = TRUE) {

  # Get current grouping
  grouping <- dplyr::groups(data)

  # Get describe vars
  vars <- grab_vars(data, enquos(...))
  vars_str <- purrr::map_chr(vars, as_label)

  # Check if vars is empty and all vars are numeric
  if (length(vars) == 0) {
    stop("No numeric variables found to descibre")
  }

  if (!all(purrr::map_lgl(data %>%
                          dplyr::ungroup() %>%
                          dplyr::select(!!!vars),
                          is.numeric))) {
    stop("... must only contain numeric variables.")
  }

  # Describe
  data %>%
    dplyr::select(!!!vars, !!!grouping) %>%
    tidyr::pivot_longer(c(!!!vars), names_to = "Variable", values_to = "Value") %>%
    dplyr::group_by(.data$Variable, .add = TRUE, .drop = TRUE) %>%
    dplyr::summarise(
      N = dplyr::n() - sum(is.na(.data$Value)),
      Missing = sum(is.na(.data$Value)),
      M = mean(.data$Value, na.rm = na.rm),
      SD = sd(.data$Value, na.rm = na.rm),
      Min = min(.data$Value, na.rm = na.rm),
      Q25 = quantile(.data$Value, .25, na.rm = na.rm),
      Mdn = median(.data$Value, na.rm = na.rm),
      Q75 = quantile(.data$Value, .75, na.rm = na.rm),
      Max = max(.data$Value, na.rm = na.rm),
      Range = .data$Max - .data$Min,
      CI_95_LL = .data$M - stats::qt(0.975, df = .data$N-1) * .data$SD/sqrt(.data$N),
      CI_95_UL = .data$M + stats::qt(0.975, df = .data$N-1) * .data$SD/sqrt(.data$N),
      Skewness = skewness(.data$Value),
      Kurtosis = kurtosis(.data$Value)
    ) %>%
    dplyr::arrange(match(.data$Variable, vars_str))

}

#' Describe categorical variables
#'
#' Describe categorical variables by N, number of unique values, and mode.
#' Note that in case of multiple modes, the first mode by order of values
#' is chosen.
#'
#' If no variables are specified, all categorical (character or factor)
#' variables are described.
#'
#' @param data a [tibble][tibble::tibble-package]
#' @param ... Variables to describe (column names). Leave empty to describe all
#'   categorical variables in data.
#'
#' @return a [tibble][tibble::tibble-package]
#'
#' @examples
#' iris %>% describe_cat()
#'
#' @family descriptives
#'
#' @export
describe_cat <- function(data, ...) {

  # Get current grouping
  grouping <- dplyr::groups(data)

  # Get describe vars
  vars <- grab_vars(data, enquos(...), alternative = "categorical")
  vars_str <- purrr::map_chr(vars, as_label)

  # Check if vars is empty and all vars are numeric
  if (length(vars) == 0) {
    stop("No categorical variables found to descibre")
  }

  # Describe
  data %>%
    dplyr::select(!!!vars, !!!grouping) %>%
    tidyr::pivot_longer(c(!!!vars), names_to = "Variable", values_to = "Value") %>%
    dplyr::group_by(.data$Variable, .add = TRUE, .drop = TRUE) %>%
    dplyr::summarise(
      N = dplyr::n() - sum(is.na(.data$Value)),
      Missing = sum(is.na(.data$Value)),
      Unique = length(unique(.data$Value)),
      Mode = as.character(get_mode(.data$Value)),
      Mode_N = sum(.data$Value == .data$Mode, na.rm = TRUE)
    ) %>%
    dplyr::arrange(match(.data$Variable, vars_str))
}

### Internal functions ###

## Skewness
##
## Compute empirical skewness
##
## @param x a numerical vector
##
## @return a `dbl`
##
## @keywords internal
skewness <- function(x) {
  x <- x[!is.na(x)]
  m <- mean(x, na.rm = TRUE)
  sum((x - m)^3) / length(x) / ((sum((x - m)^2) / length(x))^(3/2))
}

## Kurtosis
##
## Compute empirical kurtosis
##
## @param x a numerical vector
##
## @return a `dbl`
##
## @keywords internal
kurtosis <- function(x) {
  x <- x[!is.na(x)]
  m <- mean(x, na.rm = TRUE)
  sum((x - m)^4) / (sum((x - m)^2)^2) * length(x)
}
