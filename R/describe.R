#' Describe variables
#'
#' Describe variables by several measures of
#' central tendency and variability. If no variables are specified,
#' all numeric (integer or double) variables are described.
#'
#' @param data A (tidy) dataset.
#' @param ... Variables to describe (column names)
#' @param na.rm a logical value indicating whether NA values should be stripped
#'     before the computation proceeds. Defaults to TRUE.
#'
#' @return a [tibble][tibble::tibble-package]
#'
#' @examples
#' describe(mtcars, disp, hp, mpg)
#' describe(iris)
#'
#' @export
describe <- function(data, ..., na.rm = TRUE) {
  vars <- rlang::quos(...)

  if (length(vars) == 0) {
    vars <- data %>%
      dplyr::select_if(~ is.numeric(.)) %>%
      names() %>%
      rlang::syms()
  }

  data %>%
    dplyr::select(!!! vars) %>%
    tidyr::gather(Variable, Value, !!! vars) %>%
    dplyr::group_by(Variable) %>%
    dplyr::summarise(
      N = dplyr::n(),
      Missing = sum(is.na(Value)),
      M = mean(Value, na.rm = na.rm),
      SD = sd(Value, na.rm = na.rm),
      Min = min(Value, na.rm = na.rm),
      Max = max(Value, na.rm = na.rm),
      Range = Max - Min,
      Mdn = median(Value, na.rm = na.rm),
      Q25 = quantile(Value, .25, na.rm = na.rm),
      Q75 = quantile(Value, .75, na.rm = na.rm)
    )

}

#' Describe groups by variable
#'
#' Describe one or more groups by descriptive statistics for one continous
#' variable.
#'
#' @param data A (tidy) dataset
#' @param var Variable to describe by (column name)
#' @param ... Variable(s) to group by (column name)
#' @param na.rm a logical value indicating whether NA values should be stripped
#' before the computation proceeds. Defaults to TRUE.
#'
#' @return a [tibble][tibble::tibble-package]
#'
#' @examples describe(mtcars, mpg, cyl, am)
#'
#' @export
describe_groups <- function(data, var, ..., na.rm = TRUE) {

  data %>%
    dplyr::select({{ var }}, ...) %>%
    dplyr::group_by(...) %>%
    dplyr::summarise(
      N = dplyr::n(),
      Missing = sum(is.na({{ var }})),
      M = mean({{ var }}, na.rm = na.rm),
      SD = sd({{ var }}, na.rm = na.rm),
      Min = min({{ var }}, na.rm = na.rm),
      Max = max({{ var }}, na.rm = na.rm),
      Range = Max - Min,
      Mdn = median({{ var }}, na.rm = na.rm),
      Q25 = quantile({{ var }}, .25, na.rm = na.rm),
      Q75 = quantile({{ var }}, .75, na.rm = na.rm)
    )
}
