#' Describe variables
#'
#' Describe variables by several measures of
#' central tendency and variability. If no variables are specified,
#' all numeric (integer or double) variables are described.
#'
#' @param data a [tibble][tibble::tibble-package]
#' @param ... Variables to describe (column names)
#' @param na.rm a logical value indicating whether NA values should be stripped
#'     before the computation proceeds. Defaults to TRUE.
#'
#' @return a [tibble][tibble::tibble-package]
#'
#' @examples
#' iris %>% describe()
#' mtcars %>% describe(mpg, am, cyl)
#'
#' @export
describe <- function(data, ..., na.rm = TRUE) {

  # Get current grouping
  grouping <- dplyr::groups(data)

  # Get describe vars
  vars <- rlang::quos(...)

  if (length(vars) == 0) {
    vars <- data %>%
      dplyr::ungroup() %>% # Deselect grouping variable
      dplyr::select_if(is.numeric) %>%
      names() %>%
      rlang::syms()
  }

  # Check if vars is empty and all vars are numeric
  if (length(vars) == 0) {
    stop("No numeric variables found to descibre")
  }

  if (!all(purrr::map_lgl(data %>%
                          dplyr::ungroup() %>%
                          dplyr::select(!!! vars),
                          is.numeric))) {
    stop("... must only contain numeric variables.")
  }

  # Describe
  data %>%
    dplyr::select(!!! vars, !!!grouping) %>%
    tidyr::gather(Variable, Value, !!! vars) %>%
    dplyr::group_by(Variable, add = TRUE) %>%
    dplyr::summarise(
      N = dplyr::n(),
      Missing = sum(is.na(Value)),
      M = mean(Value, na.rm = na.rm),
      SD = sd(Value, na.rm = na.rm),
      Min = min(Value, na.rm = na.rm),
      Q25 = quantile(Value, .25, na.rm = na.rm),
      Mdn = median(Value, na.rm = na.rm),
      Q75 = quantile(Value, .75, na.rm = na.rm),
      Max = max(Value, na.rm = na.rm),
      Range = Max - Min,
      Skewness = moments::skewness(Value, na.rm = na.rm),
      Kurtosis = moments::kurtosis(Value, na.rm = na.rm)
    )

}
