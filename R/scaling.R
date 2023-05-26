#' Reverse a numeric continuous scale
#'
#' Reverses a continuous scale into a new variable. A 5-1 scale thus turns into
#' a 1-5 scale. Missing values are retained. This function really just subtracts
#' each value from the maximum value which was incremented by one.
#'
#' @param data a [tibble][tibble::tibble-package] or a [tdcmm] model
#' @param scale_var a numeric variable for which the scale gets reversed
#' @param name the name of the new variable. By default, this is the same name
#' as the `scale_var` but suffixed with `_rev`.
#'
#' @return a [tdcmm] model
#' @export
#' @family scaling
#'
#' @examples
#' WoJ %>% reverse_scale(autonomy_emphasis)
#' WoJ %>% reverse_scale(autonomy_emphasis, name = "my_reversed_variable")
#'
#' WoJ %>%
#'   reverse_scale(autonomy_emphasis) %>%
#'   tab_frequencies(autonomy_emphasis, autonomy_emphasis_rev) %>%
#'   visualize()
reverse_scale <- function(data, scale_var,
                          name = paste0(as_label(expr({{ scale_var }})),
                                        "_rev")) {
  scale_var_data <- data %>%
    dplyr::pull({{ scale_var }})
  if (!is.numeric(scale_var_data)) {
    stop("... must be numeric.")
  }
  reverse_base <- max(scale_var_data,
                      na.rm = TRUE) + 1
  data %>%
    dplyr::mutate(!!name := reverse_base - {{ scale_var }}) %>%
    new_tdcmm() %>%
    return()
}

#' Change/Rescale a numeric continuous scale
#'
#' Given a specified minimum and maximum, this function translates each value
#' into a new value within this specified range, thereby keeping distances the
#' same. Hence, mean and standard deviations change but. However, if both the
#' original scale and the changed scale were to be z-standardized, they would
#' turn out equal again.
#'
#' @param data a [tibble][tibble::tibble-package] or a [tdcmm] model
#' @param scale_var a numeric variable for which the scale gets changed
#' @param name the name of the new variable. By default, this is the same name
#' as `scale_var` but suffixed with `change_to_min` and `change_to_max` so that,
#' for example, "variable" becomes "variable_3to5".
#' @param change_to_min numeric minimum of the new range (default is `0`)
#' @param change_to_max numeric maximum of the new range (default is `1`)
#'
#' @return a [tdcmm] model
#' @export
#' @family scaling
#'
#' @examples
#' WoJ %>% change_scale(autonomy_emphasis)
#' WoJ %>% change_scale(autonomy_emphasis, change_to_min = 1, change_to_max = 10, name = "my_changed_variable")
#'
#' WoJ %>%
#'   change_scale(autonomy_emphasis) %>%
#'   tab_frequencies(autonomy_emphasis, autonomy_emphasis_0to1) %>%
#'   visualize()
change_scale <- function(data, scale_var,
                         change_to_min = 0, change_to_max = 1,
                         name = paste0(as_label(expr({{ scale_var }})), "_",
                                       change_to_min, "to", change_to_max)) {
  scale_var_data <- data %>%
    dplyr::pull({{ scale_var }})
  if (!is.numeric(scale_var_data) &
      !is.logical(scale_var_data) &
      !lubridate::is.POSIXt(scale_var_data) &
      !lubridate::is.Date(scale_var_data)) {
    stop("... must be numeric, logical, or a date/time.")
  }

  if (!is.numeric(change_to_max) |
      !is.numeric(change_to_min)) {
    stop("... change_to_min and change_to_max must be numeric.")
  }

  if (change_to_max <= change_to_min) {
    stop("... change_to_max must be larger than change_to_min.")
  }

  data %>%
    dplyr::mutate(!!name := scales::rescale({{ scale_var }},
                                            to = c(change_to_min,
                                                   change_to_max))) %>%
    new_tdcmm() %>%
    return()
}

#' Center a numeric continuous scale
#'
#' Subtracts the mean from each individual data point. A centered scale then
#' centers at a mean of 0.0.
#'
#' @param data a [tibble][tibble::tibble-package] or a [tdcmm] model
#' @param scale_var a numeric variable for which the scale gets centered
#' @param name the name of the new variable. By default, this is the same name
#' as `scale_var` but suffixed with `_centered`.
#'
#' @return a [tdcmm] model
#' @export
#' @family scaling
#'
#' @examples
#' WoJ %>% center_scale(autonomy_emphasis)
#' WoJ %>% center_scale(autonomy_emphasis, name = "my_centered_variable")
#'
#' WoJ %>%
#'   center_scale(autonomy_emphasis) %>%
#'   tab_frequencies(autonomy_emphasis, autonomy_emphasis_centered) %>%
#'   visualize()
center_scale <- function(data, scale_var,
                         name = paste0(as_label(expr({{ scale_var }})),
                                       "_centered")) {
  scale_var_data <- data %>%
    dplyr::pull({{ scale_var }})
  if (!is.numeric(scale_var_data)) {
    stop("... must be numeric.")
  }
  data %>%
    dplyr::mutate(!!name := as.vector(scale({{ scale_var }},
                                            center = TRUE,
                                            scale = FALSE))) %>%
    new_tdcmm() %>%
    return()
}

#' Z-standardize a numeric continuous scale
#'
#' Subtracts the mean from each individual data point and then divides the
#' result by the standard deviation. Eventually, a z-standardized scale centers
#' at a mean of 0.0 and obtains a standard deviation of 1.0. It becomes thus
#' comparable to other z-standardized distributions.
#'
#' @param data a [tibble][tibble::tibble-package] or a [tdcmm] model
#' @param scale_var a numeric variable for which the scale gets z-standardized
#' @param name the name of the new variable. By default, this is the same name
#' as `scale_var` but suffixed with `_z`.
#'
#' @return a [tdcmm] model
#' @export
#' @family scaling
#'
#' @examples
#' WoJ %>% z_scale(autonomy_emphasis)
#' WoJ %>% z_scale(autonomy_emphasis, name = "my_zstdized_variable")
#'
#' WoJ %>%
#'   z_scale(autonomy_emphasis) %>%
#'   tab_frequencies(autonomy_emphasis, autonomy_emphasis_z) %>%
#'   visualize()
z_scale <- function(data, scale_var,
                    name = paste0(as_label(expr({{ scale_var }})),
                                  "_z")) {
  scale_var_data <- data %>%
    dplyr::pull({{ scale_var }})
  if (!is.numeric(scale_var_data)) {
    stop("... must be numeric.")
  }
  data %>%
    dplyr::mutate(!!name := as.vector(scale({{ scale_var }},
                                            center = TRUE,
                                            scale = TRUE))) %>%
    new_tdcmm() %>%
    return()
}
