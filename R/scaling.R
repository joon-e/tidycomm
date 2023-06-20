#' Reverse a numeric continuous scale
#'
#' Reverses a continuous scale into a new variable. A 5-1 scale thus turns into
#' a 1-5 scale. Missing values are retained. For a given continuous variable
#' the lower and upper end of the scale should be provided. If they are not
#' provided, the function assumes the scale's minimum and maximum value to
#' represent these lower/upper ends (and issues a warning about this fact).
#' This default behavior is prone to errors, however, because a scale may not
#' include its actual lower and upper ends which might in turn affect correct
#' reversing. Hence, it is strongly suggested to manually set the lower and
#' upper bounds of the original continuous scale.
#'
#' @param data a [tibble][tibble::tibble-package] or a [tdcmm] model
#' @param scale_var a numeric variable for which the scale gets reversed
#' @param name the name of the new variable. By default, this is the same name
#' as the `scale_var` but suffixed with `_rev`.
#' @param lower_end lower end of provided continuous scale (i.e., of scale_var)
#' (default is to use minimum value of current values, which might not be the
#' actual lower end of the scale)
#' @param upper_end upper end of provided continuous scale (i.e., of scale_var)
#' (default is to use maximum value of current values, which might not be the
#' actual upper end of the scale)
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
#'   tab_frequencies(autonomy_emphasis, autonomy_emphasis_rev)
reverse_scale <- function(data, scale_var,
                          name = paste0(as_label(expr({{ scale_var }})),
                                        "_rev"),
                          lower_end = NULL,
                          upper_end = NULL) {
  scale_var_str <- as_label(expr({{ scale_var }}))
  scale_var_data <- data %>%
    dplyr::pull({{ scale_var }})
  if (!is.numeric(scale_var_data)) {
    stop(glue("{scale_var_str} must be numeric."))
  }

  warn_about_ends <- FALSE
  if (is.null(lower_end) | !is.numeric(lower_end)) {
    lower_end <- min(scale_var_data, na.rm = TRUE)
    warn_about_ends <- TRUE
  }

  if (is.null(upper_end) | !is.numeric(upper_end)) {
    upper_end <- max(scale_var_data, na.rm = TRUE)
    warn_about_ends <- TRUE
  }

  if (warn_about_ends) {
    warning(glue("Lower and/or upper end missing. Based on the minimum and ",
                 "maximum values observed in the data, the original scale ",
                 "({scale_var_str}) is assumed to range from {lower_end} to ",
                 "{upper_end}. To prevent this warning, please provide the ",
                 "lower_end and upper_end values as arguments when calling the",
                 " function."),
            call. = FALSE)
  }

  scale_orig <- seq(lower_end, upper_end)
  scale_rev <- rev(scale_orig)
  mapped_scale <- as.list(scale_rev)
  names(mapped_scale) <- scale_orig

  mapped_scale[[".x"]] <- scale_var_data

  data %>%
    dplyr::mutate(!!name := do.call(dplyr::recode,
                                    mapped_scale)) %>%
    new_tdcmm() %>%
    return()
}

#' Rescale a numeric continuous scale to new minimum/maximum boundaries
#'
#' Given a specified minimum and maximum, this function translates each value
#' into a new value within this specified range. The transformation maintains
#' the relative distances between values, resulting in changes to the mean and
#' standard deviations. However, if both the original scale and the transformed
#' scale are z-standardized, they will be equal again, indicating that the
#' relative positions and distributions of the values remain consistent.
#'
#' @param data a [tibble][tibble::tibble-package] or a [tdcmm] model
#' @param scale_var a numeric variable for which the scale gets changed
#' @param name the name of the new variable. By default, this is the same name
#' as `scale_var` but suffixed with `change_to_min` and `change_to_max` so that,
#' for example, "variable" becomes "variable_3to5". Negative values are prefixed
#' with "neg" to avoid invalid columns names (e.g., -3 to 3 becomes
#' "variable_neg3to5").
#' @param change_to_min numeric minimum of the new range (default is `0`)
#' @param change_to_max numeric maximum of the new range (default is `1`)
#'
#' @return a [tdcmm] model
#' @export
#' @family scaling
#'
#' @examples
#' WoJ %>% minmax_scale(autonomy_emphasis)
#' WoJ %>% minmax_scale(autonomy_emphasis, change_to_min = 1, change_to_max = 10, name = "my_changed_variable")
#'
#' WoJ %>%
#'   minmax_scale(autonomy_emphasis) %>%
#'   tab_frequencies(autonomy_emphasis, autonomy_emphasis_0to1)
minmax_scale <- function(data, scale_var,
                         change_to_min = 0, change_to_max = 1,
                         name = paste0(as_label(expr({{ scale_var }})), "_",
                                       gsub("-", "neg",
                                            paste0(change_to_min,
                                                   "to",
                                                   change_to_max)))) {
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
#'   tab_frequencies(autonomy_emphasis, autonomy_emphasis_centered)
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
#'   tab_frequencies(autonomy_emphasis, autonomy_emphasis_z)
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
