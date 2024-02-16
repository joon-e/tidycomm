#' Reverse numeric, logical, or date/time continuous variables
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
#' @param data A [tibble][tibble::tibble-package] or a [tdcmm] model.
#' @param ... Numeric variables to be reverse scaled. If none are provided,
#' all numeric columns will be scaled.
#' @param lower_end Lower end of provided continuous scale (default is to use
#' minimum value of current values, which might not be the
#' actual lower end of the scale).
#' @param upper_end Upper end of provided continuous scale (default is to use
#' maximum value of current values, which might not be the actual upper end of
#' the scale).
#' @param name Optional name for the new reversed variable when a single
#' variable is provided. By default, the name will be the original variable
#' name suffixed with `_rev`.
#' @param overwrite Logical. If `TRUE`, it overwrites the original variable(s)
#' with the reversed values. If `FALSE` (default), a new variable(s) is created.
#'
#' @return A [tdcmm] model with the reversed variable(s).
#' @export
#' @family scaling
#'
#' @examples
#' WoJ %>% reverse_scale(autonomy_emphasis, lower_end = 0, upper_end = 1)
#' WoJ %>% reverse_scale(autonomy_emphasis, name = "my_reversed_variable",
#' lower_end = 0, upper_end = 1)
#' WoJ %>% reverse_scale(overwrite = TRUE)
#' WoJ %>%
#'   reverse_scale(autonomy_emphasis, lower_end = 0, upper_end = 1) %>%
#'   tab_frequencies(autonomy_emphasis, autonomy_emphasis_rev)
reverse_scale <- function(data, ...,
                          lower_end = NULL,
                          upper_end = NULL,
                          name = NULL,
                          overwrite = FALSE) {
  scale_vars <- rlang::quos(...)

  if (length(scale_vars) == 0) {
    message("NOTE: No variables provided. All numeric columns will be reversed.")
    numeric_vars <- sapply(data, is.numeric)
    scale_vars <- rlang::syms(names(data)[numeric_vars])
  }

  if (length(scale_vars) > 1 && !is.null(name)) {
    stop("The 'name' parameter cannot be used when reversing multiple variables.")
  }

  if (!is.null(name) && overwrite) {
    stop("You cannot specify both 'name' and 'overwrite' parameters simultaneously.")
  }

  for (scale_var_enquo in scale_vars) {
    scale_var_data <- data %>% dplyr::pull(!!scale_var_enquo)
    scale_var_str <- rlang::quo_name(scale_var_enquo)

    # Determine the name of the new variable
    if (overwrite) {
      new_var_name <- scale_var_str
    } else if (is.null(name)) {
      new_var_name <- paste0(scale_var_str, "_rev")
    } else {
      new_var_name <- name
    }

    if (!is.numeric(scale_var_data) &&
        !is.logical(scale_var_data) &&
        !lubridate::is.POSIXt(scale_var_data) &&
        !lubridate::is.Date(scale_var_data)) {
      stop(glue("{scale_var_str} must be numeric, logical, or a date/time."))
    }

    warn_about_ends <- FALSE

    if (lubridate::is.Date(scale_var_data) ||
        lubridate::is.POSIXt(scale_var_data)) {
      # handle dates/times
      if (is.null(lower_end)) {
        lower_end <- min(scale_var_data, na.rm = TRUE)
        warn_about_ends <- TRUE
      }
      if (is.null(upper_end)) {
        upper_end <- max(scale_var_data, na.rm = TRUE)
        warn_about_ends <- TRUE
      }
      if (!is.numeric(lower_end)) {
        lower_end <- as.numeric(lower_end)
      }
      if (!is.numeric(upper_end)) {
        upper_end <- as.numeric(upper_end)
      }
    } else {
      # handle other data types
      if (is.null(lower_end) || !is.numeric(lower_end)) {
        lower_end <- min(scale_var_data, na.rm = TRUE)
        warn_about_ends <- TRUE
      }
      if (is.null(upper_end) || !is.numeric(upper_end)) {
        upper_end <- max(scale_var_data, na.rm = TRUE)
        warn_about_ends <- TRUE
      }
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

    if (lubridate::is.Date(scale_var_data) ||
        lubridate::is.POSIXt(scale_var_data)) {
      # preserve timezone
      tz <- lubridate::tz(scale_var_data)
      # Convert dates to numeric values (number of days since a reference date)
      numeric_dates <- as.numeric(scale_var_data)
      # Reverse scale
      scale_orig <- seq(from = as.numeric(lower_end),
                        to = as.numeric(upper_end),
                        length.out = length(numeric_dates))
      scale_rev <- rev(scale_orig)
      map_to_rev_scale <- stats::approxfun(scale_orig, scale_rev)
      numeric_dates_rev <- map_to_rev_scale(numeric_dates)
      # Convert numeric values back to dates
      if(lubridate::is.Date(scale_var_data)) {
        data <- data %>%
          dplyr::mutate(!!new_var_name := as.Date(numeric_dates_rev,
                                                  origin = "1970-01-01",
                                                  tz = tz))
      } else { # if(lubridate::is.POSIXt(scale_var_data)) {
        data <- data %>%
          dplyr::mutate(!!new_var_name := as.POSIXct(numeric_dates_rev,
                                                     origin = "1970-01-01",
                                                     tz = tz))
      }
    } else if (is.numeric(scale_var_data)) {
      scale_orig <- seq(from = lower_end,
                        to = upper_end,
                        length.out = length(scale_var_data))
      scale_rev <- rev(scale_orig)
      map_to_rev_scale <- stats::approxfun(scale_orig, scale_rev)
      data <- data %>%
        dplyr::mutate(!!new_var_name := map_to_rev_scale(!!sym(scale_var_str)))
    } else { # if (is.logical(scale_var_data)) {
      data <- data %>%
          dplyr::mutate(!!new_var_name := !{!!rlang::sym(scale_var_str)})
    }
  }

  data %>%
    new_tdcmm(
      func = "reverse_scale",
      data = data,
      params = list(scale_vars = purrr::map_chr(scale_vars, ~ rlang::quo_name(.)),
                    lower_end = lower_end, upper_end = upper_end,
                    name = name, overwrite = overwrite)
    ) %>%
    return()
}

#' Rescale numeric continuous variables to new minimum/maximum boundaries
#'
#' Given a specified minimum and maximum, this function translates each value
#' into a new value within this specified range. The transformation maintains
#' the relative distances between values, resulting in changes to the mean and
#' standard deviations. However, if both the original scale and the transformed
#' scale are z-standardized, they will be equal again, indicating that the
#' relative positions and distributions of the values remain consistent.
#'
#' @param data A [tibble][tibble::tibble-package] or a [tdcmm] model.
#' @param ... Numeric variables to be min-max scaled. If none are provided,
#' all numeric columns will be scaled.
#' @param change_to_min The desired minimum value after scaling.
#' @param change_to_max The desired maximum value after scaling.
#' @param name Optional name for the new scaled variable when a single
#' variable is provided. By default, the name will be the original variable
#' name suffixed with the range. For example, "variable" becomes
#' "variable_3to5". Negative values are prefixed with "neg" to avoid invalid
#' columns names (e.g., -3 to 3 becomes "variable_neg3to5").
#' @param overwrite Logical. If `TRUE`, it overwrites the original variable(s)
#' with the scaled values. If `FALSE` (default), a new variable(s) is created.
#'
#' @return A [tdcmm] model with the min-max scaled variable(s).
#' @export
#' @family scaling
#'
#' @examples
#' WoJ %>% minmax_scale(autonomy_emphasis, change_to_min = 0,
#' change_to_max = 1)
#' WoJ %>% minmax_scale(autonomy_emphasis, name = "my_scaled_variable",
#' change_to_min = 0, change_to_max = 1)
#' WoJ %>%
#'   minmax_scale(autonomy_emphasis, change_to_min = 0, change_to_max = 1) %>%
#'   tab_frequencies(autonomy_emphasis, autonomy_emphasis_0to1)
minmax_scale <- function(data, ..., change_to_min = 0,
                         change_to_max = 1, name = NULL, overwrite = FALSE) {
  scale_vars <- rlang::quos(...)

  if (length(scale_vars) == 0) {
    message("NOTE: No variables provided. All numeric columns will
            receive new minimum/maximum boundaries.")
    numeric_vars <- sapply(data, is.numeric)
    scale_vars <- rlang::syms(names(data)[numeric_vars])
  }

  if (length(scale_vars) > 1 && !is.null(name)) {
    stop("The 'name' parameter cannot be used when scaling multiple variables.")
  }

  if (!is.null(name) && overwrite) {
    stop("You cannot specify both 'name' and 'overwrite' parameters
         simultaneously.")
  }

  for (scale_var_enquo in scale_vars) {
    scale_var_data <- data %>% dplyr::pull(!!scale_var_enquo)

    if (!is.numeric(scale_var_data) &&
        !is.logical(scale_var_data) &&
        !lubridate::is.POSIXt(scale_var_data) &&
        !lubridate::is.Date(scale_var_data)) {
      stop(paste("The variable",
                 rlang::quo_name(scale_var_enquo),
                 "must be numeric, logical, or a date/time."))
    }

    scale_var_str <- rlang::quo_name(scale_var_enquo)

    if (overwrite) {
      new_var_name <- scale_var_str
    } else if (is.null(name)) {
      new_var_name <- paste0(scale_var_str, "_",
                             gsub("-", "neg",
                                  paste0(change_to_min, "to", change_to_max)))
    } else {
      new_var_name <- name
    }

    data <- data %>%
      dplyr::mutate(!!new_var_name :=
                      rescale(!!scale_var_enquo,
                                      to = c(change_to_min, change_to_max)))
  }

  data %>%
    new_tdcmm(
      func = "minmax_scale",
      data = data,
      params = list(scale_vars = purrr::map_chr(scale_vars, ~ rlang::quo_name(.)),
                    change_to_min = change_to_min,
                    change_to_max = change_to_max,
                    name = name, overwrite = overwrite)
    ) %>%
    return()
}


#' Center numeric, continuous variables
#'
#' This function centers the specified numeric columns or all numeric columns
#' if none are specified. A centered scale has a mean of 0.0.
#'
#' @param data A [tibble][tibble::tibble-package] or a [tdcmm] model.
#' @param ... Numeric variables to be centered. If none are provided, all
#' numeric columns will be centered.
#' @param name Optional name for the new centered variable when a single
#' variable is provided. By default, the name will be the original variable
#' name suffixed with `_centered`.
#' @param overwrite Logical. If `TRUE`, it overwrites the original variable(s)
#' with the centered values. If `FALSE` (default), a new variable(s) is created.
#'
#' @return A [tdcmm] model with the centered variable(s).
#' @export
#' @family scaling
#'
#' @examples
#' WoJ %>% dplyr::select(autonomy_emphasis) %>% center_scale(autonomy_emphasis)
#' WoJ %>% center_scale(autonomy_emphasis, name = "my_centered_variable")
#' WoJ %>% center_scale(overwrite = TRUE)
#' WoJ %>%
#'   center_scale(autonomy_emphasis) %>%
#'   tab_frequencies(autonomy_emphasis, autonomy_emphasis_centered)
center_scale <- function(data, ..., name = NULL, overwrite = FALSE) {
  scale_vars <- rlang::quos(...)

  if (length(scale_vars) == 0) {
    message("NOTE: No variables provided. All numeric columns will be centered.")
    numeric_vars <- sapply(data, is.numeric)
    scale_vars <- rlang::syms(names(data)[numeric_vars])
  }

  if (length(scale_vars) > 1 && !is.null(name)) {
    stop("The 'name' parameter cannot be used when centering multiple variables.")
  }

  if (!is.null(name) && overwrite) {
    stop("You cannot specify both 'name' and 'overwrite' parameters simultaneously.")
  }

  for (scale_var_enquo in scale_vars) {
    scale_var_data <- data %>% dplyr::pull(!!scale_var_enquo)

    if (!is.numeric(scale_var_data)) {
      stop(paste("The variable", rlang::quo_name(scale_var_enquo),
                 "is not numeric. Please provide numeric variables only."))
    }

    scale_var_str <- rlang::quo_name(scale_var_enquo)

    if (overwrite) {
      new_var_name <- scale_var_str
    } else if (is.null(name)) {
      new_var_name <- paste0(scale_var_str, "_centered")
    } else {
      new_var_name <- name
    }

    data <- data %>%
      dplyr::mutate(!!new_var_name := as.vector(scale(!!scale_var_enquo, center = TRUE, scale = FALSE)))
  }

  data %>%
    new_tdcmm(
      func = "center_scale",
      data = data,
      params = list(scale_vars = scale_var_str,
                    name = name,
      overwrite = overwrite)
      ) %>%
    return()
}

#' Z-standardize numeric, continuous variables
#'
#' This function z-standardizes the specified numeric columns or all numeric columns
#' if none are specified. A z-standardized scale centers at a mean of 0.0 and has
#' a standard deviation of 1.0, making it comparable to other z-standardized distributions.
#'
#' @param data A [tibble][tibble::tibble-package] or a [tdcmm] model.
#' @param ... Numeric variables to be z-standardized. If none are provided, all numeric columns will be z-standardized.
#' @param name Optional name for the new z-standardized variable when a single variable is provided. By default, the name will be the original variable name suffixed with `_z`.
#' @param overwrite Logical. If `TRUE`, it overwrites the original variable(s) with the z-standardized values. If `FALSE` (default), a new variable(s) is created.
#'
#' @return A [tdcmm] model with the z-standardized variable(s).
#' @export
#' @family scaling
#'
#' @examples
#' WoJ %>% z_scale(autonomy_emphasis)
#' WoJ %>% z_scale(autonomy_emphasis, name = "my_zstdized_variable")
#' WoJ %>%
#'   z_scale(autonomy_emphasis) %>%
#'   tab_frequencies(autonomy_emphasis, autonomy_emphasis_z)
z_scale <- function(data, ..., name = NULL, overwrite = FALSE) {
  scale_vars <- rlang::quos(...)

  if (length(scale_vars) == 0) {
    message("NOTE: No variables provided. All numeric columns will be z-standardized.")
    numeric_vars <- sapply(data, is.numeric)
    scale_vars <- rlang::syms(names(data)[numeric_vars])
  }

  if (length(scale_vars) > 1 && !is.null(name)) {
    stop("The 'name' parameter cannot be used when recoding multiple variables.")
  }

  if (!is.null(name) && overwrite) {
    stop("You cannot specify both 'name' and 'overwrite' parameters simultaneously.")
  }

  for (scale_var_enquo in scale_vars) {
    scale_var_data <- data %>% dplyr::pull(!!scale_var_enquo)

    if (!is.numeric(scale_var_data)) {
      stop(paste("The variable", rlang::quo_name(scale_var_enquo),
                 "is not numeric. Please provide numeric variables only."))
    }

    scale_var_str <- rlang::quo_name(scale_var_enquo)

    if (overwrite) {
      new_var_name <- scale_var_str
    } else if (is.null(name)) {
      new_var_name <- paste0(scale_var_str, "_z")
    } else {
      new_var_name <- name
    }

    data <- data %>%
      dplyr::mutate(!!new_var_name := as.vector(scale(!!scale_var_enquo, center = TRUE, scale = TRUE)))
  }

  data %>%
    new_tdcmm(
      func = "z_scale",
      data = data,
      params = list(scale_vars = purrr::map_chr(scale_vars, ~ rlang::quo_name(.)),
                    name = name, overwrite = overwrite)
    ) %>%
    return()
}


#' Set specified values to NA in selected variables or entire data frame
#'
#' This function allows users to set specific values to `NA` in chosen variables
#' within a data frame. It can handle numeric, character, and factor variables.
#'
#' @param data A [tibble][tibble::tibble-package] or a [tdcmm] model.
#' @param ... One or more variables where specified values will be set to NA.
#' If no variables are provided, the function is applied to the entire data frame.
#' @param value A value (or vector of values) that needs to be set to NA.
#' @param name The name of the new variable(s). By default, this is the same name
#' as the provided variable(s) but suffixed with `_na`.
#' @param overwrite Logical. If `TRUE`, it overwrites the original variable(s).
#' You cannot specify both 'name' and 'overwrite' parameters simultaneously.
#'
#' @return A [tdcmm] model or a tibble.
#' @export
#' @family scaling
#'
#' @examples
#' WoJ %>%
#' dplyr::select(autonomy_emphasis) %>%
#' setna_scale(autonomy_emphasis, value = 5)
#' WoJ %>%
#' dplyr::select(autonomy_emphasis) %>%
#' setna_scale(autonomy_emphasis, value = 5, name = "new_na_autonomy")
#' WoJ %>%
#' setna_scale(value = c(2, 3, 4), overwrite = TRUE)
#' WoJ %>%
#' dplyr::select(country) %>% setna_scale(country, value = "Germany")
#' WoJ %>%
#' dplyr::select(country) %>% setna_scale(country, value = c("Germany", "Switzerland"))
setna_scale <- function(data, ..., value,
                        name = NULL, overwrite = FALSE) {

  if (missing(value) || is.null(value)) {
    stop("The 'value' parameter is required. Please specify the value or values
         you wish to set to NA.")
  }

  if (!is.null(name) && overwrite) {
    stop("You cannot specify both 'name' and 'overwrite' parameters
         simultaneously. Please choose one.")
  }

  value_type <- if (is.character(value)) "character" else if (is.numeric(value))
    "numeric" else typeof(value)
  scale_vars <- rlang::quos(...)

  if (length(scale_vars) == 0) {
    message("NOTE: No variables provided. All columns that fit the value type will receive updates to their missing values (NA).",
            call. = FALSE)
    scale_vars <- names(data)[sapply(data, function(col) {
      if (value_type == "character") {
        return(is.character(col) || is.factor(col))
      } else if (value_type == "numeric") {
        return(is.numeric(col))
      } else {
        return(FALSE)
      }
    })]
  } else {
    scale_vars <- purrr::map_chr(scale_vars, rlang::quo_name)
  }

  if (length(scale_vars) > 1 && !is.null(name)) {
    stop("The 'name' parameter cannot be used when recoding multiple variables.",
         call. = FALSE)
  }

  for (scale_var_str in scale_vars) {
    target_name <- if (overwrite) scale_var_str else if (is.null(name))
      paste0(scale_var_str, "_na") else name

    # Handle factors differently
    if (is.factor(data[[scale_var_str]])) {
      if (value_type == "character" || value_type == "numeric") {
        levels_to_na <- intersect(levels(data[[scale_var_str]]), as.character(value))
        if (length(levels_to_na) > 0) {
          data[[target_name]] <- data[[scale_var_str]]
          data[[target_name]][data[[scale_var_str]] %in% levels_to_na] <- NA
        }
      }
    } else {
      # Check if the specified value exists in the column
      if (!any(data[[scale_var_str]] %in% value)) {
        stop(paste("No values in", scale_var_str, "matched the specified value(s) to be set to NA. The function has been stopped."),
             call. = FALSE)
      }

      data <- data %>%
        dplyr::mutate(!!target_name := replace(
          .data[[scale_var_str]], .data[[scale_var_str]] %in% value, NA)
        )
    }
  }

  data %>%
    new_tdcmm(func = "setna_scale",
              data = data,
              params = list(scale_var = scale_vars,
                            value = value,
                            name = as.character(name),
                            overwrite = overwrite)) %>%
    return()
}


#' Recode one or more categorical variables into new categories
#'
#' This function transforms one or more categorical variables into new categories
#' based on specified mapping. For unmatched cases not specified in the mapping, a default
#' value can be assigned. Missing values are retained.
#'
#' @param data A tibble or a tdcmm model.
#' @param ... Variables to recode.
#' @param assign A named vector where names are the old values and values are
#' the new values to be assigned.
#' @param other The value for unmatched cases. By default, it is `NA`. This
#' parameter is used to assign a value to cases that do not match any of the keys
#' in the `assign` vector.
#' @param name The name of the new variable(s). If not specified, this is the same
#' name as the provided variable(s) but suffixed with `_rec`.
#' @param overwrite Logical. If `TRUE`, it overwrites the original variable(s).
#' You cannot specify both 'name' and 'overwrite' parameters simultaneously.
#'
#' @return A [tdcmm] model or a tibble.
#' @export
#' @family scaling
#' @examples
#' WoJ %>%
#' recode_cat_scale(country,
#' assign = c("Germany" = 1, "Switzerland" = 2), overwrite = TRUE)
#' WoJ %>%
#' recode_cat_scale(country,
#' assign = c("Germany" = "german", "Switzerland" = "swiss"), other = "other",
#' overwrite = TRUE)
#' WoJ %>%
#' recode_cat_scale(ethics_1, ethics_2,
#' assign = c(`1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1), other = 6, overwrite = TRUE)
#' WoJ %>%
#' recode_cat_scale(ethics_1, ethics_2,
#' assign = c(`1` = "very low", `2` = "low", `3` = "medium", `4` = "high", `5` = "very high"),
#' overwrite = TRUE)
#' WoJ %>%
#' dplyr::select(temp_contract) %>% recode_cat_scale(temp_contract,
#' assign = c(`Permanent` = "P", `Temporary` = "T"), other = "O")
recode_cat_scale <- function(data, ..., assign = NULL,
                             other = NA, overwrite = FALSE,
                             name = NULL) {

  if (is.null(assign)) {
    stop("The 'assign' argument is required.", call. = FALSE)
  }

  if (!is.null(name) && overwrite) {
    stop("You cannot specify both 'name' and 'overwrite' parameters simultaneously.",
         call. = FALSE)
  }

  scale_vars <- rlang::quos(...)

  if (length(scale_vars) == 0)
    stop("Please provide at least one variable to recode.", call. = FALSE)

  if (length(scale_vars) > 1 && !is.null(name)) {
    stop("The 'name' parameter cannot be used when recoding multiple variables.",
         call. = FALSE)
  }

  var_names <- purrr::map_chr(scale_vars, rlang::quo_name)

  for (var_name_str in var_names) {
    unique_values <- unique(data[[var_name_str]])

    # Check if assigned values are part of the original scale
    invalid_assign_values <- setdiff(names(assign), unique_values)
    if (length(invalid_assign_values) > 0) {
      stop(paste("The following names in the 'assign' parameter for",
                 var_name_str, "are not part of the original scale:",
                 paste(invalid_assign_values, collapse = ", "), ". Please ensure the 'assign' parameter is correctly specified. Recoding has been stopped."))
    }

    new_var_name <- if (overwrite) {
      var_name_str
    } else if (!is.null(name)) {
      name
    } else {
      paste0(var_name_str, "_rec")
    }

    # Check unassigned values, but exclude NAs
    unmatched_values <- setdiff(na.omit(unique_values), names(assign))

    # Add unassigned values to the assign vector with the 'other' value
    assign[as.character(unmatched_values)] <- other

    if (length(unmatched_values) > 0) {
      message(paste("The following unassigned values were found in", var_name_str, ":",
                    paste(unmatched_values, collapse = ", "),
                    ". They were recoded to the 'other' value (", other, ")."))
    }

    recoding_args <- c(assign, ..else = other)
    column_as_char <- as.character(data[[var_name_str]])
    new_values <- dplyr::recode(column_as_char, !!!recoding_args)
    data[[new_var_name]] <- new_values

    # Convert the column to a factor
    data[[new_var_name]] <- as.factor(data[[new_var_name]])
  }

  data %>%
    new_tdcmm(
      func = "recode_cat_scale",
      data = data,
      params = list(
        scale_var = scale_vars,
        assign = assign,
        other = other,
        overwrite = overwrite,
        name = if (overwrite) scale_vars else paste0(scale_vars, "_rec")
      )
    ) %>%
    return()
}



#' Categorize numeric variables into categories
#'
#' This function recodes one or more numeric variables into categorical
#' variables based on a specified lower end, upper end, and intermediate breaks.
#' The intervals created include the right endpoint of the interval. For example,
#' breaks = c(2, 3) with lower_end = 1 and upper_end = 5 creates intervals from
#' 1 to ≤ 2, >2 to ≤ 3, and >3 to ≤ 5. If the lower or upper ends are not provided,
#' the function defaults to the minimum and maximum values of the data and issues
#' a warning. This default behavior is prone to errors, however, because a scale may not
#' include its actual lower and upper ends which might in turn affect the recoding
#' process. Hence, it is strongly suggested to manually set the lower and upper
#' bounds of the original continuous scale.
#'
#' @param data A tibble or a tdcmm model.
#' @param ... Variables to recode as factor variables in categories.
#' If no variables are specified, all numeric columns will be recoded.
#' @param breaks A vector of numeric values specifying the breaks for
#' categorizing the data between the lower and upper ends.
#' The breaks define the boundaries of the intervals. Setting this parameter is
#' required.
#' @param labels A vector of string labels for each interval. The number of
#' labels must match the number of intervals defined by the breaks and
#' lower/upper ends.Setting this parameter is required.
#' @param lower_end Optional numeric value specifying the lower end of the scale.
#' If not provided, defaults to the minimum value of the data.
#' @param upper_end Optional numeric value specifying the upper end of
#' the scale. If not provided, defaults to the maximum value of the data.
#' @param name Optional string specifying the name of the new variable(s).
#' By default, the new variable names are the original variable names suffixed
#' with `_cat`.
#' @param overwrite Logical indicating whether to overwrite the original
#' variable(s) with the new categorical variables. If `TRUE`,
#' the original variable(s) are overwritten.
#'
#' @return A modified tibble or tdcmm model with the recoded variables.
#' @export
#' @family scaling
#'
#' @examples
#' WoJ %>%
#' dplyr::select(trust_parliament, trust_politicians) %>%
#' categorize_scale(trust_parliament, trust_politicians,
#' lower_end = 1, upper_end = 5, breaks = c(2, 3),
#' labels = c("Low", "Medium", "High"), overwrite = FALSE)
#' WoJ %>%
#' dplyr::select(autonomy_selection) %>%
#' categorize_scale(autonomy_selection, breaks = c(2, 3, 4),
#' lower_end = 1, upper_end = 5,
#' labels = c("Low", "Medium", "High", "Very High"),
#' name = "autonomy_in_categories")
categorize_scale <- function(data, ..., breaks, labels,
                             lower_end = NULL, upper_end = NULL,
                             name = NULL, overwrite = FALSE) {
  scale_vars <- rlang::quos(...)

  if (length(scale_vars) == 0) {
    message("NOTE: No variables provided. All numeric columns will be recategorized.")
    numeric_vars <- sapply(data, is.numeric)
    scale_vars <- rlang::syms(names(data)[numeric_vars])
  }

  if (length(scale_vars) > 1 && !is.null(name)) {
    stop("The 'name' parameter cannot be used when recategorizing multiple variables.",
         call. = FALSE)
  }

  if (!is.null(name) && overwrite) {
    stop("You cannot specify both 'name' and 'overwrite' parameters simultaneously.",
         call. = FALSE)
  }

  for (i in seq_along(scale_vars)) {
    scale_var_enquo <- scale_vars[[i]]
    scale_data <- dplyr::pull(data, !!scale_var_enquo)

    if (!is.numeric(scale_data)) {
      stop(paste("The variable", rlang::quo_name(scale_var_enquo),
                 "is not numeric. Please provide numeric variables only."))
    }

    warn_about_ends <- FALSE

    if (is.null(lower_end)) {
      lower_end <- min(scale_data, na.rm = TRUE)
      warn_about_ends <- TRUE
    }
    if (is.null(upper_end)) {
      upper_end <- max(scale_data, na.rm = TRUE)
      warn_about_ends <- TRUE
    }

    if (warn_about_ends) {
      warning(glue::glue("Lower and/or upper end missing. Based on the minimum and ",
                         "maximum values observed in the data, the original scale ",
                         "({rlang::quo_name(scale_var_enquo)}) is assumed to range from {lower_end} to ",
                         "{upper_end}. To prevent this warning, please provide the ",
                         "lower_end and upper_end values as arguments when calling the ",
                         "function."),
              call. = FALSE)
    }

    if (length(breaks) + 1 != length(labels)) {
      stop("The number of breaks plus one must match the number of labels.",
           call. = FALSE)
    }

    # Determine the name of the new variable
    new_var_name <- if (overwrite) {
      rlang::quo_name(scale_var_enquo)
    } else {
      if (is.null(name)) {
        paste0(rlang::quo_name(scale_var_enquo), "_cat")
      } else {
        paste0(name, "_", i)
      }
    }

    full_breaks <- c(lower_end, breaks, upper_end)

    data <- dplyr::mutate(
      data,
      !!rlang::sym(new_var_name) :=
        cut(
          !!scale_var_enquo,
          breaks = full_breaks,
          labels = labels,
          include.lowest = TRUE,
          right = TRUE
        )
    )
  }

  data %>%
    new_tdcmm(
      func = "categorize_scale",
      data = data,
      params = list(scale_vars = scale_vars,
                    lower_end = lower_end, upper_end = upper_end,
                    breaks = breaks, labels = labels,
                    name = name, overwrite = overwrite)
    ) %>%
    return()
}


#' Convert categorical variables to dummy variables
#'
#' This function transforms specified categorical variables into dummy variables.
#' Each level of the categorical variable is represented by a new dummy variable.
#' Missing values are retained.
#' These new dummy variables are appended to the original data frame. This function
#' does not allow specifying new column names for the dummy variables. Instead, it
#' follows a consistent naming pattern: the new dummy variables are named using the
#' original variable name with the category value appended. For example, if a
#' categorical variable named "autonomy" with levels "low", "medium", "high" is dummified,
#' the new dummy variables will be named "autonomy_low", "autonomy_medium", "autonomy_high".
#'
#' @param data A [tibble][tibble::tibble-package] or a [tdcmm] model.
#' @param ... Categorical variables to be transformed into dummy variables.
#' Category names will be automatically appended to the newly created dummy variables.
#' @param overwrite Logical. If `TRUE`, it overwrites the original variable(s)
#' with the dummy variables. If `FALSE` (default), new variables are created.
#'
#' @return A [tdcmm] model with the dummy variables appended.
#' @export
#' @family scaling
#'
#' @examples
#' WoJ %>% dplyr::select(temp_contract) %>% dummify_scale(temp_contract)
#' WoJ %>% categorize_scale(autonomy_emphasis, breaks = c(2, 3),
#' labels = c('low', 'medium', 'high')) %>%
#' dummify_scale(autonomy_emphasis_cat) %>% dplyr::select(starts_with('autonomy_emphasis'))
dummify_scale <- function(data, ..., overwrite = FALSE) {
  scale_vars <- rlang::quos(...)

  if (length(scale_vars) == 0) {
    message("NOTE: No variables provided. All factor columns will be dummified.")
    factor_vars <- sapply(data, is.factor)
    scale_vars <- rlang::syms(names(data)[factor_vars])
  }

  for (scale_var_enquo in scale_vars) {
    scale_var_str <- rlang::quo_name(scale_var_enquo)

    if (!is.factor(data[[scale_var_str]]) && !is.character(data[[scale_var_str]])) {
      stop(paste("The column", scale_var_str, "is neither a factor nor a character column and cannot be dummified."), call. = FALSE)
    }

    # Create dummies
    dummies <- fastDummies::dummy_cols(data, select_columns = scale_var_str, remove_selected_columns = overwrite, ignore_na = TRUE)

    # Rename the columns to ensure consistency
    new_dummy_vars <- setdiff(names(dummies), names(data))
    for (var in new_dummy_vars) {
      new_name <- paste0(tolower(scale_var_str), "_", tolower(gsub(paste0("^", scale_var_str, "_"), "", var)))
      names(dummies)[names(dummies) == var] <- new_name
    }

    data <- dummies
  }

  data %>%
    new_tdcmm(
      func = "dummify_scale",
      data = data,
      params = list(scale_vars = scale_var_str,
                    overwrite = overwrite)
    ) %>%
    return()
}
