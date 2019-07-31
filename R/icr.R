#' Compute intercoder reliability estimates
#'
#' Computes various intercoder reliability estimates
#'
#' @param data a [tibble][tibble::tibble-package]
#' @param unit_var Variable with unit identifiers
#' @param coder_var Variable with coder identifiers
#' @param ... Variables to compute intercoder reliability estimates for
#' @param ... Optional named vector with levels of test variables
#' @param na.omit Logical indicating whether `NA` values should be stripped
#'   before computation. Defaults to FALSE.
#' @param agreement Logical indicating whether simple percent agreement should
#'   be computed. Defaults to TRUE.
#' @param holsti Logical indicating whether Holsti's reliability estimate
#'   (mean pairwise agreement) should be computed. Defaults to TRUE.
#' @param kripp_alpha Logical indicating whether Krippendorff's Alpha should
#'   be computed. Defaults to TRUE.
#'
#' @return a [tibble][tibble::tibble-package]
#'
#' @family intercoder reliability
#'
#' @export
compute_icr <- function(data, unit_var, coder_var, ...,
                        levels = NULL, na.omit = FALSE,
                        agreement = TRUE, holsti = TRUE, kripp_alpha = TRUE) {

  test_vars <- rlang::enquos(...)

  purrr::map_dfr(test_vars, icr_test, data, {{ unit_var }}, {{ coder_var }},
                 levels, na.omit,
                 agreement, holsti, kripp_alpha)

}


#' Compute intercoder reliability estimates for one test variable
#'
#' Computes intercoder reliability estimates for one test variable
#'
#' @param test_var Variable to compute estimates for
#' @inheritParams compute_icr
#'
#' @family intercoder reliability
icr_test <- function(test_var, data, unit_var, coder_var,
                     levels = c(), na.omit,
                     agreement, holsti, kripp_alpha) {

  ucm <- unit_coder_matrix(data, {{ unit_var }}, {{ coder_var }}, {{ test_var}})

  # Get variable level
  var_string <- rlang::as_name(rlang::enquo(test_var))
  if (hasName(levels, var_string)) {
    var_level <- levels[[var_string]]
  } else {
    var_level <- "nominal"
  }

  # Check for missing values
  if (any(is.na(ucm))) {
    if (na.omit) {
      ucm <- na.omit(ucm)
    } else {
      warning(glue::glue("Variable '{var_string}' contains missing values.",
                         "Consider setting na.omit = TRUE or recoding missing values",
                         .sep = " "),
              call. = FALSE)
    }
  }

  # List descriptives
  test_vals <- tibble::tibble(
    Variable = var_string,
    n_Units = dim(ucm)[1],
    n_Coders = dim(ucm)[2],
    n_Categories = length(unique(na.omit(as.vector(ucm)))),
    Level = var_level
  )

  # Compute reliability estimates
  if (agreement) {
    test_vals <- test_vals %>%
      dplyr::bind_cols(
        Agreement = icr_agreement(ucm)
      )
  }

  if (holsti) {
    test_vals <- test_vals %>%
      dplyr::bind_cols(
        Holsti = icr_holsti(ucm)
      )
  }

  if (kripp_alpha) {
    test_vals <- test_vals %>%
      dplyr::bind_cols(
        Kripp_Alpha = icr_kripp_alpha(ucm, var_level)
      )
  }

  test_vals
}

#' Generate units-coders matrix
#'
#' Generates a units-coders matrix for a test variable
#'
#' @inheritParams icr_test
#'
#' @family intercoder reliability
unit_coder_matrix <- function(data, unit_var, coder_var, test_var) {

  data %>%
    dplyr::select({{ unit_var }}, {{ coder_var }}, {{ test_var }}) %>%
    tidyr::spread({{ coder_var }}, {{ test_var }}) %>%
    dplyr::select(-1) %>%
    as.matrix()
}
