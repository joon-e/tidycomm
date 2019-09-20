#' Get reliability estimates of index variables
#'
#' Get reliability estimates of index variables created with \code{\link{add_index}}.
#'
#' @param data a [tibble][tibble::tibble-package]
#' @param ... Index variables created with \code{\link{add_index}}. Leave empty
#'   to get reliability estimates for all index variables.
#' @param type Type of reliability estimate. See \code{\link[MBESS]{ci.reliability}}
#' @param interval.type Type of reliability estimate confidence interval.
#'   See \code{\link[MBESS]{ci.reliability}}
#' @param bootstrap.samples Number of bootstrap samples for CI calculation.
#'   See \code{\link[MBESS]{ci.reliability}}
#' @param conf.level Confidence level for estimate CI.
#'   See \code{\link[MBESS]{ci.reliability}}
#' @param progress Show progress for reliability estimate computation. Useful
#'   if using computationally intense computations (e. g., many bootstrapping
#'   samples) and many index variables.
#'
#' @return a [tibble][tibble::tibble-package]
#'
#' @family reliability
#'
#' @seealso [add_index()] to create index variables
#'
#' @examples
#' WoJ %>%
#'   add_index(ethical_flexibility, ethics_1, ethics_2, ethics_3, ethics_4) %>%
#'   get_reliability()
#'
#' @export
get_reliability <- function(data, ..., type = "alpha",
                            interval.type = NULL,
                            bootstrap.samples = NULL,
                            conf.level = NULL,
                            progress = FALSE) {

  test_vars <- quos(...)

  # Checks
  if (length(test_vars) == 0) {
    test_vars <- data %>%
      dplyr::select_if(~ !is.null(attr(., "index_of"))) %>%
      names() %>%
      syms()
  }

  if (length(test_vars) == 0) {
    stop(glue("No variables with attribute 'index_of' found in data. ",
              "Use add_index() to create variables for get_reliability()",
              ", or use compute_reliability() to directly add reliability variables."),
         call. = FALSE)
  }

  if (missing(interval.type)) {
    interval.type <- "none"
  }

  if (missing(bootstrap.samples)) {
    bootstrap.samples <- 10000
  }

  if (missing(conf.level)) {
    conf.level <- 0.95
  }

  # Prepare tibble
  reli_df <- tibble::tibble()
  i <- 0

  # Iterate over test variables
  for (test_var in test_vars) {

    test_var_str <- as_name(test_var)
    index_vars <- attr(data[[test_var_str]], "index_of")

    if (is.null(index_vars)) {
      stop(glue("No attribute 'index_of' found for variable {test_var_str}. ",
                "Use add_index() to create variables for get_reliability()",
                ", or use compute_reliability() to directly add reliability variables."),
           call. = FALSE)
    }

    if (progress) {
      i <- i + 1
      j <- length(test_vars)
      message(glue("{Sys.time()}: Starting reliability computation for ",
                   "{test_var_str} (variable {i} of {j})."))
    }

    var_df <- tibble::tibble(
      Index = test_var_str,
      Index_of = paste(index_vars, collapse = ", "),
      M = mean(data[[test_var_str]], na.rm = TRUE),
      SD = sd(data[[test_var_str]], na.rm = TRUE)) %>%
      dplyr::bind_cols(
        compute_reliability(data, !!!syms(index_vars),
                            type = type,
                            interval.type = interval.type,
                            bootstrap.samples = bootstrap.samples,
                            conf.level = conf.level)
      )

    reli_df <- reli_df %>%
      dplyr::bind_rows(var_df)

    if (progress) {
      message(glue("{Sys.time()}: Finished reliability computation for ",
                   "{test_var_str}"))
    }

  }

  return(reli_df)
}

### Internal functions ###

## Compute reliability
##
## Computes reliability estimates of index/scale variables.
## Provides a wrapper for \code{\link[MBESS]{ci.reliability}}.
##
## @param data a [tibble][tibble::tibble-package]
## @param ... Variables to compute reliability estimate for
## @param type Type of reliability estimate. See \code{\link[MBESS]{ci.reliability}}
## @param interval.type Type of reliability estimate confidence interval.
##   See \code{\link[MBESS]{ci.reliability}}
## @param bootstrap.samples Number of bootstrap samples for CI calculation.
##   See \code{\link[MBESS]{ci.reliability}}
## @param conf.level Confidence level for estimate CI.
##   See \code{\link[MBESS]{ci.reliability}}
##
## @return a [tibble][tibble::tibble-package]
##
## @family reliability
##
## @keywords internal
compute_reliability <- function(data, ..., type = "alpha",
                                interval.type = NULL,
                                bootstrap.samples = NULL,
                                conf.level = NULL) {

  # Checks
  if (missing(interval.type)) {
    interval.type <- "none"
  }

  if (missing(bootstrap.samples)) {
    bootstrap.samples <- 10000
  }

  if (missing(conf.level)) {
    conf.level <- 0.95
  }

  # Compute reliability
  rel_list <- data %>%
    dplyr::select(...) %>%
    MBESS::ci.reliability(type = type,
                          interval.type = interval.type,
                          B = bootstrap.samples,
                          conf.level = conf.level)
  # Prepare tibble
  if (type == "alpha") {
    type <- "Cronbachs_Alpha"
  } else if (type == "alpha-cfa") {
    type <- "Alpha_CFA"
  } else if (type == "omega") {
    type <- "Omega"
  } else if (type == "hierarchical") {
    type <- "Hierarchical_Omega"
  } else if (type == "categorical") {
    type <- "Categorical_Omega"
  }

  # Return tibble
  r_df <- tibble::tibble(
    !!type := rel_list$est
  )

  if (interval.type != "none") {
    r_df <- r_df %>%
      dplyr::bind_cols(
        CI_LL = rel_list$ci.lower,
        CI_UL = rel_list$ci.upper,
        CI_Type = rel_list$interval.type
      )
  }

  return(r_df)
}
