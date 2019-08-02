#' Compute reliability
#'
#' Computes reliability estimates of index/scale variables.
#' Provides a wrapper for \code\link[MBESS]{ci.reliability}.
#'
#' @param data a [tibble][tibble::tibble-package]
#' @param ... Variables to compute reliability estimate for
#' @param type See \code\link[MBESS]{ci.reliability}
#' @param interval.type See \code\link[MBESS]{ci.reliability}
#' @param bootstrap.samples See \code\link[MBESS]{ci.reliability}
#' @param conf.level See \code\link[MBESS]{ci.reliability}
#'
#' @return a [tibble][tibble::tibble-package]
#'
#' @family reliability
#'
#' @export
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
