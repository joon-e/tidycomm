#' Compute Cohen's Kappa
#'
#' Computes Cohen's Kappa.
#'
#' @param ucm Units-coders matrix
#'
#' @family intercoder reliability
icr_cohens_kappa <- function(ucm) {

  if(any(is.na(ucm))) {
    return(NA)
  }

  if (dim(ucm)[2] > 2) {
    warning(glue::glue("Cohen's Kappa is only available for two coders. ",
                       "Try Fleiss' Kappa (fleiss_kappa = TRUE) instead."),
            call. = FALSE)
    return(NA)
  }

  N <- dim(ucm)[1]
  cm <- table(ucm_2[, 1], ucm_2[, 2])
  pc <- sum(margin.table(cm, 1) * margin.table(cm, 2)) / N^2
  p0 <- sum(diag(cm)) / N

  return((p0 - pc) / (1 - pc))
}

#' Compute Fleiss' Kappa
#'
#' Computes Fleiss' Kappa.
#'
#' @param ucm Units-coders matrix
#'
#' @family intercoder reliability
icr_fleiss_kappa <- function(ucm) {

  if(any(is.na(ucm))) {
    return(NA)
  }

  vals <- unique(as.vector(ucm))
  uvm <- t(as.matrix(apply(ucm, 1, values_in_unit, vals)))

  n_u <- dim(uvm)[1]
  n_cod <- sum(uvm[1, ])

  pj <- apply(uvm, 2, sum) / (n_cod * n_u)
  pc <- sum(pj * pj)
  p0 <- icr_agreement(ucm)

  return((p0 - pc) / (1 - pc))
}

#' Compute Brennan & Prediger's Kappa
#'
#' Computes Brennan & Prediger's Kappa (extension to 3+ coders as proposed by
#' von Eye (2006).
#'
#' @param ucm Units-coders matrix
#'
#' @family intercoder reliability
#'
#' @references Brennan, R. L., & Prediger, D. J. (1981). Coefficient Kappa: Some
#'   Uses, Misuses, and Alternatives. Educational and Psychological Measurement,
#'   41(3), 687-699. https://doi.org/10.1177/001316448104100307
#'
#'   von Eye, A. (2006). An Alternative to Cohen's Kappa. European Psychologist, 11(1),
#'   12-24. https://doi.org/10.1027/1016-9040.11.1.12
icr_brennan_prediger <- function(ucm) {

  if(any(is.na(ucm))) {
    return(NA)
  }

  p0 <- icr_agreement(ucm)
  n_cat <- length(unique(as.vector(ucm)))
  n_cod <- dim(ucm)[2]
  pc <- (1 / (n_cat^(n_cod - 1)))

  return((p0 - pc) / (1 - pc))
}
