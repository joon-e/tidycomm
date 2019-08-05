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
