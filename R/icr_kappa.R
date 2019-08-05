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
