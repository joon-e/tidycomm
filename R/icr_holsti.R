#' Compute Holsti's reliability estimate
#'
#' Computes Holsti's reliability estimate (mean pairwise agreement).
#'
#' @param ucm Units-coders matrix
#'
#' @family intercoder reliability
icr_holsti <- function(ucm) {

  if(any(is.na(ucm))) {
    return(NA)
  }

  pair_agrees <- c()

  for (cols in combn(colnames(ucm), 2, simplify = FALSE)) {
    pair_agrees <- c(pair_agrees, icr_agreement(ucm[,cols]))
  }

  mean(pair_agrees)
}
