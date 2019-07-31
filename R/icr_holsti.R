#' Compute Holsti's reliability estimate
#'
#' Computes Holsti's reliability estimate (mean pairwise agreement).
#'
#' @param ucm Units-coders matrix
icr_holsti <- function(ucm) {
  pair_agrees <- c()

  for (cols in combn(colnames(ucm), 2, simplify = FALSE)) {
    pair_agrees <- c(pair_agrees, icr_agreement(ucm[,cols]))
  }

  mean(pair_agrees)
}
