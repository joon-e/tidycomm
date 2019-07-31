#' Compute simple percent agreement
#'
#' Computes simple percent agreement for a units-coders matrix
#'
#' @param ucm Units-coders matrix
#'
#' @family intercoder reliability
icr_agreement <- function(ucm) {

  if(any(is.na(ucm))) {
    return(NA)
  }

  sum(apply(ucm, 1, check_equal)) / dim(ucm)[1]
}

#' Check if all values in a vector are the same
#'
#' Checks if all values in a vector are the same
#'
#' @param x A vector
check_equal <- function(x) {
  length(x) - 1 == sum(duplicated(x))
}

