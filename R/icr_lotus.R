### Internal functions ###

## Compute mode
##
## Computes mode. If vector has more than one mode,
## the first mode by order of values is returned
##
## @param x A vector
##
## @family intercoder reliability
##
## @keywords internal
get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


## Get compare values
##
## Gets the values to compare coder values to.
## By default, the mode in each unit is returned.
## If variable level is either "ratio" or "interval",
## the mean of each unit is returned.
## If a gold standard is supplied, these values are used.
##
## @param ucm A unit-coder matrix
## @param var_level Variable level
## @param gold_standard Placeholder for gold standard
##
## @family intercoder reliability
##
## @keywords internal
gold_standards <- function(ucm, var_level, gold_standard = FALSE) {

  # Add support for gold standard

  if (var_level %in% c("interval", "ratio")) {

    apply(ucm, 1, mean)

  } else {

    # Default: return mode
    apply(ucm, 1, get_mode)

  }

}


## Compare against gold standard
##
## Compares coder values against gold standard
##
## @param coder_vals A vector with coder values
## @param gold_standards A vector with values to compare to
## @param var_level Variable level
## @param var_tol Comparison tolerance
##
## @family intercoder reliability
##
## @keywords internal
compare_coder_against_gs <- function(coder_vals, gold_standards, var_level, var_tol) {

  # Add support for metric variables with tolerance

  # Default: simple comparison
  coder_vals == gold_standards
}


## Compute Lotus per coder
##
## Computes Lotus per coder
##
## @param ucm A unit-coder matrix
## @param var_level Variable level
## @param var_tol Comparison tolerance
## @param gold_standard Placeholder for gold standard
## @param standardize A logical indicating whether S-Lotus should be computed
##
## @family intercoder reliability
##
## @keywords internal
compute_lotus <- function(ucm, var_level, var_tol, gold_standard,
                          standardize = FALSE) {

  if (missing(gold_standard)) {
    gold_standard <- 0
  }

  gs <- gold_standards(ucm, var_level)
  agreements <- apply(ucm, 2, compare_coder_against_gs, gs)
  lotus <- apply(agreements, 2, sum) / nrow(ucm)

  if (standardize) {
    standardizer <- 1 / length(unique(as.vector(ucm)))
    lotus <- (lotus - standardizer) / (1 - standardizer)
  }

  return(lotus)
}


## Compute overall Lotus
##
## Computes overall Lotus
##
## @param ucm A unit-coder matrix
## @param var_level Variable level
## @param var_tol Comparison tolerance
## @param gold_standard Placeholder for gold standard
## @param standardize A logical indicating whether S-Lotus should be computed
##
## @family intercoder reliability
##
## @keywords internal
icr_lotus <- function(ucm, var_level, var_tol, gold_standard,
                      standardize = FALSE) {

  if (missing(var_level)) {
    var_level <- "nominal"
  }

  if (missing(var_tol)) {
    var_tol <- 0
  }

  if (missing(gold_standard)) {
    gold_standard <- FALSE
  }

  mean(compute_lotus(ucm, var_level, var_tol, gold_standard, standardize))

}

