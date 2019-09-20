### Internal functions ###

# Functions for various Kappa coefficients of the formula (p0 - pe) / (1 - pe),
# where p0 is the overall agreement between the coders/raters, and pe is the
# chance agreement.
#
# n = Number of items/subjects coded
# r = Number of coders/raters
# q = Number of categories

## Compute Cohen's Kappa
##
## Computes Cohen's Kappa.
##
## @param ucm Units-coders matrix
##
## @family intercoder reliability
##
## @keywords internal
##
## @references Cohen, J. (1960). A coefficient of agreement for nominal scales.
##   Educational and Psychological Measurement, 20(1), 37-46.
##   https://doi.org/10.1177/001316446002000104
icr_cohens_kappa <- function(ucm) {

  if (any(is.na(ucm))) {
    return(NA)
  }

  if (dim(ucm)[2] > 2) {
    warning(glue("Cohen's Kappa is only available for two coders. ",
                       "Try Fleiss' Kappa (fleiss_kappa = TRUE) instead."),
            call. = FALSE)
    return(NA)
  }

  n <- dim(ucm)[1]

  vals <- unique(as.vector(ucm))
  q <- length(vals)

  cm <- matrix(rep(0, q * q), ncol = q)

  for (i in 1:q) {
    for (j in 1:q) {
      cm[i, j] <- sum(ucm[, 1] == vals[i] & ucm[, 2] == vals[j])
    }
  }

  pcm <- cm / n
  p0 <- sum(diag(pcm))
  pe <- sum(margin.table(pcm, 1) * margin.table(pcm, 2))

  # Output
  (p0 - pe) / (1 - pe)
}

## Compute Fleiss' Kappa
##
## Computes Fleiss' Kappa.
##
## @param ucm Units-coders matrix
##
## @family intercoder reliability
##
## @keywords internal
##
## @references Fleiss, J. L. (1971). Measuring nominal scale agreement among many raters.
##   Psychological Bulletin, 76(5), 378-382. https://doi.org/10.1037/h0031619

icr_fleiss_kappa <- function(ucm) {

  if (any(is.na(ucm))) {
    return(NA)
  }

  # Overall agreement
  p0 <- icr_holstis_CR(ucm)

  # Chance agreement
  n <- dim(ucm)[1]
  r <- dim(ucm)[2]

  vals <- unique(as.vector(ucm))
  q <- length(vals)

  pj <- c()

  for (j in 1:q) {
    rij <- c()
    for (i in 1:n) {
      rij <- c(rij, sum(ucm[i, ] == vals[j]))
    }
    pj <- c(pj, sum(rij) / (n * r))
  }

  pe <- sum(pj^2)

  # Output
  (p0 - pe) / (1 - pe)
}

## Compute Brennan & Prediger's Kappa
##
## Computes Brennan & Prediger's Kappa (extension to 3+ coders as proposed by
## von Eye (2006).
##
## @param ucm Units-coders matrix
##
## @family intercoder reliability
##
## @keywords internal
##
## @references Brennan, R. L., & Prediger, D. J. (1981). Coefficient Kappa: Some
##   uses, misuses, and alternatives. Educational and Psychological Measurement,
##   41(3), 687-699. https://doi.org/10.1177/001316448104100307
##
##   von Eye, A. (2006). An Alternative to Cohen's Kappa. European Psychologist, 11(1),
##   12-24. https://doi.org/10.1027/1016-9040.11.1.12
icr_brennan_prediger <- function(ucm) {

  if (any(is.na(ucm))) {
    return(NA)
  }

  # Overall agreement
  p0 <- icr_agreement(ucm)

  # Chance agreement
  q <- length(unique(as.vector(ucm)))
  r <- dim(ucm)[2]
  pe <- (1 / (q^(r - 1)))

  # Output
  (p0 - pe) / (1 - pe)
}
