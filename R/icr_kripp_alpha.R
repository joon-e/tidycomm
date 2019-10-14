### Internal functions ###

## Compute Krippendorff's Alpha
##
## Computers Krippendorff's Alpha intercoder reliability estimate as detailed
## in Krippendorff (2011), section E.
##
## @param ucm A units-coders matrix
## @param var_level Variable level
##
## @references Krippendorff, K. (2011). Computing Krippendorff's Alpha-Reliability.
##   Retrieved from http://repository.upenn.edu/asc_papers/43
##
## @family intercoder reliability
##
## @keywords internal
icr_kripp_alpha <- function(ucm, var_level = NULL) {

  if (missing(var_level)) {
    var_level = "nominal"
  }

  # Transponse to Coder-Unit-Matrix
  cum <- t(ucm)

  # Get all unique values
  vals <- unique(na.omit(as.vector(cum)))

  if (length(vals) == 1) {
    return(1)
  }

  if (!is.numeric(vals) & var_level != "nominal") {
    stop("Non-numeric variables must use variable level 'nominal'", call. = FALSE)
  }

  if (is.numeric(vals)) {
    vals <- sort(vals)
  }
  numvals <- seq(1, length(vals))

  # Generate values-units-matrix
  vum <- as.matrix(apply(cum, 2, values_in_unit, vals))

  # Delete non-pairable columns
  vum <- vum[, apply(vum, 2, sum) > 1]

  # Calculate alpha
  num <- sum(apply(vum, 2, kalpha_num_values, numvals, vum, var_level))
  denom <- kalpha_denom(vum, numvals, var_level)
  N <- sum(apply(vum, 2, sum))

  1 - ((N - 1) * (num / denom))

}

## Count specific value in unit
##
## Counts occurrences of a specific value in a single unit of a units-coders matrix.
##
## @param val Value to count
## @param tab_u A frequency table of values in the unit
##
## @keywords internal
count_value_in_unit <- function(val, tab_u) {
  if (as.character(val) %in% names(tab_u)) {
    return(tab_u[[as.character(val)]])
  }
  return(0)
}

## Count values in unit
##
## Counts occurrences of values in a single unit of a units-coders matrix.
##
## @param u Unit of a unit-coders matrix
## @param vals Values to count
##
## @keywords internal
values_in_unit <- function(u, vals) {
  tab_u <- table(u)
  purrr::as_vector(purrr::map(vals, count_value_in_unit, tab_u))
}

## Compute value of difference function
##
## Computes value of the difference function
##
## @param c Value c
## @param k Value k
## @param vum A values-units matrix
## @param var_level Variable level
##
## @references Krippendorff, K. (2011). Computing Krippendorff's Alpha-Reliability.
##   Retrieved from http://repository.upenn.edu/asc_papers/43
##
## @keywords internal
delta_sq <- function(c, k, vum, var_level) {
  if (var_level == "nominal") {
    if (c == k) {
      return(0)
    }
    return(1)
  }

  if (var_level == "ordinal") {
    if (c == k) {
      return(0)
    }
    g = vum[c:k, ]
    ng = sum(apply(g, 1, sum))
    nc = sum(vum[c, ])
    nk = sum(vum[k, ])
    d <- ng - ((nc + nk) / 2)
    return(d^2)
  }

  if (var_level == "interval") {
    d <- c-k
    return(d^2)
  }

  if (var_level == "ratio") {
    d <- (c-k) / (c+k)
    return(d^2)
  }
}

## Compute Krippendorff's Alpha numerator values
##
## Computes the values in the numerator in Krippendorff's Alpha formula
##
## @param u Unit of a values-units matrix
## @param numvals Amount of unique values in codings (categories)
## @param vum Values-units matrix
## @param var_level Variable level
##
## @keywords internal
##
## @references Krippendorff, K. (2011). Computing Krippendorff's Alpha-Reliability.
##   Retrieved from http://repository.upenn.edu/asc_papers/43
kalpha_num_values <- function(u, numvals, vum, var_level) {

  o <- c()
  nu <- sum(u)

  if (nu > 1) {
    for (c in numvals) {
      for (k in numvals) {
        if (k > c) {
          o <- c(o, (u[c] * u[k] * delta_sq(c, k, vum, var_level)))
        }
      }
    }
    return(sum(o) / (nu - 1))
  }
  return(0)
}

## Compute Krippendorff's Alpha denominator
##
## Computes the denominator in Krippendorff's Alpha formula
##
## @param vum Values-units matrix
## @param numvals Amount of unique values in codings (categories)
## @param var_level Variable level
##
## @keywords internal
##
## @references Krippendorff, K. (2011). Computing Krippendorff's Alpha-Reliability.
##   Retrieved from http://repository.upenn.edu/asc_papers/43
kalpha_denom <- function(vum, numvals, var_level) {

  e <- c()

  for (c in numvals) {
    for (k in numvals) {
      if (k > c) {
        n_c <- sum(vum[c,], na.rm = TRUE)
        n_k <- sum(vum[k,], na.rm = TRUE)
        e <- c(e, n_c * n_k * delta_sq(c, k, vum, var_level))
      }
    }
  }
  return(sum(e))
}
