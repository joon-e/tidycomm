### Internal functions ###

## Compute partial correlation coefficients for three variables
##
## Computes pairwise partial correlations (pearson, kendall, spearman) for
## all combinations, i.e. pairs, of specified variables while controlling for the third variable.
##
## @param data a [tibble][tibble::tibble-package]
## @param ... three variables to compute partial correlations for (column names).
## @param method a string indicating which partial correlation coefficient
##  is to be computed. One of "pearson" (default), "kendall", or "spearman"
##
## @return a [tibble][tibble::tibble-package]
##
## @family correlations
##
## @examples
## WoJ %>% correlate(autonomy_selection, autonomy_emphasis,
## partial = work_experience)
##
## @keywords internal
correlate_partial <- function(data, zvar, ..., method = "pearson") {
  if (!method %in% c("pearson", "kendall", "spearman")) {
    stop('Method must be one of "pearson", "kendall" or "spearman".',
         call. = FALSE)
  }

  input_vars <- enquos(...)
  input_vars_strings <- purrr::map_chr(input_vars, rlang::quo_name)
  vars <- grab_vars(data, input_vars_strings)

  # If zvar is not NULL (i.e., it's a character string)
  if (!is.null(zvar)) {
    vars <- c(vars, zvar)
  }
    # basic checks: check whether three variables have been provided
    if (length(vars) > 3) {
      stop('The computation cannot be performed due to an excessive number of variables provided. Please provide exactly three variables for a partial correlation.',
           call. = FALSE)
    }

    if (length(vars) == 0) {
      stop("No variable(s) given. Please provide exactly three variables for a partial correlation.",
           call. = FALSE)
    }

    if (length(vars) < 3) {
      stop('The computation cannot be performed because there are not enough variables provided. Please provide exactly three variables for a partial correlation.',
           call. = FALSE)
    }

  if (dplyr::is.grouped_df(data)) {
    warning("correlate(partial = TRUE) does not support grouped data. Groups will be dropped.",
            call. = FALSE)
    data <- dplyr::ungroup(data)
  }

  data <- data %>%
    dplyr::select(!!!vars) %>%
    dplyr::filter(stats::complete.cases(.))

  var_strings <- names(data)

  var_combs <- permn(var_strings)
  if (length(var_combs) < 4) {
    stop("The computation cannot be performed because there are not enough variables provided. Please provide exactly three different variables for a partial correlation.",
         call. = FALSE)
  }
  var_combs <- list(var_combs[[1]], var_combs[[2]], var_combs[[4]])
  var_comb <- var_combs[[1]]

  results <- correlation_partial_test(var_comb, data, method)

  return(results)
}


###########################################
correlation_partial_test <- function(var_comb, data, method) {
  x <- var_comb[[1]]
  y <- var_comb[[2]]
  z <- var_comb[[3]]
  xvar <- data[[x]]
  yvar <- data[[y]]
  zvar <- data[[z]]

  # basic checks
  if (any(!is.numeric(xvar), !is.numeric(yvar), !is.numeric(zvar))) {
    stop(glue::glue("The computation cannot be performed because at least one of {x}, {y}, and {z} is not numeric."),
         call. = FALSE)
    return()
  }

  cor_partial_test <- pcor.test(xvar, yvar, zvar, method = method)

  if (method == "pearson") {
    name <- "r"
  } else if (method == "kendall") {
    name <- "tau"
  } else if (method == "spearman") {
    name <- "rho"
  }

  tibble <- tibble(
    x = x,
    y = y,
    z = z,
    !!name := cor_partial_test$estimate,
    df = cor_partial_test$n - 2 - 1, # df formula: n - 2 - k,
    # where k == number of vars we are conditioning on
    p = ifelse(is.null(cor_partial_test$p.value),
               NA, cor_partial_test$p.value),
    n = cor_partial_test$n
  )

  return(tibble)
}

###########################################
# The 'permn' function was originally written by Scott D. Chasalow and is based on
# algorithmic ideas presented in the book "Combinatorial Algorithms: Theory and Practice"
# by Reingold, E.M., Nievergelt, J., and Deo, N. (1977, Prentice-Hall).
# Source: https://github.com/cran/combinat/blob/master/R/permn.R
# Author: Scott D. Chasalow
# Reference: Reingold, E.M., Nievergelt, J., Deo, N. (1977) Combinatorial
# Algorithms: Theory and Practice. NJ: Prentice-Hall. pg. 170.
permn <- function(x, fun = NULL, ...)
  {
    # DATE WRITTEN: 23 Dec 1997          LAST REVISED:  23 Dec 1997
    # AUTHOR:  Scott D. Chasalow (Scott.Chasalow@users.pv.wau.nl)
    #
    # DESCRIPTION:
    #             Generates all permutations of the elements of x, in a minimal-
    #	change order. If x is a	positive integer,  returns all permutations
    #	of the elements of seq(x). If argument "fun" is not null,  applies
    #	a function given by the argument to each point. "..." are passed
    #	unchanged to the function given by argument fun, if any.
    #
    #	Returns a list; each component is either a permutation, or the
    #	results of applying fun to a permutation.
    #
    if(is.numeric(x) && length(x) == 1 && x > 0 && trunc(x) == x) x <- seq(
      x)
    n <- length(x)
    nofun <- is.null(fun)
    out <- vector("list", gamma(n + 1))
    p <- ip <- seqn <- 1:n
    d <- rep(-1, n)
    d[1] <- 0
    m <- n + 1
    p <- c(m, p, m)
    i <- 1
    use <-  - c(1, n + 2)
    while(m != 1) {
      out[[i]] <- if(nofun) x[p[use]] else fun(x[p[use]], ...)
      i <- i + 1
      m <- n
      chk <- (p[ip + d + 1] > seqn)
      m <- max(seqn[!chk])
      if(m < n)
        d[(m + 1):n] <-  - d[(m + 1):n]
      index1 <- ip[m] + 1
      index2 <- p[index1] <- p[index1 + d[m]]
      p[index1 + d[m]] <- m
      tmp <- ip[index2]
      ip[index2] <- ip[m]
      ip[m] <- tmp
    }
    out
  }


###########################################
# These function pcor and pcor.test were originally written by Seongho Kim
# <biostatistician.kim@gmail.com> under GPL-2 licence.
# Source: https://rdrr.io/cran/ppcor/
# References: Kim, S. (2015) ppcor: An R Package for a Fast Calculation to Semi-partial Correlation Coefficients.
# Communications for Statistical Applications and Methods, 22(6), 665-674.
# Changes made to the original code:
# - Return Type: The pcor and the pcor.test function's output has been changed from a list to a tibble.
# - Data Extraction: Specific indices are now used in pcor to construct the tibble, focusing on key results..
# - Formatting: Very minor adjustments for improved readability and maintainability.
pcor <- function(x, method = c("pearson", "kendall", "spearman"))
{
  # correlation method
  method <- match.arg(method)

  # check the data
  if (is.data.frame(x))
    x <- as.matrix(x)
  if (!is.matrix(x))
    stop("supply a matrix-like 'x'")
  if (!(is.numeric(x) || is.logical(x)))
    stop("'x' must be numeric")
  stopifnot(is.atomic(x))

  # sample number
  n <- dim(x)[1]

  # given variables' number
  gp <- dim(x)[2]-2

  # covariance matrix
  cvx <- stats::cov(x,method=method)

  # inverse covariance matrix
  if(det(cvx) < .Machine$double.eps){
    warning("The inverse of variance-covariance matrix is calculated using Moore-Penrose generalized matrix invers due to its determinant of zero.")
    icvx <- MASS::ginv(cvx)
  }else
    icvx <- solve(cvx)

  # partial correlation
  pcor <- -stats::cov2cor(icvx)
  diag(pcor) <- 1

  # p-value
  if(method == "kendall"){
    statistic <- pcor/sqrt(2*(2*(n-gp)+5)/(9*(n-gp)*(n-1-gp)))
    p.value <- 2*stats::pnorm(-abs(statistic))

  }else{
    statistic <- pcor*sqrt((n-2-gp)/(1-pcor^2))
    p.value <- 2*stats::pt(-abs(statistic),(n-2-gp))
    #p.value <- 2*pnorm(-abs(statistic))
  }

  diag(statistic) <- 0
  diag(p.value) <- 0

  tibble <- tibble(
    estimate = pcor[2,1],
    p.value = p.value[2,1],
    statistic = statistic[2,1],
    n = n,
    gp = gp,
    Method = method
  )

  return(tibble)
}

pcor.test <- function(x,y,z,method=c("pearson", "kendall", "spearman"))
{
  # The partial correlation coefficient between x and y given z
  #
  # pcor.test is free and comes with ABSOLUTELY NO WARRANTY.
  #
  # x and y should be vectors
  #
  # z can be either a vector or a matrix

  # correlation method
  method <- match.arg(method)

  x <- c(x)
  y <- c(y)
  z <- as.data.frame(z)

  # merge into a matrix
  xyz <- data.frame(x,y,z)

  # partial correlation
  pcor <- pcor(xyz,method=method)

  return(pcor)
  }
