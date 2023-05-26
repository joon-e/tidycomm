### Internal functions ###

## Compute partial correlation coefficients for three variables
##
## Computes pairwise partial correlations (pearson, kendall, spearman) for
## all combinations, i.e. pairs, of specified variables while controlling for the third variable.
##
## @param data a [tibble][tibble::tibble-package]
## @param ... three variables to compute partial correlations for (column names).
## @param method a character string indicating which partial correlation coefficient
##  is to be computed. One of "pearson" (default), "kendall", or "spearman"
##
## @return a [tibble][tibble::tibble-package]
##
## @family correlations
##
## @examples
## WoJ %>% correlate_partial(autonomy_selection, autonomy_emphasis, work_experience)
##
## @keywords internal
correlate_partial <- function(data, ..., method = "pearson") {

  if (!method %in% c("pearson", "kendall", "spearman")) {
    stop('Method must be one of "pearson", "kendall" or "spearman".',
         call. = FALSE)
  }

  vars <- grab_vars(data, enquos(...), alternative = "none")

  # basic checks: check whether three variables have been provided
  if (length(vars) > 3) {
    stop('The computation cannot be performed due to an excessive number of variables provided. Please provide exactly three variables for a partial correlation.',
         call. = FALSE)
  }

  if (length(vars) == 0) {
    stop("No variable(s) given. Please provide exactly three variables for a partial correlation.")
  }

  if (length(vars) < 3) {
    stop('The computation cannot be performed because there are not enough variables provided. Please provide exactly three variables for a partial correlation.',
         call. = FALSE)
  }

  # basic checks: check whether data is grouped
  if (dplyr::is.grouped_df(data)) {
    warning("correlate(partial = TRUE) does not support grouped data. Groups will be dropped.",
            call. = FALSE)
    data <- dplyr::ungroup(data)
  }

  # ensure completeness
  data_rows <- nrow(data)
  data <- data %>%
    dplyr::select(!!!vars) %>%
    dplyr::filter(stats::complete.cases(.))
  delta <- data_rows - nrow(data)
  if (delta > 0) {
    warning(glue("Only complete cases without any missing values allowed. {delta} cases were removed."),
            call. = FALSE)
  }

  var_strings <- data %>%
    dplyr::select(!!!vars) %>%
    names()
  var_combs <- combinat::permn(var_strings)
  var_combs <- list(var_combs[[1]], var_combs[[2]], var_combs[[4]])
  purrr::map_dfr(var_combs, correlation_partial_test, data, method)
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

  cor_partial_test <- ppcor::pcor.test(xvar, yvar, zvar, method = method)

  if (method == "pearson") {
    name <- "r"
  } else if (method == "kendall") {
    name <- "tau"
  } else if (method == "spearman") {
    name <- "rho"
  }

  tibble(
    x = x,
    y = y,
    z = z,
    !!name := cor_partial_test$estimate,
    df = cor_partial_test$n - 2 - 1, # df formula: n - 2 - k,
    # where k == number of vars we are conditioning on
    p = ifelse(is.null(cor_partial_test$p.value),
               NA, cor_partial_test$p.value)
  )
}
