#' Perform an intercoder reliability test
#'
#' Performs an intercoder reliability test by computing various intercoder
#' reliability estimates for the included variables
#'
#' @param data a [tibble][tibble::tibble-package] or a [tdcmm] model
#' @param unit_var Variable with unit identifiers
#' @param coder_var Variable with coder identifiers
#' @param ... Variables to compute intercoder reliability estimates for. Leave
#'   empty to compute for all variables (excluding `unit_var` and `coder_var``)
#'   in data.
#' @param levels Optional named vector with levels of test variables
#' @param na.omit Logical indicating whether `NA` values should be stripped
#'   before computation. Defaults to `FALSE`.
#' @param agreement Logical indicating whether simple percent agreement should
#'   be computed. Defaults to `TRUE`.
#' @param holsti Logical indicating whether Holsti's reliability estimate
#'   (mean pairwise agreement) should be computed. Defaults to `TRUE`.
#' @param kripp_alpha Logical indicating whether Krippendorff's Alpha should
#'   be computed. Defaults to `TRUE`.
#' @param cohens_kappa Logical indicating whether Cohen's Kappa should
#'   be computed. Defaults to `FALSE`.
#' @param fleiss_kappa Logical indicating whether Fleiss' Kappa should
#'   be computed. Defaults to `FALSE`.
#' @param brennan_prediger Logical indicating whether Brennan & Prediger's Kappa
#'   should be computed (extension to 3+ coders as proposed by von Eye (2006)).
#'   Defaults to `FALSE`.
#' @param lotus Logical indicating whether Fretwurst's Lotus should be
#'   computed. Defaults to `FALSE`
#' @param s_lotus Logical indicating whether Fretwurst's standardized Lotus
#'   (S-Lotus) should be computed. Defaults to `FALSE`.
#' @param check_disagreements Logical, indicates whether to check for intercoder disagreements.
#'   This must be set to TRUE to use the `text_var` parameter, as `text_var` is used specifically
#'   for evaluating reasons behind intercoder disagreements. Defaults to `FALSE`.
#'   This is mutually exclusive with `check_pairs`.
#' @param text_var Optional, specifies the name of the text variable that contains annotated data.
#'   This variable is used to facilitate the analysis of reasons for intercoder disagreements.
#'   This parameter should only be provided if `check_disagreements` is `TRUE`. Defaults to `NULL`.
#' @param check_pairs Logical, indicates whether to compute and compare intercoder reliability
#'   for each pair of coders. Defaults to `FALSE`. This is mutually exclusive with `check_disagreements`.
#'
#'
#' @return a [tdcmm] model
#'
#' @examples
#' fbposts %>% test_icr(post_id, coder_id, pop_elite, pop_othering)
#' fbposts %>% test_icr(post_id, coder_id, levels = c(n_pictures = "ordinal"), fleiss_kappa = TRUE)
#'
#' @family intercoder reliability
#'
#' @export
#'
#' @references Brennan, R. L., & Prediger, D. J. (1981). Coefficient Kappa: Some
#'   uses, misuses, and alternatives. Educational and Psychological Measurement,
#'   41(3), 687-699. https://doi.org/10.1177/001316448104100307
#'
#'   Cohen, J. (1960). A coefficient of agreement for nominal scales.
#'   Educational and Psychological Measurement, 20(1), 37-46.
#'   https://doi.org/10.1177/001316446002000104
#'
#'   Fleiss, J. L. (1971). Measuring nominal scale agreement among many raters.
#'   Psychological Bulletin, 76(5), 378-382. https://doi.org/10.1037/h0031619
#'
#'   Fretwurst, B. (2015). Reliabilität und Validität von Inhaltsanalysen.
#'   Mit Erläuterungen zur Berechnung des Reliabilitätskoeffizienten „Lotus“ mit SPSS.
#'   In W. Wirth, K. Sommer, M. Wettstein, & J. Matthes (Ed.),
#'   Qualitätskriterien in der Inhaltsanalyse (S. 176–203). Herbert von Halem.
#'
#'   Krippendorff, K. (2011). Computing Krippendorff's Alpha-Reliability.
#'   Retrieved from http://repository.upenn.edu/asc_papers/43
#'
#'   von Eye, A. (2006). An Alternative to Cohen's Kappa. European Psychologist, 11(1),
#'   12-24. https://doi.org/10.1027/1016-9040.11.1.12
test_icr <- function(data, unit_var, coder_var, ..., levels = NULL, na.omit = FALSE,
                     agreement = TRUE, holsti = TRUE, kripp_alpha = TRUE,
                     cohens_kappa = FALSE, fleiss_kappa = FALSE, brennan_prediger = FALSE,
                     lotus = FALSE, s_lotus = FALSE, check_disagreements = FALSE, text_var = NULL,
                     check_pairs = FALSE) {

  # Check if unit_var and coder_var are provided
  if (missing(unit_var) | missing(coder_var)) {
    stop("Please provide both a variable with unit identifiers and a variable with coder identifiers.")
  }

  # Ensure that check_pairs and check_disagreements are not both TRUE
  if (check_pairs && check_disagreements) {
    stop("The parameters 'check_pairs' and 'check_disagreements' are mutually exclusive. Please choose only one.")
  }

  exclude_vars <- c(as_label(expr({{ unit_var }})), as_label(expr({{ coder_var }})))
  test_vars <- grab_vars(data, enquos(...), alternative = "all", exclude_vars = exclude_vars)
  test_vars_str <- purrr::map_chr(test_vars, as_label)
  text_var_arg <- enquo(text_var)

  # Check if the text_var parameter is provided and correctly specified
  if (!rlang::quo_is_null(text_var_arg)) {
    if (!rlang::quo_is_null(text_var_arg) && isFALSE(check_disagreements)) {
      stop("Please ensure 'check_disagreements' is set to TRUE when using 'text_var' for evaluating intercoder disagreements.", call. = FALSE)
    }
    else {
      text_var <- rlang::quo_name(text_var_arg)
      if (!rlang::quo_is_null(text_var_arg) && !(text_var %in% names(data))) {
        stop("Specified text variable ('text_var') not found in the data. Please check its spelling.", call. = FALSE)
      }
    }
  }

  # Check if disagreements parameter is set to TRUE and calculate disagreements
  if (check_disagreements == TRUE) {
    disagreements_tbl <- purrr::map_dfr(test_vars, function(var) {
      compute_disagreements(data, {{ unit_var }}, {{ coder_var }}, {{ var }}, {{ text_var }})
    })
  }

  # If the data is grouped, use group_map function
  if (dplyr::is.grouped_df(data)) {
    out <- data %>% dplyr::group_map(.f = function(.x, .y) {
      tmp_out <- purrr::map_dfr(test_vars, compute_icr, .x, {{ unit_var }}, {{ coder_var }},
                                levels, na.omit,
                                agreement, holsti, kripp_alpha, cohens_kappa, fleiss_kappa, brennan_prediger,
                                lotus, s_lotus, check_pairs)
      # Add the group variable to the resulting data frames
      dplyr::mutate(tmp_out, group = .y[[1]])
    })
    # Bind all data frames together and reorder resulting data frame
    out <- dplyr::bind_rows(out) %>%
      dplyr::select(group, tidyselect::everything())
  } else {
    out <- purrr::map_dfr(test_vars, compute_icr, data, {{ unit_var }}, {{ coder_var }},
                          levels, na.omit,
                          agreement, holsti, kripp_alpha, cohens_kappa, fleiss_kappa, brennan_prediger,
                          lotus, s_lotus, check_pairs)

    if (check_disagreements) {
      out <- disagreements_tbl
    }
  }

  result <- new_tdcmm(
    out,
    func = "test_icr",
    data = data,
    params = list(
      unit_var = as_name(enquo(unit_var)),
      coder_var = as_name(enquo(coder_var)),
      vars = test_vars_str,
      levels = levels,
      na.omit = na.omit,
      agreement = agreement,
      holsti = holsti,
      kripp_alpha = kripp_alpha,
      cohens_kappa = cohens_kappa,
      fleiss_kappa = fleiss_kappa,
      brennan_prediger = brennan_prediger,
      lotus = lotus,
      s_lotus = s_lotus,
      check_disagreements = check_disagreements,
      check_pairs = check_pairs,
      text_var = if (!is.null(text_var)) quo_name(enquo(text_var)) else NULL
    )
  )

  return(result)
}


### Internal functions ###

## Compute intercoder reliability estimates for one test variable
##
## Computes intercoder reliability estimates for one test variable
##
## @param test_var Variable to compute estimates for
## @inheritParams test_icr
##
## @family intercoder reliability
##
## @keywords internal
compute_icr <- function(test_var, data, unit_var, coder_var,
                        levels = c(), na.omit = FALSE,
                        agreement = TRUE, holsti = TRUE, kripp_alpha = TRUE,
                        cohens_kappa = FALSE, fleiss_kappa = FALSE, brennan_prediger = FALSE,
                        lotus = FALSE, s_lotus = FALSE, check_pairs = FALSE) {

  ucm <- unit_coder_matrix(data, {{ unit_var }}, {{ coder_var }}, {{ test_var }})

  if (length(na.omit(ucm)) == 0) {
    stop("Empty units-coders matrix detected. ",
         "This is most likely due to none of the units having been coded by all coders. ",
         "See vignette('v04_icr') for details.", call. = FALSE)
  }

  # Extract variable level or default to "nominal"
  var_string <- as_name(enquo(test_var))
  var_level <- ifelse(hasName(levels, var_string) && levels[[var_string]] %in% c("nominal", "ordinal", "interval", "ratio"),
                      levels[[var_string]], "nominal")

  # Check for missing values
  if (any(is.na(ucm))) {
    if (na.omit) {
      ucm <- na.omit(ucm)
    } else {
      warning(glue("Variable '{var_string}' contains missing values.",
                   "Consider setting na.omit = TRUE or recoding missing values",
                   .sep = " "),
              call. = FALSE)
    }
  }

  calculate_reliability <- function(ucm, var_level) {
    test_vals <- tibble::tibble(
      Variable = var_string,
      n_Units = dim(ucm)[1],
      n_Coders = dim(ucm)[2],
      n_Categories = length(unique(na.omit(as.vector(ucm)))),
      Level = var_level
    )

    # Add metrics conditionally
    reliabilities <- list()
    if (agreement) reliabilities$Agreement <- icr_agreement(ucm)
    if (holsti) reliabilities$Holstis_CR <- icr_holstis_CR(ucm)
    if (kripp_alpha) reliabilities$Krippendorffs_Alpha <- icr_kripp_alpha(ucm, var_level)
    if (cohens_kappa) reliabilities$Cohens_Kappa <- icr_cohens_kappa(ucm)
    if (fleiss_kappa) reliabilities$Fleiss_Kappa <- icr_fleiss_kappa(ucm)
    if (brennan_prediger) reliabilities$Brennan_Predigers_Kappa <- icr_brennan_prediger(ucm)
    if (lotus) reliabilities$Lotus <- icr_lotus(ucm)
    if (s_lotus) reliabilities$S_Lotus <- icr_lotus(ucm, standardize = TRUE)

    test_vals <- dplyr::bind_cols(test_vals, reliabilities)
    return(test_vals)
  }

  if (check_pairs) {
    # Get all unique coder pairs and calculate reliability for each
    coder_pairs <- combn(colnames(ucm), 2, simplify = FALSE)
    pair_results <- lapply(coder_pairs, function(pair) {
      ucm_pair <- ucm[, pair, drop = FALSE]
      test_vals <- calculate_reliability(ucm_pair, var_level)
      test_vals$Coder_Pair <- paste(pair, collapse = " & ")
      test_vals
    })
    test_vals <- dplyr::bind_rows(pair_results) %>%
      dplyr::relocate(Coder_Pair, .after = n_Coders)
  } else {
    # Calculate reliability for the entire matrix
    test_vals <- calculate_reliability(ucm, var_level)
  }
  return(test_vals)
}

## Generate units-coders matrix
##
## Generates a units-coders matrix for a test variable
##
## @inheritParams compute_icr
##
## @family intercoder reliability
##
## @keywords internal
## Generate units-coders matrix
##
## Generates a units-coders matrix for a test variable
##
## @inheritParams compute_icr
##
## @family intercoder reliability
##
## @keywords internal
unit_coder_matrix <- function(data, unit_var, coder_var, test_var) {

  data %>%
    dplyr::select({{ unit_var }}, {{ coder_var }}, {{ test_var }}) %>%
    tidyr::spread({{ coder_var }}, {{ test_var }}) %>%
    dplyr::select(-1) %>%
    as.matrix()
}


## Compute disagreements among coders for one test variable
##
## Computes and returns disagreements for one test variable
##
## @param test_var Variable to compute disagreements for
## @param data tibble containing the observations
## @param unit_var unit identifier variable
## @param coder_var coder identifier variable
## @param text_var text variable for detailed disagreement context (optional)
## @inheritParams test_icr
##
## @family intercoder reliability
##
## @keywords internal
compute_disagreements <- function(data, unit_var, coder_var, test_var, text_var = NULL) {

  # Check if the text_var parameter is provided
  if (!rlang::quo_is_null(expr({{ text_var }}))) {
    text_var <- rlang::quo_name(expr({{ text_var }}))
  }

  # Select data
  data_selection <- data %>%
    dplyr::select({{ unit_var }}, {{ coder_var }}, {{ test_var }})

  # Pivot data to a wide format focusing on test_var
  wide_data <- tidyr::pivot_wider(data_selection, names_from = {{ coder_var }}, values_from = {{ test_var }})

  # Detect disagreements
  wide_data$disagreement <- apply(wide_data[,-1], 1, function(x) length(unique(na.omit(x))) > 1)
  disagreements_tbl <- wide_data %>%
    dplyr::filter(disagreement == TRUE) %>%
    dplyr::select(-disagreement)

  # Rename coder columns to include the variable name
  coder_columns <- setdiff(colnames(disagreements_tbl), as_label(expr({{ unit_var }})))
  colnames(disagreements_tbl)[-1] <- paste(as_label(expr({{ test_var }})), coder_columns, sep = "_")

  if (!rlang::quo_is_null(expr({{ text_var }}))) {
    text_data <- data %>%
      dplyr::select({{ unit_var }}, {{ text_var }})

    disagreements_tbl <- dplyr::left_join(disagreements_tbl, text_data, by = as_label(expr({{ unit_var }}))) %>%
      dplyr::relocate({{ text_var }}, .after = {{ unit_var }}) %>%
      dplyr::distinct()

    return(disagreements_tbl)
  }
  return(disagreements_tbl)
}
