#' Compute t-tests
#'
#' Computes t-tests for one group variable and specified test variables.
#' If no variables are specified, all numeric (integer or double) variables are
#' used.
#'
#' @param data a [tibble][tibble::tibble-package]
#' @param group_var group variable (column name)
#' @param ... test variables (column names). Leave empty to compute t-tests for
#'   all numeric variables in data.
#' @param var.equal a logical variable indicating whether to treat the two
#'   variances as being equal. If `TRUE` then the pooled variance is used to
#'   estimate the variance otherwise the Welch (or Satterthwaite) approximation
#'   to the degrees of freedom is used. Defaults to `TRUE`.
#' @param paired a logical indicating whether you want a paired t-test. Defaults
#'   to `FALSE`.
#' @param pooled_sd a logical indicating whether to use the pooled standard
#'   deviation in the calculation of Cohen's d. Defaults to `TRUE`.
#' @param levels optional: a vector of length two specifying the two levels of
#'   the group variable.
#' @param case_var optional: case-identifying variable (column name). If you
#'   set `paired = TRUE`, specifying a case variable will ensure that data
#'   are properly sorted for a dependent t-test.
#'
#' @return a [tibble][tibble::tibble-package]
#'
#' @family t-test
#'
#' @examples
#' WoJ %>% t_test(temp_contract, autonomy_selection, autonomy_emphasis)
#' WoJ %>% t_test(temp_contract)
#' WoJ %>% t_test(employment, autonomy_selection, autonomy_emphasis,
#'   levels = c("Full-time", "Freelancer"))
#'
#' @export
t_test <- function(data, group_var, ...,
                   var.equal = TRUE, paired = FALSE, pooled_sd = TRUE,
                   levels = NULL, case_var = NULL) {

  # Get vars
  test_vars <- grab_vars(data, quos(...))
  test_vars_string <- purrr::map_chr(test_vars, as_label)

  # Get group var name
  group_var_str <- as_label(quo({{ group_var }}))
  if (group_var_str %in% test_vars_string) {
    test_vars <- syms(test_vars_string[test_vars_string != group_var_str])
  }

  # Drop unused levels (if data is filtered)
  data <- droplevels(data)

  if (missing(levels)) {
    # Get levels
    levels <- data %>%
      dplyr::pull({{ group_var }}) %>%
      na.omit() %>%
      unique() %>%
      as.character()

    # Check
    if (length(levels) < 2) {
      stop("Grouping variable must have more than one level", call. = FALSE)
    } else if (length(levels) > 2) {
      warning(glue("{group_var_str} has more than 2 levels, defaulting to first two ",
                         "({levels[1]} and {levels[2]}). ",
                         "Consider filtering your data ",
                         "or setting levels with the levels argument"),
              call. = FALSE)
      data <- data %>%
        dplyr::filter({{ group_var }} %in% levels[1:2]) %>%
        droplevels()
    }
  } else if (length(levels) != 2) {
    stop("If using the levels argument, please provide exactly two levels",
         call. = FALSE)
  } else if (!all(levels %in% unique(data[[group_var_str]]))) {
    stop("At least one level specified in the levels argument not found in data",
         call. = FALSE)
  }

  # Prepare data
  levels <- levels[1:2]

  if (!missing(case_var)) {
    data <- data %>%
      dplyr::arrange({{ group_var }}, {{ case_var }})
  }

  data <- dplyr::select(data, {{ group_var }}, !!!test_vars)

  # Main function
  purrr::map_dfr(test_vars, compute_t_test, data, {{ group_var }},
                 levels, var.equal, paired, pooled_sd)

}

#' Compute t-test
#'
#' Computes and outputs a t-test for one test variable
#'
#' @inheritParams t_test
#' @param test_var Test variable
#'
#' @return a [tibble][tibble::tibble-package]
#'
#' @family t-test
compute_t_test <- function(test_var, data, group_var, levels,
                           var.equal, paired, pooled_sd) {

  # Split data
  x <- data %>%
    dplyr::filter({{ group_var }} == levels[1]) %>%
    dplyr::pull({{ test_var }})
  y <- data %>%
    dplyr::filter({{ group_var }} == levels[2]) %>%
    dplyr::pull({{ test_var }})

  # Test
  tt <- t.test(x, y, var.equal = var.equal, paired = paired)

  # Get names
  level_names <- levels %>%
    stringr::str_replace_all(" ", "_") %>%
    stringr::str_sub(1, 10)
  M_str <- paste("M", level_names, sep = "_")
  SD_str <- paste("SD", level_names, sep = "_")
  test_var_str <- as_label(quo({{ test_var }}))

  tibble::tibble(
    Variable = test_var_str,
    !!M_str[1] := mean(x, na.rm = TRUE),
    !!SD_str[1] := sd(x, na.rm = TRUE),
    !!M_str[2] := mean(y, na.rm = TRUE),
    !!SD_str[2] := sd(y, na.rm = TRUE),
    Delta_M = mean(x, na.rm = TRUE) - mean(y, na.rm = TRUE),
    t = tt$statistic,
    df = tt$parameter,
    p = tt$p.value,
    d = cohens_d(x, y, pooled_sd, na.rm = TRUE)
  )
}

#' Compute Cohen's d
#'
#' Computes the effect size estimate Cohen's d for two sets of numerical values
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param y a (non-empty) numeric vector of data values.
#' @param pooled_sd a logical indicating whether to use the pooled standard
#'   deviation in the calculation of Cohen's d. Defaults to `TRUE`.
#' @param na.rm a logical value indicating whether NA values should be stripped
#'  before the computation proceeds. Defaults to `TRUE`.
#'
#' @return a `dbl`
#'
#' @family t-test
cohens_d <- function(x, y, pooled_sd = TRUE, na.rm = TRUE) {

  nx <- length(!is.na(x))
  ny <- length(!is.na(y))
  mx <- mean(x, na.rm = na.rm)
  my <- mean(y, na.rm = na.rm)
  varx <- var(x, na.rm = na.rm)
  vary <- var(y, na.rm = na.rm)

  if (pooled_sd) {
    s <- sqrt(((nx - 1) * varx + (ny - 1) * vary) / (nx + ny - 2))
  } else {
    s <- sqrt((varx + vary) / 2)
  }

  (mx - my) / s
}
