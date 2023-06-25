#' Compute t-tests
#'
#' Computes t-tests for one group variable and specified test variables.
#' If no variables are specified, all numeric (integer or double) variables are
#' used.
#'
#' @param data a [tibble][tibble::tibble-package] or a [tdcmm] model
#' @param group_var group variable (column name) to specify where to split two
#'   samples (two-sample t-test) or which variable to compare a one-sample
#'   t-test on
#' @param ... test variables (column names). Leave empty to compute t-tests for
#'   all numeric variables in data. Also leave empty for one-sample t-tests.
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
#' @param mu optional: a number indicating the *true* value of the mean in the
#'   general population (\eqn{\mu}). If set, a one-sample t-test (i.e., a
#'   location test) is being calculated. Leave to `NULL` to calculate
#'   two-sample t-test(s).
#'
#' @return a [tdcmm] model
#'
#' @family t-test
#'
#' @examples
#' WoJ %>% t_test(temp_contract, autonomy_selection, autonomy_emphasis)
#' WoJ %>% t_test(temp_contract)
#' WoJ %>% t_test(employment, autonomy_selection, autonomy_emphasis,
#'   levels = c("Full-time", "Freelancer"))
#' WoJ %>% t_test(autonomy_selection, mu = 3.62)
#'
#' @export
t_test <- function(data, group_var, ...,
                   var.equal = TRUE, paired = FALSE, pooled_sd = TRUE,
                   levels = NULL, case_var = NULL, mu = NULL) {

  # Get group var name
  group_var_str <- as_label(quo({{ group_var }}))

  # Drop unused levels (if data is filtered)
  data <- droplevels(data)

  # run one-sample t-test if requested
  if (!is.null(mu)) {
    # Get vars
    test_vars <- grab_vars(data, quos(...), alternative = "none")

    if (length(test_vars) > 0) {
      test_vars_string <- purrr::map_chr(test_vars, as_label)
      stop(glue("If using the mu argument, a one-sample t-test is being ",
                "calculated. This cannot interfere with a two-sample t-test. ",
                "Please omit any test variables (i.e., ",
                paste(test_vars_string, collapse = ", "),
                ")."),
           call. = FALSE)
    } else {
      return(one_sample_t_test(data, {{ group_var }}, group_var_str, mu))
    }
  }

  # Get vars
  test_vars <- grab_vars(data, quos(...))
  test_vars_string <- purrr::map_chr(test_vars, as_label)

  # Filter group var if necessary
  if (group_var_str %in% test_vars_string) {
    test_vars <- syms(test_vars_string[test_vars_string != group_var_str])
  }

  # if not one-sample, run two-sample t-test
  return(two_sample_t_test(data,
                           {{ group_var }}, group_var_str,
                           {{ test_vars }}, test_vars_string,
                           var.equal, paired, pooled_sd, levels, case_var))
}

#' @rdname visualize
#' @export
visualize.tdcmm_ttst <- function(x, ..., .design = design_lmu()) {
  if (attr(x, "func") == "t_test") {
    return(visualize_t_test(x, .design))
  }

  return(warn_about_missing_visualization(x))
}

### Internal functions ###

## Run one-sample t-test
##
## Run actual one-sample or location t-test as specified
##
## @inheritParams t_test
## @param group_var_str Stringified version of group variable
##
## @return a [tdcmm] model
##
## @family t-test
##
## @keywords internal
one_sample_t_test <- function(data, group_var, group_var_str, mu) {
  # Prepare data
  data_prepared <- data %>%
    dplyr::pull({{ group_var }})

  if (!is.numeric(data_prepared)) {
    stop(glue("Within a one-sample t-test, {group_var_str} must be numeric."),
         call. = FALSE)
  }

  # Compute and Create output
  tt <- t.test(data_prepared, mu = mu)
  out <- tibble::tibble(
    Variable = group_var_str,
    M = mean(data_prepared, na.rm = TRUE),
    SD = sd(data_prepared, na.rm = TRUE),
    CI_95_LL = tt$conf.int[[1]],
    CI_95_UL = tt$conf.int[[2]],
    Mu = mu,
    t = tt$statistic,
    df = tt$parameter,
    p = tt$p.value
  )

  # Output
  return(new_tdcmm_ttst(
    new_tdcmm(out,
              func = "t_test",
              data = data,
              params = list(group_var = group_var_str,
                            mu = mu),
              model = list(tt)))
  )
}

## Run two-sample t-test
##
## Run actual two-sample t-test(s) as specified
##
## @inheritParams t_test
## @param group_var_str Stringified version of group variable
## @param test_vars Test variables
## @param test_vars_str Stringified version of test variables
##
## @return a [tdcmm] model
##
## @family t-test
##
## @keywords internal
two_sample_t_test <- function(data, group_var, group_var_str, test_vars,
                              test_vars_str,
                              var.equal, paired, pooled_sd, levels, case_var) {
  if (is.null(levels)) {
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
  model_list <- list()
  out <- NULL
  for (test_var in test_vars) {

    # Split data
    x <- data %>%
      dplyr::filter({{ group_var }} == levels[1]) %>%
      dplyr::pull({{ test_var }})
    y <- data %>%
      dplyr::filter({{ group_var }} == levels[2]) %>%
      dplyr::pull({{ test_var }})

    # Compute and Create output
    tt <- t.test(x, y, var.equal = var.equal, paired = paired)
    tt_row <- format_t_test({{ test_var }}, tt, x, y, levels, pooled_sd)

    # collect
    model_list[[length(model_list) + 1]] <- tt
    out <- out %>%
      dplyr::bind_rows(tt_row)
  }

  # Output
  return(new_tdcmm_ttst(
    new_tdcmm(out,
              func = "t_test",
              data = data,
              params = list(group_var = group_var_str,
                            vars = test_vars_str,
                            var.equal = var.equal,
                            paired = paired,
                            pooled_sd = pooled_sd,
                            levels = levels,
                            case_var = case_var),
              model = model_list))
  )
}

## Format computed t-test
##
## Outputs a t-test for one test variable
##
## @inheritParams t_test
## @param test_var Test variable
## @param tt [htest] t.test object as returned from [compute_t_test]
## @param x splitted x part of the data
## @param y splitted y part of the data
##
## @return a [tibble][tibble::tibble-package]
##
## @family t-test
##
## @keywords internal
format_t_test <- function(test_var, tt, x, y, levels, pooled_sd) {

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

## Compute Cohen's d
##
## Computes the effect size estimate Cohen's d for two sets of numerical values
##
## @param x a (non-empty) numeric vector of data values.
## @param y a (non-empty) numeric vector of data values.
## @param pooled_sd a logical indicating whether to use the pooled standard
##   deviation in the calculation of Cohen's d. Defaults to `TRUE`.
## @param na.rm a logical value indicating whether NA values should be stripped
##  before the computation proceeds. Defaults to `TRUE`.
##
## @return a `dbl`
##
## @family t-test
##
## @keywords internal
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

## Visualize `t_test()` as points with 95% CI ranges
##
## @param x a [tdcmm] model
##
## @return a [ggplot2] object
##
## @family tdcmm visualize
#
## @keywords internal
visualize_t_test <- function(x, design = design_lmu()) {
  if ("mu" %in% names(attr(x, "params"))) {
    return(warn_about_missing_visualization(x))
  }

  # get variables
  group_var_str <- attr(x, "params")$group_var
  group_var <- sym(group_var_str)

  test_vars_str <- attr(x, "params")$vars
  test_vars_str <- test_vars_str[test_vars_str != group_var_str]
  test_vars <- syms(test_vars_str)

  data <- attr(x, "data")
  levels <- attr(x, "params")$levels
  level_names <- levels %>%
    stringr::str_replace_all(" ", "_") %>%
    stringr::str_sub(1, 10)
  n_str <- paste("N", level_names, sep = "_")

  # collect n
  n <- NULL
  for (test_var_str in test_vars_str) {
    n <- n %>%
      rbind(tibble::tibble(Variable = test_var_str,
                           !!n_str[1] := (
                             data %>%
                               dplyr::filter({{ group_var }} == levels[1]) %>%
                               nrow()
                           ),
                           !!n_str[2] := (
                             data %>%
                               dplyr::filter({{ group_var }} == levels[2]) %>%
                               nrow()
                           )))
  }

  # merge
  data <- x %>%
    dplyr::select(-c("Delta_M", "t", "df", "p", "d")) %>%
    dplyr::left_join(n, by = "Variable") %>%
    tidyr::pivot_longer(tidyselect::ends_with(level_names),
                        names_to = "level") %>%
    dplyr::mutate(var = stringr::str_split_i(.data$level, "_", 1),
                  level = stringr::str_split_i(.data$level, "_", 2)) %>%
    tidyr::pivot_wider(names_from = "var",
                       values_from = "value") %>%
    dplyr::mutate(ci_95_ll = calculate_ci_ll(.data$M, .data$SD, .data$N),
                  ci_95_ul = calculate_ci_ul(.data$M, .data$SD, .data$N))

  # build graph
  data %>%
    ggplot2::ggplot(ggplot2::aes(xmin = .data$ci_95_ll,
                                 x = .data$M,
                                 xmax = .data$ci_95_ul,
                                 y = .data$Variable,
                                 color = .data$level)) +
    ggplot2::geom_pointrange(stat = "identity",
                             position = ggplot2::position_dodge2(width = 0.9),
                             linewidth = design$main_size) +
    ggplot2::scale_x_continuous(NULL,
                                n.breaks = 8) +
    ggplot2::scale_y_discrete(NULL) +
    ggplot2::scale_color_manual(NULL,
                                values = design$main_colors,
                                guide = ggplot2::guide_legend(reverse = TRUE)) +
    design$theme() +
    ggplot2::theme(legend.position = "bottom")
}

# Constructors ----

new_tdcmm_ttst <- function(x) {
  stopifnot(is_tdcmm(x))

  structure(
    x,
    class = c("tdcmm_ttst", class(x))
  )
}
