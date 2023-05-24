#' Tabulate frequencies
#'
#' Tabulates frequencies for one or more categorical variable, including relative,
#' and cumulative frequencies.
#'
#' @param data a [tibble][tibble::tibble-package] or a [tdcmm] model
#' @param ... Variables to tabulate
#'
#' @return a [tdcmm] model
#'
#' @examples
#' WoJ %>% tab_frequencies(employment)
#' WoJ %>% tab_frequencies(employment, country)
#'
#' @family categorical
#'
#' @export
tab_frequencies <- function(data, ...) {
  vars <- grab_vars(data, enquos(...))
  vars_str <- purrr::map_chr(vars, as_label)

  grouping <- dplyr::groups(data)

  d <- data %>%
    dplyr::group_by(..., !!!grouping) %>%
    dplyr::summarise(n = dplyr::n()) %>%
    dplyr::group_by(!!!grouping) %>%
    dplyr::mutate(percent = .data$n / sum(.data$n)) %>%
    dplyr::arrange(!!!grouping)

  out <- d %>%
    dplyr::bind_cols(d %>%
                dplyr::select(!!!grouping,
                       cum_n = "n",
                       cum_percent = "percent") %>%
                dplyr::mutate_at(dplyr::vars(-dplyr::group_cols()), cumsum) %>%
                dplyr::ungroup() %>%
                dplyr::select("cum_n", "cum_percent")
    )

  return(new_tdcmm_ctgrcl(new_tdcmm(out,
                                    func = "tab_frequencies",
                                    data = data,
                                    params = list(vars = vars_str))))
}

#' Crosstab variables
#'
#' Computes contingency table for one independent (column) variable and one or
#' more dependent (row) variables.
#'
#' @param data a [tibble][tibble::tibble-package] or a [tdcmm] model
#' @param col_var Independent (column) variable.
#' @param ... Dependent (row) variables.
#' @param add_total Logical indicating whether a 'Total' column should be
#'   computed. Defaults to `FALSE`.
#' @param percentages Logical indicating whether to output column-wise
#'   percentages instead of absolute values. Defaults to `FALSE`.
#' @param chi_square Logical indicating whether a Chi-square test should be computed.
#'   Test results will be reported via message(). Defaults to `FALSE`.
#'
#' @return a [tdcmm] model
#'
#' @examples
#' WoJ %>% crosstab(reach, employment)
#' WoJ %>% crosstab(reach, employment, add_total = TRUE, percentages = TRUE, chi_square = TRUE)
#'
#' @family categorical
#'
#' @export
crosstab <- function(data, col_var, ..., add_total = FALSE,
                     percentages = FALSE, chi_square = FALSE) {

  # Checks
  if (dplyr::is_grouped_df(data)) {
    warning("Grouping variable(s) present in data will be ignored.",
            call. = FALSE)
  }

  vars <- grab_vars(data, enquos(...))
  vars_str <- purrr::map_chr(vars, as_label)

  cross_vars <- length(quos(...))

  if (cross_vars < 1) {
    stop("Must provide at least one variable to crosstabulate.")
  }

  # Prepare crosstab
  xt <- data %>%
    dplyr::group_by({{ col_var }}, ...) %>%
    dplyr::count() %>%
    tidyr::spread({{ col_var }}, .data$n, fill = 0) %>%
    dplyr::ungroup()

  xt_cross_vars <- xt %>%
    dplyr::select(1:tidyselect::all_of(cross_vars))

  xt_col_vars <- xt %>%
    dplyr::select(-(1:tidyselect::all_of(cross_vars)))

  # Estimate Chi-square test
  if (chi_square) {
    chi2 <- xt_col_vars %>%
      as.matrix() %>%
      chisq.test()
  }

  # Augment
  if (add_total) {
    xt_col_vars <- xt_col_vars %>%
      dplyr::mutate(Total = rowSums(xt_col_vars))
  }

  if (percentages) {
    xt_col_vars <- xt_col_vars %>%
      dplyr::mutate_all(col_percs)
  }

  # Output
  out <- xt_cross_vars %>%
    dplyr::bind_cols(xt_col_vars)

  if (chi_square) {
    return(new_tdcmm_ctgrcl(
      new_tdcmm(out,
                func = "crosstab",
                data = data,
                params = list(vars = vars_str,
                              col_var = as_name(enquo(col_var)),
                              add_total = add_total,
                              percentages = percentages,
                              chi_square = chi_square),
                model = list(chi2))))
  } else {
    return(new_tdcmm_ctgrcl(
      new_tdcmm(out,
                func = "crosstab",
                data = data,
                params = list(vars = vars_str,
                              col_var = as_name(enquo(col_var)),
                              add_total = add_total,
                              percentages = percentages,
                              chi_square = chi_square))))
  }
}

#' @export
visualize.tdcmm_ctgrcl <- function(x, ...) {
  if (attr(x, "func") == "tab_frequencies") {
    return(visualize_tab_frequencies(x))
  }

  if (attr(x, "func") == "crosstab") {
    return(visualize_crosstab(x))
  }

  return(warn_about_missing_visualization(x))
}

# Internal functions ----

## Compute Cramer's V
##
## Computes Cramer's V
##
## @param chi2 Output from a `chisq.test()`.
##
## @return a `dbl`
##
## @family categorical
##
## @keywords internal
cramer_V <- function(chi2) {

  X2 <- chi2$statistic
  N <- sum(chi2$observed)
  k = min(dim(chi2$observed))

  unname(sqrt(X2 / (N * (k - 1))))
}

## Compute column percentages
##
## Computes column percentages
##
## @param x Numeric vector
##
## @return a `dbl`
col_percs <- function(x) {
  x / sum(x, na.rm = TRUE)
}

#' Visualize `tab_frequencies()` as one or many histogram(s)
#'
#' @param x a [tdcmm] model
#'
#' @return a [ggplot2] object
#'
#' @family tdcmm visualize
#'
#' @keywords internal
visualize_tab_frequencies <- function(x) {
  var_names <- attr(x, "params")$vars
  num_histograms <- length(var_names)

  # collect data
  data <- NULL
  for (variable in var_names) {
    data <- data %>%
      rbind(attr(x, "data") %>%
              tab_frequencies(!!sym(variable)) %>%
              dplyr::mutate(var = variable,
                            level = !!sym(variable)) %>%
              dplyr::select(var, level, percent))
  }

  # visualize
  g <- data %>%
    ggplot2::ggplot(ggplot2::aes(x = level,
                                 y = percent)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::scale_x_discrete(NULL) +
    ggplot2::scale_y_continuous(NULL,
                                labels = percentage_labeller,
                                limits = c(0, 1),
                                breaks = seq(0, 1, .1)) +
    tdcmm_visual_defaults()$theme()

  # wrap depending on number of variables
  if (num_histograms > 1) {
    g <- g + ggplot2::facet_wrap(ggplot2::vars(var),
                                 scales = "free_x")

    if (num_histograms >= 4) {
      warning(glue("Visualizing too many histograms at once might strongly ",
                   "inhibit readability. Consider reducing the number of ",
                   "variables in tab_frequencies() before calling visualize()."),
              call. = FALSE)
    }
  }

  return(g)
}

#' Visualize `crosstab()` as horizontal stacked bar plot, either absolute or
#' relative (depending on `percentages`).
#'
#' @param x a [tdcmm] model
#'
#' @return a [ggplot2] object
#'
#' @family tdcmm visualize
#
#' @keywords internal
visualize_crosstab <- function(x) {
  independent_var_string <- attr(x, "params")$col_var
  dependent_var_strings <- attr(x, "params")$vars
  dependent_var_string <- dependent_var_strings[1]

  if (length(dependent_var_strings) > 1) {
    stop(glue("Visualizing multiple crosstabs at once looks overwhelming. ",
              "Consider reducing the number of variables in crosstab() to ",
              "two before calling visualize()."),
         call. = FALSE)
  }

  data <- x %>%
    tidyr::pivot_longer(!c(!!sym(dependent_var_string)),
                        names_to = "label_independent") %>%
    dplyr::mutate(label_independent =
                    forcats::as_factor(.data$label_independent),
                  label_independent_desc =
                    forcats::fct_rev(.data$label_independent)) %>%
    dplyr::rename(level = !!sym(dependent_var_string))

  if (length(dplyr::n_distinct(data$label_independent)) > 12) {
    stop(glue("Cannot visualize crosstabs with more than 12 levels of the ",
              "independent variable ({independent_var_string})."),
         call. = FALSE)
  }

  # visualize
  g <- data %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$value,
                                 y = .data$label_independent_desc,
                                 fill = .data$level)) +
    ggplot2::geom_bar(stat = "identity",
                      position = "stack")

  if (attr(x, "params")$percentages) {
    g <- g +
      ggplot2::geom_text(ggplot2::aes(label = percentage_labeller(.data$value)),
                         position = ggplot2::position_stack(vjust = .5)) +
      ggplot2::scale_x_continuous(NULL,
                                  labels = percentage_labeller,
                                  limits = c(0, 1),
                                  breaks = seq(0, 1, .1))
  } else {
    g <- g +
      ggplot2::geom_text(ggplot2::aes(label = .data$value),
                         position = ggplot2::position_stack(vjust = .5)) +
      ggplot2::scale_x_continuous('N',
                                  limits = c(0, NA),
                                  n.breaks = 10)
  }

  g <- g +
    ggplot2::scale_y_discrete(NULL) +
    ggplot2::scale_fill_brewer(NULL,
                               palette = tdcmm_visual_defaults()$fill_qual_max12,
                               guide = ggplot2::guide_legend(reverse = TRUE)) +
    tdcmm_visual_defaults()$theme() +
    ggplot2::theme(legend.position = "bottom")

  return(g)
}


# Constructors ----

new_tdcmm_ctgrcl <- function(x) {
  stopifnot(is_tdcmm(x))

  structure(
    x,
    class = c("tdcmm_ctgrcl", class(x))
  )
}

# Formatting ----

#' @export
tbl_format_footer.tdcmm_ctgrcl <- function(x, ...) {
  default_footer <- NextMethod()

  if (attr(x, "func") != "crosstab" | length(attr(x, "params")) == 0) {
    return(default_footer)
  }

  # Get values
  chi2 <- model(x)

  # Format test string
  test_string <- glue("Chi-square = {format_value(chi2$statistic, 3)}, ",
                      "df = {format(chi2$parameter, digits = 4)}, ",
                      "{format_pvalue(chi2$p.value)}, ",
                      "V = {format_value(cramer_V(chi2), 3)}")

  # Add to footer and display
  test_footer <- style_subtle(glue("# {test_string}"))

  c(default_footer, test_footer)
}
