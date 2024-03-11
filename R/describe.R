#' Describe numeric variables
#'
#' Describe numeric variables by several measures of
#' central tendency and variability. If no variables are specified,
#' all numeric (integer or double) variables are described.
#'
#' - N: number of valid cases (i.e., all but missing)
#' - Missing: number of NA cases
#' - M: [mean] average
#' - SD: standard deviation, [sd]
#' - Min: minimum value, [min]
#' - Q25: 25% quantile, [quantile]
#' - Mdn: [median] average, same as 50% quantile
#' - Q75: 75% quantile, [quantile]
#' - Max: maximum value, [max]
#' - Range: difference between Min and Max
#' - CI_95_LL: \eqn{M - Q(0.975) \times \frac{SD}{\sqrt{N}}} where \eqn{Q(0.975)} denotes Student t's [stats::quantile] function with a probability of \eqn{0.975} and \eqn{N-1} degrees of freedom
#' - CI_95_UL: \eqn{M + Q(0.975) \times \frac{SD}{\sqrt{N}}} where \eqn{Q(0.975)} denotes Student t's [stats::quantile] function with a probability of \eqn{0.975} and \eqn{N-1} degrees of freedom
#' - Skewness: traditional Fisher-Pearson coefficient of skewness of valid cases as per \eqn{\frac{\frac{1}{N} \sum\limits_{i=1}^N (x_{i}-\overline{x})^3}{[\frac{1}{N}\sum\limits_{i=1}^N (x_{i}-\overline{x})^2]^{3/2}}}
#' where \eqn{\overline{x}} denotes \eqn{M}, following Doane & Seward (2011, p. 6, 1a). See DOI \doi{10.1080/10691898.2011.11889611}.
#' - Kurtosis: empirical sample kurtosis (i.e., standardized fourth population moment about the mean) as per \eqn{\frac{\sum (x-\overline{x})^4 / N}{(\sum (x-\overline{x})^2 / N)^2}}, following DeCarlo (1997, p. 292, b2). See DOI \doi{10.1037/1082-989X.2.3.292}.
#'
#' @param data a [tibble][tibble::tibble-package] or a [tdcmm] model
#' @param ... Variables to describe (column names). Leave empty to describe all
#'   numeric variables in data.
#' @param na.rm a logical value indicating whether `NA` values should be stripped
#'  before the computation proceeds. Defaults to `TRUE`.
#'
#' @return a [tdcmm] model
#'
#' @examples
#' WoJ %>% describe(autonomy_selection, autonomy_emphasis, work_experience)
#' fbposts %>% describe(n_pictures)
#'
#' @family descriptives
#'
#' @export
describe <- function(data, ..., na.rm = TRUE) {

  # Get current grouping
  grouping <- dplyr::groups(data)

  # Get describe vars
  vars <- grab_vars(data, enquos(...))
  vars_str <- purrr::map_chr(vars, as_label)

  # Check if vars is empty and all vars are numeric
  if (length(vars) == 0) {
    stop("No numeric variables found to descibre")
  }

  if (!all(purrr::map_lgl(data %>%
                          dplyr::ungroup() %>%
                          dplyr::select(!!!vars),
                          is.numeric))) {
    stop("... must only contain numeric variables.")
  }

  # Describe
  data <- data %>%
    dplyr::select(!!!vars, !!!grouping) %>%
    tidyr::pivot_longer(c(!!!vars), names_to = "Variable", values_to = "Value")

  out <- data %>%
    dplyr::group_by(.data$Variable, .add = TRUE, .drop = TRUE) %>%
    dplyr::summarise(
      N = dplyr::n() - sum(is.na(.data$Value)),
      Missing = sum(is.na(.data$Value)),
      M = mean(.data$Value, na.rm = na.rm),
      SD = sd(.data$Value, na.rm = na.rm),
      Min = min(.data$Value, na.rm = na.rm),
      Q25 = quantile(.data$Value, .25, na.rm = na.rm),
      Mdn = median(.data$Value, na.rm = na.rm),
      Q75 = quantile(.data$Value, .75, na.rm = na.rm),
      Max = max(.data$Value, na.rm = na.rm),
      Range = .data$Max - .data$Min,
      CI_95_LL = calculate_ci_ll(.data$M, .data$SD, .data$N),
      CI_95_UL = calculate_ci_ul(.data$M, .data$SD, .data$N),
      Skewness = skewness(.data$Value),
      Kurtosis = kurtosis(.data$Value)
    ) %>%
    dplyr::arrange(match(.data$Variable, vars_str))

  # Output
  return(new_tdcmm_dscrb(new_tdcmm(out,
                                   func = "describe",
                                   data = data,
                                   params = list(vars = vars_str,
                                                 na.rm = na.rm))))
}

#' Describe categorical variables
#'
#' Describe categorical variables by N, number of unique values, and mode.
#' Note that in case of multiple modes, the first mode by order of values
#' is chosen.
#'
#' If no variables are specified, all categorical (character or factor)
#' variables are described.
#'
#' - N: number of valid cases (i.e., all but missing)
#' - Missing: number of NA cases
#' - Unique: number of unique categories in a given variable, without Missing
#' - Mode: mode average (if multiple modes exist, first mode by order of values is returned)
#' - Mode_N: number of cases reflecting the Mode
#'
#' @param data a [tibble][tibble::tibble-package] or a [tdcmm] model
#' @param ... Variables to describe (column names). Leave empty to describe all
#'   categorical variables in data.
#'
#' @return a [tdcmm] model
#'
#' @examples
#' WoJ %>% describe_cat(reach, employment, temp_contract)
#' fbposts %>% describe_cat(type)
#'
#' @family descriptives
#'
#' @export
describe_cat <- function(data, ...) {

  # Get current grouping
  grouping <- dplyr::groups(data)

  # Get describe vars
  vars <- grab_vars(data, enquos(...), alternative = "categorical")
  vars_str <- purrr::map_chr(vars, as_label)

  # Check if vars is empty and all vars are numeric
  if (length(vars) == 0) {
    stop("No categorical variables found to descibre")
  }

  # Describe
  data <- data %>%
    dplyr::select(!!!vars, !!!grouping) %>%
    tidyr::pivot_longer(c(!!!vars), names_to = "Variable", values_to = "Value")

  out <- data %>%
    dplyr::group_by(.data$Variable, .add = TRUE, .drop = TRUE) %>%
    dplyr::summarise(
      N = dplyr::n() - sum(is.na(.data$Value)),
      Missing = sum(is.na(.data$Value)),
      Unique = length(unique(.data$Value)) - ifelse(.data$Missing > 0, 1, 0),
      Mode = as.character(get_mode(.data$Value)),
      Mode_N = sum(.data$Value == .data$Mode, na.rm = TRUE)
    ) %>%
    dplyr::arrange(match(.data$Variable, vars_str))

  # Output
  return(new_tdcmm_dscrb(new_tdcmm(out,
                                   func = "describe_cat",
                                   data = data,
                                   params = list(vars = vars_str))))
}

#' @rdname visualize
#' @export
visualize.tdcmm_dscrb <- function(x, ..., .design = design_lmu()) {
  if (attr(x, "func") == "describe") {
    return(visualize_describe(x, .design))
  }

  if (attr(x, "func") == "describe_cat") {
    return(visualize_describe_cat(x, .design))
  }

  return(warn_about_missing_visualization(x))
}



### Internal functions ###

## Skewness
##
## Compute empirical skewness
##
## @param x a numerical vector
##
## @return a `dbl`
##
## @keywords internal
skewness <- function(x) {
  x <- x[!is.na(x)]
  m <- mean(x, na.rm = TRUE)
  sum((x - m)^3) / length(x) / ((sum((x - m)^2) / length(x))^(3/2))
}

## Kurtosis
##
## Compute empirical kurtosis
##
## @param x a numerical vector
##
## @return a `dbl`
##
## @keywords internal
kurtosis <- function(x) {
  x <- x[!is.na(x)]
  m <- mean(x, na.rm = TRUE)
  n <- length(x)
  (sum((x - m)^4) / n) / (sum((x - m)^2) / n)^2
}

## Visualize `describe()` as horizontal box plot
##
## @param x a [tdcmm] model
##
## @return a [ggplot2] object
##
## @family tdcmm visualize
#
## @keywords internal
visualize_describe <- function(x, design = design_lmu()) {
  x %>%
    dplyr::mutate(Variable = forcats::as_factor(.data$Variable),
                  Variable_desc = forcats::fct_rev(.data$Variable)) %>%
    ggplot2::ggplot(ggplot2::aes(xmin = .data$Min,
                                 xlower = .data$Q25,
                                 xmiddle = .data$Mdn,
                                 xupper = .data$Q75,
                                 xmax = .data$Max,
                                 y = .data$Variable_desc)) +
    ggplot2::geom_boxplot(stat = "identity",
                          fill = ggplot2::alpha(design$main_color_1,
                                                .25),
                          color = design$main_color_1,
                          linewidth = design$main_size) +
    ggplot2::scale_x_continuous(NULL,
                                limits = c(0, NA),
                                n.breaks = 8) +
    ggplot2::scale_y_discrete(NULL) +
    design$theme()
}

## Visualize `describe_cat()` as horizontal bar plot
##
## @param x a [tdcmm] model
##
## @return a [ggplot2] object
##
## @family tdcmm visualize
##
## @keywords internal
visualize_describe_cat <- function(x, design = design_lmu()) {
  x %>%
    attr("data") %>%
    dplyr::arrange(.data$Variable) %>%
    dplyr::mutate(Variable = forcats::as_factor(.data$Variable),
                  Variable_desc = forcats::fct_rev(.data$Variable)) %>%
    dplyr::group_by(.data$Variable_desc,
                    .data$Value,
                    .add = TRUE, .drop = TRUE) %>%
    dplyr::summarise(N = dplyr::n() - sum(is.na(.data$Value))) %>%
    dplyr::arrange(.data$Variable_desc,
                   dplyr::desc(.data$N)) %>%
    dplyr::mutate(Value = paste(" ", .data$Value, " "),
                  Value = forcats::as_factor(.data$Value),
                  Value_desc = forcats::fct_rev(.data$Value),
                  label_placement = ifelse(dplyr::row_number() == 1, 1, 0),
                  label_color = ifelse(dplyr::row_number() == 1, "w", "b"))%>%
    ggplot2::ggplot(ggplot2::aes(x = .data$N,
                                 y = .data$Variable_desc,
                                 fill = .data$Value_desc,
                                 label = .data$Value_desc)) +
    ggplot2::geom_bar(stat = "identity",
                      position = ggplot2::position_dodge2(width = 0.9)) +
    ggplot2::geom_text(ggplot2::aes(hjust = .data$label_placement,
                                    color = .data$label_color),
                       position = ggplot2::position_dodge2(width = 0.9)) +
    ggplot2::scale_x_continuous('N',
                                limits = c(0, NA),
                                n.breaks = 10) +
    ggplot2::scale_y_discrete(NULL) +
    ggplot2::scale_fill_manual(NULL,
                               values = rep(design$main_color_1,
                                            dplyr::n_distinct(attr(x, "data")$Value))) +
    ggplot2::scale_color_manual(NULL, values = c("w" = design$main_contrast_1,
                                                 "b" = "black")) +
    design$theme() +
    ggplot2::theme(legend.position = "none")
}


# Constructors ----

new_tdcmm_dscrb <- function(x) {
  stopifnot(is_tdcmm(x))

  structure(
    x,
    class = c("tdcmm_dscrb", class(x))
  )
}
