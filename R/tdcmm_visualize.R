### Internal functions ###

#' tidycomm defaults for visualizations
#'
#' @return a list
#'
#' @family tdcmm visualize
#'
#' @keywords internal
tdcmm_defaults <- function() {
  return(list(fill_qual_1 = "#858585",
              fill_qual_max12 = "Set3",
              fill_qual_inf = c("#858585", "#E2E2E2"),
              theme = ggplot2::theme_minimal))
}


#' Helper function for labelling purposes
#'
#' @param numeric share between 0 and 1
#'
#' @return a string with formatted % (rounded and suffixed)
#'
#' @family tdcmm visualize
#'
#' @keywords internal
percentage_labeller <-  function(x) {
  return(paste0(round(100*x, 0), "%"))
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
    tdcmm_defaults()$theme()

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

  if (length(dependent_var_strings) > 1) {
    stop(glue("Visualizing multiple crosstabs at once looks overwhelming. ",
              "Consider reducing the number of variables in crosstab() to ",
              "two before calling visualize()."),
            call. = FALSE)
  }

  data <- x %>%
    dplyr::rename(level = !!sym(dependent_var_strings[1])) %>%
    tidyr::pivot_longer(level,
                        names_to = "label_independent") %>%
    dplyr::mutate(label_independent = forcats::as_factor(.data$label_independent),
                  label_independent_desc = forcats::fct_rev(.data$label_independent))

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
                                 palette = tdcmm_defaults()$fill_qual_max12,
                                 guide = ggplot2::guide_legend(reverse = TRUE)) +
      tdcmm_defaults()$theme() +
      ggplot2::theme(legend.position = "bottom")

  return(g)
}
