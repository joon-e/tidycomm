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
              fill_qual_max12 = ggplot2::scale_fill_brewer(NULL, palette = "Set3"),
              fill_qual_inf = c("#858585", "#E2E2E2"),
              theme = ggplot2::theme_minimal))
}

#' Visualize `describe()` as horizontal box plot
#'
#' @param x a [tdcmm] model
#'
#' @return a [ggplot2] object
#'
#' @family tdcmm visualize
#
#' @keywords internal
visualize_describe <- function(x) {
  x %>%
    dplyr::mutate(Variable = forcats::as_factor(.data$Variable),
                  Variable_desc = forcats::fct_rev(.data$Variable)) %>%
    ggplot2::ggplot(ggplot2::aes(xmin = .data$Min,
                                 xlower = .data$Q25,
                                 xmiddle = .data$Mdn,
                                 xupper = .data$Q75,
                                 xmax = .data$Max,
                                 y = .data$Variable_desc)) +
    ggplot2::geom_boxplot(stat = "identity") +
    ggplot2::scale_x_continuous(NULL,
                                limits = c(0, NA),
                                n.breaks = 8) +
    ggplot2::scale_y_discrete(NULL) +
    tdcmm_defaults()$theme()
}

#' Visualize `describe_cat()` as horizontal bar plot, either stacked or
#' unstacked
#'
#' @param x a [tdcmm] model
#' @param stacked if `TRUE`, bars are stacked
#'
#' @return a [ggplot2] object
#'
#' @family tdcmm visualize
#'
#' @keywords internal
visualize_describe_cat <- function(x, stacked) {
  g <- x %>%
    attr("data") %>%
    dplyr::arrange(.data$Variable) %>%
    dplyr::mutate(Variable = forcats::as_factor(.data$Variable),
                  Variable_desc = forcats::fct_rev(.data$Variable)) %>%
    dplyr::group_by(.data$Variable_desc,
                    .data$Value,
                    .add = TRUE, .drop = TRUE) %>%
    dplyr::summarise(N = dplyr::n() - sum(is.na(.data$Value))) %>%
    dplyr::arrange(.data$Variable_desc,
                   dplyr::desc(.data$N))

  if (stacked) {
    g %>%
      dplyr::mutate(Value = forcats::as_factor(.data$Value),
                    Value_desc = forcats::fct_rev(.data$Value)) %>%
      ggplot2::ggplot(ggplot2::aes(x = .data$N,
                                   y = .data$Variable_desc,
                                   fill = .data$Value_desc,
                                   label = .data$Value_desc)) +
      ggplot2::geom_bar(stat = "identity",
                        position = "stack") +
      ggplot2::geom_text(position = ggplot2::position_stack(vjust = .5)) +
      ggplot2::scale_x_continuous('N',
                                  limits = c(0, NA),
                                  n.breaks = 10) +
      ggplot2::scale_y_discrete(NULL) +
      ggplot2::scale_fill_manual(NULL,
                                 values = rep(tdcmm_defaults()$fill_qual_inf,
                                              dplyr::n_distinct(attr(x, "data")$Value))) +
      tdcmm_defaults()$theme() +
      ggplot2::theme(legend.position = "none")
  } else {
    g %>%
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
      ggplot2::geom_text(ggplot2::aes(hjust = label_placement,
                                      color = label_color),
                         position = ggplot2::position_dodge2(width = 0.9)) +
      ggplot2::scale_x_continuous('N',
                                  limits = c(0, NA),
                                  n.breaks = 10) +
      ggplot2::scale_y_discrete(NULL) +
      ggplot2::scale_fill_manual(NULL,
                                 values = rep(tdcmm_defaults()$fill_qual_1,
                                              dplyr::n_distinct(attr(x, "data")$Value))) +
      ggplot2::scale_color_manual(NULL, values = c("w" = "white",
                                                   "b" = "black")) +
      tdcmm_defaults()$theme() +
      ggplot2::theme(legend.position = "none")
  }
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
  percentage_labeller <-  function(x) {
    paste0(round(100*x, 0), "%")
  }

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
      warning("Visualizing too many histograms at once might strongly inhibit ",
              "readability. Consider reducing the number of variables in ",
              "tab_frequencies() before calling visualize().",
              call. = FALSE)
    }
  }

  return(g)
}
