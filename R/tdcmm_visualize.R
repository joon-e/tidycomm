### Internal functions ###

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
    dplyr::mutate(Variable = forcats::as_factor(Variable),
                  Variable_desc = forcats::fct_rev(Variable)) %>%
    ggplot2::ggplot(ggplot2::aes(y = Variable_desc)) +
    ggplot2::geom_boxplot(ggplot2::aes(xmin = Min,
                                       xlower = Q25,
                                       xmiddle = Mdn,
                                       xupper = Q75,
                                       xmax = Max),
                          stat = "identity") +
    ggplot2::scale_x_continuous(NULL,
                                limits = c(0, NA),
                                n.breaks = 8) +
    ggplot2::scale_y_discrete(NULL) +
    ggplot2::theme_minimal()
}
