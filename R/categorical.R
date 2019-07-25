# Contains functions for the description of categorical variables

#' Tabulate frequencies
#'
#' Tabulates frequencies for one or more categorical variable, including relative,
#' and cumulative frequencies.
#'
#' @param data a [tibble][tibble::tibble-package]
#' @param ... Variables to tabulate
#'
#' @return a [tibble][tibble::tibble-package]
#'
#' @export
tab_frequencies <- function(data, ...) {
  grouping <- dplyr::groups(data)

  d <- data %>%
    dplyr::group_by(..., !!!grouping) %>%
    dplyr::summarise(n = n()) %>%
    dplyr::group_by(!!!grouping) %>%
    dplyr::mutate(percent = n / sum(n)) %>%
    dplyr::arrange(!!!grouping)

  d %>%
    dplyr::bind_cols(d %>%
                dplyr::select(!!!grouping,
                       cum_n = n,
                       cum_percent = percent) %>%
                dplyr::mutate_at(dplyr::vars(-dplyr::group_cols()), cumsum)
    )

}
