#' Add index
#'
#' Add a rowwise mean or sum index of specific variables to the dataset.
#'
#' @param data a [tibble][tibble::tibble-package]
#' @param name Name of the index column to compute.
#' @param ... Variables used for the index.
#' @param type Type of index to compute. Either "mean" (default) or "sum".
#' @param na.rm a logical value indicating whether NA values should be stripped
#'   before the computation proceeds. Defaults to TRUE.
#'
#' @return a [tibble][tibble::tibble-package]
#'
#' @seealso [get_reliability()] to compute reliability estimates of added index
#'   variables.
#'
#' @export
add_index <- function(data, name, ..., type = "mean", na.rm = TRUE) {

  name <- rlang::as_label(rlang::enquo(name))
  index_vars <- rlang::enquos(...)

  # Add index column
  index_df <- data %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      !!name := ifelse(type == "sum",
                       sum(c(!!!index_vars), na.rm = na.rm),
                       mean(c(!!!index_vars), na.rm = na.rm))
    ) %>%
    dplyr::ungroup()

  # Add index_of attribute
  index_vars_vec <- purrr::map_chr(index_vars, rlang::as_label)
  attributes(index_df[[name]]) <- list(index_of = index_vars_vec)

  return(index_df)
}
