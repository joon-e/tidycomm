#' Add index
#'
#' Add a rowwise mean or sum index of specific variables to the dataset.
#'
#' @param data a [tibble][tibble::tibble-package]
#' @param name Name of the index column to compute.
#' @param ... Variables used for the index.
#' @param type Type of index to compute. Either "mean" (default) or "sum".
#' @param na.rm a logical value indicating whether `NA` values should be stripped
#'   before the computation proceeds. Defaults to `TRUE`.
#' @param cast.numeric a logical value indicating whether all variables selected
#'   for index computation should be converted to numeric. Useful if computing
#'   indices from factor variables. Defaults to `FALSE`.
#'
#' @return a [tibble][tibble::tibble-package]
#'
#' @examples
#' WoJ %>% add_index(ethical_flexibility, ethics_1, ethics_2, ethics_3, ethics_4)
#' WoJ %>% add_index(ethical_flexibility, ethics_1, ethics_2, ethics_3, ethics_4, type = "sum")
#'
#' @seealso [get_reliability()] to compute reliability estimates of added index
#'   variables.
#'
#' @export
add_index <- function(data, name, ..., type = "mean",
                      na.rm = TRUE, cast.numeric = FALSE) {

  name <- as_label(enquo(name))

  # Add index column
  i <- data %>%
    dplyr::select(...)

  # Cast numeric
  if (cast.numeric) {
    i <- dplyr::mutate(i, dplyr::across(dplyr::everything(), as.numeric))
  }

  # Check
  if (!all(purrr::map_lgl(i, is.numeric))) {
    stop("All variables for index computation must be numeric. ",
         "Set cast.numeric = TRUE to try automatically convert all used
         variables to numeric", call. = FALSE)
  }

  # Compute
  if (type == "sum") {
    index_col <- rowSums(i, na.rm = na.rm)
  } else {
    index_col <- rowMeans(i, na.rm = na.rm)
  }

  index_df <- data %>%
    dplyr::mutate(!!name := index_col)

  # Add index_of attribute
  index_vars_str <- names(i)
  attributes(index_df[[name]]) <- list(index_of = index_vars_str)

  return(index_df)
}
