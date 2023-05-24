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


#' Helper function for indicating a lack of visualization
#'
#' @param a [tdcmm] model
#'
#' @return NULL
#'
#' @family tdcmm visualize
#'
#' @keywords internal
warn_about_missing_visualization <- function(x) {
  warning(glue("No visualization implemented for this model."),
          call. = FALSE)
  return(NULL)
}
