# Class description ----

#' `tdcmm` class
#'
#' @description The `tdcmm` class is a subclass of a [`tbl_df`][tibble::tbl_df],
#' also know as a "tibble", used for augmenting output tibbles of `tidycomm`
#' functions with additional information. For output based on statistical tests,
#' the model object(s) estimated and any performed assumption checks.
#'
#' `tdcmm` objects in `tidycomm` are further subclassed with individual classes
#' handling visualization and printing per "output" type.
#'
#' @name tdcmm-class
#' @aliases tdcmm tdcmm-class
NULL

# Constructors ----

#' `tdcmm` output constructor
#'
#' @description
#' Creates a new `tdcmm` class output object.
#'
#' The `tdcmm` class is a subclass of a [`tbl_df`][tibble::tbl_df],
#' also know as a "tibble", used for augmenting output tibbles of `tidycomm`
#' functions with additional information. For output based on statistical tests,
#' the model object(s) estimated and any performed assumption checks.
#'
#' `tdcmm` objects in `tidycomm` are further subclassed with individual classes
#' handling visualization and printing per "output" type.
#'
#' @param x A [tibble][tibble::tibble-package].
#' @param model A list of model object(s) used in preparation of the output.
#'   Defaults to `NULL`. A single model should be wrapped in a list of length
#'   `1`.
#' @param checks A list of assumption check object(s) used in preparation of the
#'   output. Defaults to `NULL`.
new_tdcmm <- function(x, model = NULL, checks = NULL) {
  stopifnot(tibble::is_tibble(x))
  stopifnot(is.list(model) | is.null(model))
  stopifnot(is.list(checks) | is.null(checks))

  structure(
    x,
    class = c("tdcmm", class(x)),
    model = model,
    checks = checks
  )
}

# Test ----

#' @describeIn new_tdcmm Test for class `tdcmm`
is_tdcmm <- function(x) {
  inherits(x, "tdcmm")
}


# Accessors ----

#' Access model(s) used to estimate output
#'
#' Returns model objects used to estimate `tdcmm` output.
#'
#' @param x `tdcmm` output
#' @param ... other arguments
#'
#' @returns A model object or a list of model objects
#'
#' @export
model <- function(x, ...) {
  UseMethod("model")
}

#' @export
model.tdcmm <- function(x, ...) {
  model <- attr(x, "model")
  if (is.null(model)) {
    warning(glue("'{deparse(substitute(x))}' does not contain any model.'"),
            call. = FALSE)
  }
  if (length(model) == 1) model[[1]] else model
}
