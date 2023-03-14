# Constructors ----

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

# Accessors ----

#' Access model(s) used to estimate output
#' @export
model <- function(x, ...) {
  UseMethod("model")
}

#' @export
model.tdcmm <- function(x, ...) {
  model <- attr(x, "model")
  if (length(model) == 1) model[[1]] else model
}
