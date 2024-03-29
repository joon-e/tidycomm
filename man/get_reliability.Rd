% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reliability.R
\name{get_reliability}
\alias{get_reliability}
\title{Get reliability estimates of index variables}
\usage{
get_reliability(
  data,
  ...,
  type = "alpha",
  interval.type = NULL,
  bootstrap.samples = NULL,
  conf.level = NULL,
  progress = FALSE
)
}
\arguments{
\item{data}{a \link[tibble:tibble-package]{tibble} or a \link{tdcmm} model}

\item{...}{Index variables created with \code{\link{add_index}}. Leave empty
to get reliability estimates for all index variables.}

\item{type}{Type of reliability estimate. See \code{\link[MBESS]{ci.reliability}}}

\item{interval.type}{Type of reliability estimate confidence interval.
See \code{\link[MBESS]{ci.reliability}}}

\item{bootstrap.samples}{Number of bootstrap samples for CI calculation.
See \code{\link[MBESS]{ci.reliability}}}

\item{conf.level}{Confidence level for estimate CI.
See \code{\link[MBESS]{ci.reliability}}}

\item{progress}{Show progress for reliability estimate computation. Useful
if using computationally intense computations (e. g., many bootstrapping
samples) and many index variables.}
}
\value{
a \link{tdcmm} model
}
\description{
Get reliability estimates of index variables created with \code{\link{add_index}}.
}
\examples{
WoJ \%>\%
  add_index(ethical_flexibility, ethics_1, ethics_2, ethics_3, ethics_4) \%>\%
  get_reliability()

}
\seealso{
\code{\link[=add_index]{add_index()}} to create index variables
}
\concept{reliability}
