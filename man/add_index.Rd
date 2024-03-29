% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_index.R
\name{add_index}
\alias{add_index}
\title{Add index}
\usage{
add_index(data, name, ..., type = "mean", na.rm = TRUE, cast.numeric = FALSE)
}
\arguments{
\item{data}{a \link[tibble:tibble-package]{tibble} or a \link{tdcmm} model}

\item{name}{Name of the index column to compute.}

\item{...}{Variables used for the index.}

\item{type}{Type of index to compute. Either "mean" (default) or "sum".}

\item{na.rm}{a logical value indicating whether \code{NA} values should be stripped
before the computation proceeds. Defaults to \code{TRUE}.}

\item{cast.numeric}{a logical value indicating whether all variables selected
for index computation should be converted to numeric. Useful if computing
indices from factor variables. Defaults to \code{FALSE}.}
}
\value{
a \link{tdcmm} model
}
\description{
Add a rowwise mean or sum index of specific variables to the dataset.
}
\examples{
WoJ \%>\% add_index(ethical_flexibility, ethics_1, ethics_2, ethics_3, ethics_4)
WoJ \%>\% add_index(ethical_flexibility, ethics_1, ethics_2, ethics_3, ethics_4, type = "sum")

}
\seealso{
\code{\link[=get_reliability]{get_reliability()}} to compute reliability estimates of added index
variables.
}
