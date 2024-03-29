% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/categorical.R
\name{tab_frequencies}
\alias{tab_frequencies}
\title{Tabulate frequencies}
\usage{
tab_frequencies(data, ...)
}
\arguments{
\item{data}{a \link[tibble:tibble-package]{tibble} or a \link{tdcmm} model}

\item{...}{Variables to tabulate}
}
\value{
a \link{tdcmm} model
}
\description{
Tabulates frequencies for one or more categorical variable, including relative,
and cumulative frequencies.
}
\examples{
WoJ \%>\% tab_frequencies(employment)
WoJ \%>\% tab_frequencies(employment, country)

}
\seealso{
Other categorical: 
\code{\link{crosstab}()}
}
\concept{categorical}
