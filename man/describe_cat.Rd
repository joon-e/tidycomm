% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/describe.R
\name{describe_cat}
\alias{describe_cat}
\title{Describe categorical variables}
\usage{
describe_cat(data, ...)
}
\arguments{
\item{data}{a \link[tibble:tibble-package]{tibble}}

\item{...}{Variables to describe (column names). Leave empty to describe all
categorical variables in data.}
}
\value{
a \link[tibble:tibble-package]{tibble}
}
\description{
Describe categorical variables by N, number of unique values, and mode.
Note that in case of multiple modes, the first mode by order of values
is chosen.
}
\details{
If no variables are specified, all categorical (character or factor)
variables are described.
}
\examples{
iris \%>\% describe_cat()

}
\seealso{
Other descriptives: 
\code{\link{describe}()}
}
\concept{descriptives}
