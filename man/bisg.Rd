% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bisg.R
\name{bisg}
\alias{bisg}
\title{Compute traditional BISG}
\usage{
bisg(name, state, county, year = 2020)
}
\arguments{
\item{name}{(character vector) A vector of surnames. Coerced to lowercase
internally.}

\item{state}{(character vector) A vector of state abbreviations. Coerced to
uppercase internally.}

\item{county}{(character vector) A vector of counties. Coerced to lowercase
internally.}

\item{year}{(integer) The year of the data to use to compute the estimates.
Currently only 2020 is available.}
}
\value{
(data frame) A data frame with colums \code{name}, \code{year}, \code{state},
\code{county}, plus six BISG probability columns:
\itemize{
\item \code{bisg_aian}
\item \code{bisg_api}
\item \code{bisg_black_nh}
\item \code{bisg_hispanic}
\item \code{bisg_white_nh}
\item \code{bisg_other}
}
}
\description{
Compute Bayesian Improved Surname Geocoding (BISG) estimates
using census surname and county reference tables.
}
\examples{
\dontrun{
bisg(
  name = c("Lopez", "Jackson", "Smith"),
  county = c("King", "King", "Chittenden"),
  state = c("WA", "WA", "VT"),
  year = 2020
)
}

}
\keyword{internal}
