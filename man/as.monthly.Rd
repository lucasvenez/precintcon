% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/as.monthly.r
\name{as.monthly}
\alias{as.monthly}
\alias{as.precintcon.monthly}
\title{Convert a daily precipitation serie to a monthly serie}
\usage{
as.monthly(object)
}
\arguments{
\item{object}{a precintcon.daily object or a data.frame containing 
33 or 3 columns}
}
\value{
A data.frame (precintcon.monthly) containing the following variables:
\itemize{
\item \code{year} is the year.
\item \code{month} is the month.
\item \code{precipitation} is the precipitation amount in millimeters.
}
}
\description{
Converts a daily precipitation serie to a monthly serie.
}
\examples{
## Loading the daily precipitation serie.
#
data(daily)

## Converting precipitation
#
as.monthly(daily)
}
\author{
Lucas Venezian Povoa \email{lucasvenez@gmail.com}
}
\seealso{
\code{\link{pplot.lorenz}}
\code{\link{read.data}}
}
\keyword{monthly}
\keyword{precipitation}

