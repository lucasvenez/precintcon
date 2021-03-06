% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pplot.pcd.r
\name{pplot.pcd}
\alias{pplot.pcd}
\title{Plot Precipitation Concentration Degree}
\usage{
pplot.pcd(\dots, xlab = "Year", ylab = "PCD", 
       ylim = c(0,360), legend = NULL, fontsize = 10, axis.text.color = "black", 
       export = FALSE, export.name = "pcd_plot.png", width = 8.6, 
       height = 7.5, units = "cm")
}
\arguments{
\item{xlab}{the text for the x axis. (Default value: "Year")}

\item{ylab}{the text for the y axis. (Default value: "PCD")}

\item{ylim}{the limits of the y axis. (Default value: c(0, 360))}

\item{legend}{the text vector for the legend items. If NULL the legends will 
be equals to the variable names. (Default value: NULL)}

\item{fontsize}{the font size value in pt. (Default value: 10)}

\item{axis.text.color}{the legend colors. (Default value: "black")}

\item{export}{the logical value for defining whether the graph should be export 
to a file or not. (Default value: FALSE)}

\item{export.name}{the text for defining the exported file name. It is only used 
if export = TRUE. (Default value: "pcd_plot.png")}

\item{width}{the number for defining the exported graph width. It is only used if 
export = TRUE. (Default value: 8.6)}

\item{height}{the number for defining the exported graph height. It is only used 
if export = TRUE. (Default value: 7.5)}

\item{units}{the text for defining the units of the height and width parameters. 
It is only used if export = TRUE. (Default value: "cm")}

\item{\dots}{a set of daily or monthly precipitation series.}
}
\description{
Plots the Precipitation Concentraition Degre per year of a precipitation serie.
}
\examples{
##
# Loading the daily precipitation serie.
data(daily)

##
# Plotting PCD index per year
pplot.pcd(daily)
}
\author{
Lucas Venezian Povoa \email{lucasvenez@gmail.com}
}
\references{
Zhang LJ, Qian YF (2003) Annual distribution features of precipitation in China and their interannual variations. J Acta Meteorological Sinica 17:146-163
}
\seealso{
\code{\link{pcd}}
\code{\link{read.data}}
}
\keyword{PCD}
\keyword{concentration}
\keyword{degree}
\keyword{precipitation}

