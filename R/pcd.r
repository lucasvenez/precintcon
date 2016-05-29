#' @name pcd
#' @aliases pcd
#' @author Lucas Venezian Povoa \email{lucasvenez@@gmail.com}
#' @title Precipitation Concentration Degree
#' @description Calculates the Precipitation Concentration Degree (PCD) on a 
#' daily or monthly precipitation serie.
#' @usage pcd(object, azimuth = seq(0, 330, by = 30))
#' @param object a daily or monthly precipitation serie.
#' @param azimuth a set of degrees for each month of an year.
#' @return A data.frame containing the following variables:
#' \itemize{
#' \item \code{year} is the year.
#' \item \code{pcd} is the percentage of precipitation concentration corresponding to a year. 
#' Values closer to 0 mean precipitation dispersion among all months and 1 concentration into only a month.
#' }
#' @seealso 
#' \code{\link{pplot.pcd}}
#' \code{\link{read.data}}
#' \code{\link{as.daily}}
#' \code{\link{as.monthly}}
#' @examples 
#' ##
#' # Loading the monthly precipitation serie.
#' data(monthly)
#' 
#' ## 
#' # Performing the Precipitation Concentration Index analysis
#' pcd(monthly)
#' @references Zhang LJ, Qian YF (2003) Annual distribution features of precipitation in China and their interannual variations. 
#' J Acta Meteorological Sinica 17:146-163
#' @keywords precipitation concentration degree PCD
#' @export
pcd <- function(
  object, 
  azimuth = seq(0, 330, by = 30)) {
	
	if (is.element("precintcon.daily", class(object)))
		object <- as.precintcon.monthly(object);
	
	if (!is.element("precintcon.monthly", class(object)))
		stop(paste("Invalid data. Please, check your input object. Actual object type:", class(object), sep = " "))
	
	azimuth <- azimuth * 0.0174532925
	
	r <- as.annual(object)
	
	rx <- aggregate(object$precipitation * sin(azimuth), by = list(object$year), FUN = sum)[2]
	
	ry <- aggregate(object$precipitation * cos(azimuth), by = list(object$year), FUN = sum)[2]
	
	r <- data.frame(year = r$year, pcd = sqrt(rx^2 + ry^2) / r$precipitation)
	
	colnames(r) <- c("year", "pcd")
	
	return(r)
}