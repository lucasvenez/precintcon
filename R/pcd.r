#' @name pcd
#' @aliases pcd
#' @author Lucas Venezian Povoa \email{lucasvenez@@gmail.com}
#' 
#' @title Precipitation Concentration Degree
#' @description It calculates the Precipitation Concentration Degree (PCI) on a 
#' daily or monthly precipitation serie.
#' @usage pcd(object)
#' @param object a daily or monthly precipitation serie.
#' @return A data.frame containing the following variables:
#' \itemize{
#' \item \code{year} is the year.
#' \item \code{pcd} is the precipitation concentration index.
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
#' @keywords precipitation concentration degree
#' @export
pcd <- function(object, azimuth = c(0, 30, 60, 90, 120, 150, 180, 210, 240, 270, 300, 330)) {
	
	if (is.element("precintcon.daily", class(object)))
		object <- as.precintcon.monthly(object);
	
	if (!is.element("precintcon.monthly", class(object)))
		stop("Invalid data. Please, check your input object.")
	
	azimuth <- azimuth * 0.0174532925
	
	r <- as.annual(object)
	
	rx <- aggregate(object$precipitation * sin(azimuth), by = list(object$year), FUN = sum)[2]
	
	ry <- aggregate(object$precipitation * cos(azimuth), by = list(object$year), FUN = sum)[2]
	
	data.frame(year = r$year, pcd = sqrt(rx^2 + ry^2) / r$precipitation)
}