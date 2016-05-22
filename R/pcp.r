#' @name pcp
#' @aliases pcp
#' @author Lucas Venezian Povoa \email{lucasvenez@@gmail.com}
#' 
#' @title Precipitation Concentration Period
#' @description It calculates the Precipitation Concentration Period (PCP) on a 
#' daily or monthly precipitation serie.
#' @usage pcp(object)
#' @param object a daily or monthly precipitation serie.
#' @param azimuth a set of degrees for each month of an year.
#' @return A data.frame containing the following variables:
#' \itemize{
#' \item \code{year} is the year.
#' \item \code{pcp} is the precipitation concentration period in degree.
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
pcp <- function(object, azimuth = c(0, 30, 60, 90, 120, 150, 180, 210, 240, 270, 300, 330)) {
	
	if (is.element("precintcon.daily", class(object)))
		object <- as.precintcon.monthly(object)
	
	if (!is.element("precintcon.monthly", class(object)))
		stop("Invalid data. Please, check your input object.")
	
	azimuth <- azimuth * 0.0174532925
	
	rx <- aggregate(object$precipitation * sin(azimuth[object$month]), by = list(object$year), FUN = sum)[2]
	
	ry <- aggregate(object$precipitation * cos(azimuth[object$month]), by = list(object$year), FUN = sum)[2]
	
	pcp = atan(rx / ry) / 0.0174532925
	
	data.frame(year = unique(object$year), t = pcp, pcp = pcp %% 360)
}