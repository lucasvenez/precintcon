#' @include as.precintcon.decade.r
NULL

#' @name as.decade
#' @author Lucas Venezian Povoa \email{lucasvenez@@gmail.com}
#' @title Convert a precipitation serie to a decade serie
#' @aliases as.precintcon.decade as.decade
#' @description Converts a daily, monthly, seasonal, or annual 
#' precipitation serie to a decade serie.
#' @usage as.decade(object)
#' @param object a precintcon.daily, precintcon.monthly, precintcon.seasonal,
#' precintcon.annual object or a data.frame containing 33 or 3 columns.
#' @return A data.frame (precintcon.decade) containing the following variables:
#' \itemize{
#'	\item \code{year} is the year.
#' \item \code{precipitation} is the decade's precipitation in millimeters.   
#' }
#' @seealso 
#' \code{\link{as.precintcon.annual}} 
#' \code{\link{as.precintcon.seasonal}} 
#' \code{\link{as.precintcon.monthly}}
#' \code{\link{as.precintcon.daily}}
#' @examples 
#' ##
#' # Loading the daily precipitation serie.
#' data(daily)
#' 
#' ##
#' # Converting precipitation
#' as.monthly(daily)
#' @keywords decade precipitation
#' @export
as.decade <- function(object) {
   return(as.precintcon.decade(object))  
}