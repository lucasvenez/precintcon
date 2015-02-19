#' @include as.precintcon.seasonal.r
NULL

#' @name as.seasonal
#' @author Lucas Venezian Povoa \email{lucasvenez@@gmail.com} 
#' @aliases as.precintcon.seasonal as.seasonal 
#' @title Convert a precipitation serie to an seasonal serie 
#' @description Converts a daily or monthly precipitation serie 
#' to a seasonal serie. 
#' @usage as.seasonal(object) 
#' @param object a precintcon.daily, or precintcon.monthly object or 
#' a data.frame containing 33 or 3 columns.
#' @return A data.frame (precintcon.seasonal) containing the following variables:
#' \itemize{
#' \item \code{year} is the year.
#' \item \code{season} is the season.
#' \item \code{precipitation} is the precipitation amount in millimeters.
#' }
#' @seealso 
#' \code{\link{pplot.lorenz}}
#' \code{\link{read.data}}
#' @examples 
#' ##
#' # Loading the daily precipitation serie.
#' data(daily)
#' 
#' ##
#' # Converting precipitation
#' as.seasonal(daily)
#' @keywords seasonal precipitation
#' @export 
as.seasonal <- function(object) {
   return(as.precintcon.seasonal(object))  
}