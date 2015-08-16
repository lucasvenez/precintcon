#' @name tii
#' @aliases tii
#' @author Lucas Venezian Povoa
#' 
#' @title Temporaly Irregularity Index
#' @description It calculate the temporaly irregularity index according function sum(log(Pi+1/Pi))/(n-1),
#' where Pi is the precipitation amount of year i, and n is the number of years.
#' @details Daily, monthly or seasonal precipitation series are transformed to annual series.
#' @usage tii(object)
#' @param object is a daily, monthly or seasonal precipitation serie
#' @return the temporaly irregularity index
#' @example
#' data(montly)
#' 
#' tii(montly)
#' @export
tii <- function(object) {
  object <- as.annual(object)
  return(sum(log(object$precipitation[2:nrow(object)]/object$precipitation[(1:(nrow(object)-1))]))/(nrow(object)-1))
}
