#' @title Temporaly Irregularity Index
#' @name tii
#' @param object is a daily, monthly or seasonal precipitation serie
#' @usage tii(object)
#' @return the temporaly irregularity index
#' @author Lucas Venezian Povoa
#' @export
tii <- function(object) {
  object <- as.annual(object)
  return ((1/(nrow(object)-1))*sum(log(object$precipitation[2:nrow(object)]/object$precipitation[(1:(nrow(object)-1))])))
}
