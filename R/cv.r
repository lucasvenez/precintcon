#' @title Coefficient of Variance
#' @name cv
#' @usage cv(object)
#' @param object is a daily or monthly precipitation serie
#' @return the coefficient of variance
#' @export
cv <- function(object) {
  object <- as.monthly(object)
  
  return(sd(object$precipitation)/mean(object$precipitation))
}