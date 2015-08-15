#' @author Lucas Venezian Povoa
#' @name f.factor
#' @title F factor
#' @description 
#' @usage f.factor(object)
#' @param object is a daily or monthly precipitation serie
#' @return the f factor index
#' @export
f.factor <- function(object) {
  
  m <- as.monthly(object)
  
  y <- as.annual(object)
  
  rm(object)
  
  result <- 0;
  
  for (j in 1:nrow(y)) {
    my <- m[m$year == y$year[j],]
    for (i in 1:nrow(my)) {
      result <- result + ((my$precipitation[i]**2) / y$precipitation[j])
    }
  }
  
  return(result / nrow(y))
}