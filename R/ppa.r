#' @name ppa
#' @title Precipitation Percentage and Amount
#' @author Lucas Venezian Povoa \email{lucasvenez@@gmail.com}
#' @aliases ppa
#' @usage ppa(..., percent = 25)
#' @param ... a set of daily precipitation series
#' @param percent is the percentage of the rainiest days to be considered
#' @return the total precipitation and percentage of the p% of the rainiest days
#' @export 
ppa <- function(..., percent = 25) {
  
  l <- list(...)
  
  if (length(l) > 0) {
    
    set <- NULL
    
    pars <- as.list(match.call()[1:length(l)+1])
    
    res <- lapply(l, function(object, p) {
      
      ##
      # Percentage of rain in the 25% rainest days
      v <- as.vector(as.matrix(object[-(1:2)]))
      total <- sum(tail(sort(v), n = (p/100) * length(v)), na.rm = T)
      perc  <- total / sum(v, na.rm = T)
      
      return(data.frame(percentage = p, absolute = total, relative = perc))
      
    }, percent)
    
    do.call(rbind.data.frame, res)
  }
}