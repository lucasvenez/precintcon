#' @name pci.seasonal
#' @title Seasonal PCI
#' @author Lucas Venezian Povoa \email{lucasvenez@@gmail.com}
#' @param object a complete daily, monthly or seasonal precipitation serie
pci.seasonal <- function(object, hemisthere = c("n", "s")) {
  
  object <- as.precintcon.monthly(object)
  
  result <- data.frame()
  
  station <- c("spring", "summer", "autumn", "winter")
  
  start <- which(object$month == 3)[1]
  
  for(i in seq(start, nrow(object), by = 3)) {
    if (nrow(object) - i < 2) break;
    
    result <- rbind(result, 
              data.frame(
                year          = object[i,1], 
                season        = station[abs((i - start) %/% 3 + ifelse(hemisthere == "n", 0, 2)) %% 4 + 1], 
                pci.seasonal  = (sum(object[i:(i+2),3]**2)/sum(object[i:(i+2),3])**2)*25))
  }
  
  class(result) <- c("data.frame", "precintcon.seasonal")
  
  return(result)
}