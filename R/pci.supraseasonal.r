pci.suprasenason <- function(object, hemisthere = c("n", "s")) {
  
  object <- as.precintcon.monthly(object)
  
  result <- data.frame()
  
  station <- c("dry", "wet")
  
  start <- which(object$month == 4)[1]
  
  for(i in seq(start, nrow(object), by = 6)) {
    if (nrow(object) - i < 5) break;
    
    result <- rbind(result, 
              data.frame(
                year          = object[i,1], 
                season        = station[abs((i - start) %/% 6 + ifelse(hemisthere == "n", 0, 1)) %% 2 + 1], 
                pci.seasonal  = (sum(object[i:(i+5),3]**2)/sum(object[i:(i+5),3])**2)*50))
  }
  
  class(result) <- c("data.frame", "precintcon.seasonal")
  
  return(result)
  
}