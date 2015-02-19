#' @export
as.precintcon.decade <- function(object) {
	object <- as.precintcon.annual(object)
	
	if ((max(object[,1]) - min(object[,1]) %% 10 != 0))
		stop("input data should has a number of rows multiple of 10")
	
	result <- data.frame()
	
	for (i in seq(i: nrow(object), by=10))
		result <- rbind(result, data.frame(year=object[i,1], precipitation=sum(object[i:(i+9),2])))
	
	class(result) <- c("data.frame", "precintcon.decade")
	
	return(result)
}