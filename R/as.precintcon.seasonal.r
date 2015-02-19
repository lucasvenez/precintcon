#' @export
as.precintcon.seasonal <- function(object) {
	
	if (is.element("precintcon.daily", class(object)))
		object <- as.precintcon.monthly(object)
	
	result <- data.frame()
	
	for(i in seq(1,nrow(object), by=3))
		result <- rbind(result, 
			data.frame(
				year=object[i,1], 
				season=i %% 12 %/% 3 + 1, 
				precipitation=sum(object[i:(i+2),3])))

	class(result) <- c("data.frame", "precintcon.seasonal")

	return(result)
}