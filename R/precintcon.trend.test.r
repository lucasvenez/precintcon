#' @noRd
#' @name precintcon.trend.test
#' @author Lucas Venezian Povoa \email{lucasvenez@@gmail.com} 
#' @aliases precintcon.trend.test 
#' @title precintcon.trend.test 
#' @description precintcon.trend.test 
#' @usage precintcon.trend.test(object, significance.level = 0.05)
#' @param object object
#' @param significance.level significance.level
#' @return A trend test index.
#' @seealso \code{\link{precintcon.ci.analysis}}
#' @keywords precipitation trend test 
precintcon.trend.test <- function(object, significance.level=0.05) {

	data <- NULL
	
	if (is.element("precintcon.daily", class(object)) || 
			is.element("precintcon.monthly", class(object))) {
		
		if (is.element("precintcon.monthly", class(object)))
			data <- object[[3]]
			
		else 
			data <- as.vector((as.matrix(object[,3:33])))
	
	} else if (is.vector(object) && class(object) == "numeric") 
		data <- object
	
	else
		stop("Invalud data. Please, check your input object.")
	
	n <- length(data)
	
	data[is.na(data)] <- 0.0
	
	S <- 0.0
	
	for (i in 2:n) {
		
	   r <- data[(i:n)] - data[i-1]
	   S <- S + length(r[r>0]) + (-1 * length(r[r<0]))
	}

	S.var <- ((n * (n - 1) * (2 * n + 5))) / 18
	
	Z <- 0.0
	p.value <- 0.0
	
	if (n > 10) {
		Z <- if (S > 0) (S - 1) / sqrt(S.var) else if (S == 0) 0 else (S + 1) /sqrt(S.var)
		p.value <- pnorm(Z)
	}
	
	return(data.frame(S=S, var.S=S.var, Z=Z,p.value=p.value, p.value.two.tailed=2*p.value))
}

###
##
# Calculating ties
#
#	ties <- rep(0, length(data))
#	
#	for (i in 1:(length(data)-1))
#		
#		for(j in (i+1):length(data)) {
#			
#			if (!is.na(data[j]) && !is.na(data[i])) {
#				
#				r <- data[j] - data[i]
#
#				if (r == 0)
#					ties[j] <- ties[j] + 1
#			}
#		}
#	
#	for (i in 1:length(data))
#		sum <- sum + (ties[i] * i * (i -1) * (2 * 1 + 5))
#	
#   S.var <- ((n * (n - 1) * (2 * n + 5)) - sum) / 18
