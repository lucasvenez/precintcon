#' @name trend.test
#' @author Lucas Venezian Povoa \email{lucasvenez@@gmail.com} 
#' @aliases precintcon.trend.test 
#' @title trend.test 
#' @description trend.test 
#' @usage trend.test(object, significance.level = 0.05)
#' @param object is a daily precipitation serie
#' @param significance.level is 
#' @return A trend test index.
#' @seealso \code{\link{ci}} \code{\link{ci.per.year}} \code{\link{pci}} \code{\link{read.data}}
#' @keywords precipitation trend test 
#' @export
trend.test <- function(object, significance.level = 0.05) {

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
