#' @export 
precintcon.plot.deciles <- function(...,
	grouped         = FALSE,
	ylab            = "Precipitation",
	legend.title    = "Legend",
	legend          = NULL,
	fontsize        = 10, 
	axis.text.color = "black", 
	export          = FALSE, 
	export.name     = "deciles_plot.png", 
	width           = 8.6, 
	height          = 7.5, 
	units           = "cm"
) {
	
	l <- list(...)
	
	if (length(l) > 1 && !export && !grouped)
		par(ask = T)
	
	varl <- as.list(match.call()[1:length(l)+1])

	if (!is.null(legend) && length(varl) != length(legend))
		stop(paste("legend should has length equals to the number of input data. legend parameter length", 
						length(legend), ": number of input data", length(varl)))
	
	else if (!is.null(legend))
		varl <- as.list(legend)

	if (!grouped) {
		min <- min(unlist(lapply(l, FUN = function(d) return(as.precintcon.deciles(d)[2]))))
		max <- max(unlist(lapply(l, FUN = function(d) return(as.precintcon.deciles(d)[11]))))

      #######
      #
      # Function for plotting
      #
      f <- function(d, max, min,
            ylab, fontsize, axis.text.color, 
            export, export.name, width, height, units) {
         
         d <- as.precintcon.deciles(d)
         
         graph <- ggplot(d) + 
               geom_boxplot(aes_string(x = "dataset", ymin = "D1", lower = "D2",
                           middle = "D5", upper = "D9", ymax = "D10"), stat = "identity", show_guide = FALSE) +
               ylab(ylab) +
               theme(text = element_text(size = fontsize), 
                     axis.text = element_text(color = axis.text.color),
                     axis.title.x = element_blank(),
                     axis.text.x  = element_blank(),
                     axis.ticks.x = element_blank()) +
               ylim(min, max) +
               facet_grid(. ~ dataset)
         
         if (!export) {
            print(graph)
         } else {
            export.name <- paste(export.name, sep = "_")
            ggsave(export.name, graph, width = width, height = height, units = units)
         }
      }
      
		###########
		#
		# Generating graphs for each input data
		#	
		mapply(f, l, max = max, min = min, fontsize = fontsize, axis.text.color = axis.text.color, 
		   export = export, export.name = export.name, width = width, height = height, units = units,
		   MoreArgs = list(ylab = ylab), SIMPLIFY = FALSE)
	
	} else {
	
		#######
		#
		# Generating deciles for each input data
		#
		l <- mapply(function(d) as.precintcon.deciles(d), l, SIMPLIFY = FALSE)
		
		######
		#
		# list of data.frame to data.frame (rbind)
		#
		data <- do.call(rbind.data.frame, l)
		
		graph <- ggplot(data, aes_string(fill = "dataset")) + 
				 geom_boxplot(
						 aes_string(x = "dataset", ymin = "Q1", 
							lower = "Q2", middle = "Q5", upper = "Q9", ymax = "Q10"), 
					stat = "identity") + xlab(xlab) + ylab(ylab) +
				 scale_fill_discrete(legend.title) +
				 theme(text = element_text(size = fontsize),
					axis.text = element_text(color = axis.text.color),
					axis.title.x = element_blank(),
		 			axis.text.x  = element_blank(),
		 			axis.ticks.x = element_blank())
		
		if (!export) {
			print(graph)
		} else {
			ggsave(export.name, graph, width = width, height = height, units = units)			 
		}
		 
	}
	
	par(ask = F)
}
