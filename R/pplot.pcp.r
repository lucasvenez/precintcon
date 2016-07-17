#' @name pplot.pcp
#' @aliases pplot.pcp
#' @author Lucas Venezian Povoa \email{lucasvenez@@gmail.com} 
#' @title Plot Precipitation Concentration Period per Year 
#' @description Plots the Precipitation Concentration Period per year of a precipitation serie. 
#' @usage pplot.pcp(\dots, xlab = "Year", 
#'        ylab = "PCP", ylim = c(0,360), legend = NULL, fontsize = 10, 
#'        axis.text.color = "black", export = FALSE, export.name = "pcd_plot.png", 
#'        width = 8.6, height = 7.5, units = "cm") 
#' @param \dots a set of daily or monthly precipitation series.
#' @param xlab the text for the x axis. (Default value: "Year")
#' @param ylab the text for the y axis. (Default value: "PCD")
#' @param ylim the limits of the y axis. (Default value: c(0, 360))
#' @param legend the text vector for the legend items. If NULL the legends will 
#' be equals to the variable names. (Default value: NULL)
#' @param fontsize the font size value in pt. (Default value: 10)
#' @param axis.text.color the legend colors. (Default value: "black")
#' @param export the logical value for defining whether the graph should be export 
#' to a file or not. (Default value: FALSE)
#' @param export.name the text for defining the exported file name. It is only used 
#' if export = TRUE. (Default value: "pcd_plot.png")
#' @param width the number for defining the exported graph width. It is only used if 
#' export = TRUE. (Default value: 8.6)
#' @param height the number for defining the exported graph height. It is only used 
#' if export = TRUE. (Default value: 7.5)
#' @param units the text for defining the units of the height and width parameters. 
#' It is only used if export = TRUE. (Default value: "cm")
#' @seealso 
#' \code{\link{pcp}}
#' \code{\link{read.data}}
#' @examples 
#' ##
#' # Loading the daily precipitation serie.
#' data(daily)
#' 
#' ##
#' # Plotting PCP per year
#' pplot.pcp(daily)
#' @references Zhang LJ, Qian YF (2003) Annual distribution features of precipitation in China and their interannual variations. J Acta Meteorological Sinica 17:146-163
#' @keywords precipitation concentration period (PCP)
#' @export
pplot.pcp <- function(
  ..., 
  xlab            = "Year",
  ylab            = "PCP", 
  ylim            = c(0,360),
  legend          = NULL,
  fontsize        = 10, 
  axis.text.color = "black", 
  export          = FALSE, 
  export.name     = "pcd_plot.png", 
  width           = 8.6, 
  height          = 7.5, 
  units            = "cm"
) {
  
  l <- list(...)
  
  if (length(l) <= 0)
    stop("empty input data in precintcon.plot.spi function.")
  
  varl <- as.list(match.call()[1:length(l)+1])
  
  if (!is.null(legend) && length(varl) != length(legend))
    stop(paste("legend should has length equals to the number of input data. legend parameter length", 
               length(legend), ": number of input data", length(varl)))
  
  else if (!is.null(legend))
    varl <- as.list(legend)
  
  plotl <- mapply(FUN = function(x, y) 
    cbind(pcp(x), data.frame(dataset = as.character(y))), 
    l, varl, SIMPLIFY = FALSE)
  
  
  plotl <- do.call(rbind, plotl)
  
  plot <- ggplot(plotl, aes_string(x = "year", y = "pcp")) + 
            geom_bar(stat = "identity", position = "identity") +
            xlab(xlab) + ylab(ylab) + facet_grid(. ~ dataset, scales = "free_x")
  
  rm(plotl, varl)
  
  if (!export)
    print(plot)
  else
    ggsave(export.name, plot, width = width, height = height, units = units)
}