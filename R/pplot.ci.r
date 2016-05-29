#' @include precintcon.plot.lorenz.r
NULL

#' @export
pplot.ci <- function(
  ..., 
  interval        = 1,
  grouped         = FALSE, 
  xlab            = expression(sum(n[i]), i==1),
  ylab            = expression(sum(P[i]), i==1), 
  legend.title    = "Legend",
  legend          = NULL,
  fontsize        = 10, 
  axis.text.color = "black", 
  export          = FALSE, 
  export.name     = "lorenz_plot.png", 
  width           = 8.6, 
  height          = 7.5, 
  units            = "cm"
) {
  precintcon.plot.lorenz(..., interval = interval, grouped = grouped, 
                         xlab = xlab, ylab = ylab, legend.title = legend.title, legend = legend, 
                         fontsize = fontsize, axis.text.color = axis.text.color, export = export, 
                         export.name = export.name, width = width, height = height, units = units)
}