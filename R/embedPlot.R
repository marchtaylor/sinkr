#' Embed a new plot in a relative position of an existing plot region
#'
#' @param expr expression. Defines a call to a new plot (must contain a 
#'   high-level call). Use curly brackets to contain multiple line evaluations.
#'   Other `par()` changes should be done outside of the call to `embedPlot`.
#' @param at numeric vector. Four values between 0-1 
#'   (`at = c(x0, x1, y0, y1)`), defining the region of the new plot relative 
#'   to the original plot region.  
#'
#' @return An embedded plot in a relative position of the existing plot.
#' 
#' @references Inspired by Stackoverflow answer by Allan Cameron: 
#'   (\url{https://stackoverflow.com/a/63114977/1199289})
#' 
#' @export
#'
#' @examples
#' 
#' # error given when expression argument missing
#' image(volcano)
#' embedPlot()
#' 
#' # addition of histogram
#' image(volcano)
#' embedPlot(
#'   expr = expression(hist(c(volcano), main = "", xlab = "")), 
#'   at = c(0.6, 0.95, 0.6, 0.95))
#' 
#' # addition of elevation scale
#' image(volcano)
#' embedPlot(
#'   expr = expression({
#'     imageScale(volcano, axis.pos = 1)
#'     mtext("Elevation [m]", side = 1, line = 2)}), 
#'   at = c(0.5, 0.95, 0.9, 0.95))
#' 
#' # addition of elevation scale and profile
#' # Note that yyou may need to add a background box manually
#' image(volcano, xlab = "X", ylab = "Y")
#' embedPlot(
#'   expr = expression({
#'     imageScale(volcano, axis.pos = 1)
#'     mtext("Elevation [m]", side = 1, line = 2)}), 
#'   at = c(0.5, 0.95, 0.9, 0.95))
#'   
#' abline(v = 0.2, lty = 2, lwd = 2)
#' polygon(x = c(0.4,0.96,0.96,0.4), y = c(0.01,0.01,0.52,0.52), col = "white")
#' embedPlot(
#'   expr = expression({
#'     plot(x = seq(0, 1, length.out = nrow(volcano)), 
#'       y = volcano[,round(ncol(volcano)*0.2)], xlab = "Y", ylab = "Elevation", bg = "white")
#'   }), 
#'   at = c(0.5, 0.95, 0.1, 0.5))
#' box() # should be back at the full plot region
#'   
#' 
#' # example mixing log scales, and demo of par changes
#' set.seed(1)
#' x = runif(100, min = 1, max = 1e5)
#' y = runif(100, min = 1, max = 1e5)
#' plot(x, y, log = "xy", xlab = "", ylab = "", col = 2)
#' tmp <- par(ps = 8, no.readonly = TRUE)
#' embedPlot(expression(plot(x, y, xlab = "", ylab = "", cex = 0.5)), 
#'   at = c(0.1, 0.5, 0.2, 0.5))
#' par(tmp)
#' embedPlot(expression(plot(x, y, xlab = "", ylab = "", cex = 0.5, 
#'   log = "xy")), at = c(0.1, 0.5, 0.65, 0.95))
#' 
#' 
#' 
embedPlot <- function(
  expr = expression({
    plot(1, t = "n", axes = FALSE, xlab = "", ylab = "")
    text(1,1, font = 3, 
      labels = "embedPlot error:\nexpression\nnot defined")}), 
  at = c(0.5, 0.95, 0.6, 0.95)
){

  # helper function to simplify coordinate conversions
  space_convert <- function(vec1, vec2){
    vec1[1:2] <- vec1[1:2] * diff(vec2)[1] + vec2[1]
    vec1[3:4] <- vec1[3:4] * diff(vec2)[3] + vec2[3]
    vec1}
  
  # Only write to par once for drawing insert plot: change back afterwards
  op <- par(no.readonly = T) 
  plt <- op$"plt"
  plt_space <- space_convert(at, plt)
  par(plt = plt_space, new = TRUE)

  # evaluate expression
  eval(expr = expr)
  
  par(op)
}


