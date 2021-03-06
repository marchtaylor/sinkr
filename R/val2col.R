#' @title Convert values to color levels
#' @description The \code{val2col} function converts a vector of values("z") 
#' to a vector of color levels. One must define the number of colors. 
#' The limits of the color scale ("zlim") or the break points for the 
#' color changes("breaks") can also be defined. When breaks and zlim are 
#' defined, breaks overrides zlim. All arguments are similar to those in the
#' \code{image} function.
#' @param z A vector of values (default is 12 colors from the \code{\link[grDevices]{hcl.colors}} palette).
#' @param zlim Limits of the color scale values.
#' @param col Vector of color values
#' @param breaks Break points for color changes. If breaks is specified then \code{zlim} 
#' is unused and the algorithm used follows \code{\link[base]{cut}}, so intervals are 
#' closed on the right and open on the left except for the lowest interval 
#' which is closed at both ends.
#' 
#' @importFrom grDevices adjustcolor hcl.colors
#' @importFrom stats na.exclude
#' @export
#' @examples
#' set.seed(1)
#' n <- 250
#' x <- seq(n)
#' y <- rnorm(n)
#' 
#' # Use all levels, evenly distributed breaks
#' Col <- val2col(y, col=rainbow(20))
#' plot(x,y, pch=21, bg=Col)
#' 
#' # Use limits, evenly distributed breaks
#' pal <- colorRampPalette(c("blue", "cyan", "yellow", "red"))
#' Col <- val2col(y, zlim=c(-1,1), col=pal(20))
#' plot(x,y, pch=21, bg=Col)
#' abline(h=c(-1,1), col=8, lty=2)
#' 
#' # Use custom breaks (break vector must have one more break than color)
#' Col <- val2col(y, col=topo.colors(6), breaks=c(-Inf, -2, -1, 0, 1, 2, Inf))
#' plot(x,y, pch=21, bg=Col)
#' abline(h=c(-Inf, -2, -1, 0, 1, 2, Inf), col=8, lty=2)
#' 
#' # by default, image() and val2col() use the same palette
#' grd <- expand.grid(x = seq(0, 1, length.out = nrow(volcano)), 
#'   y = seq(0, 1, length.out = ncol(volcano)))
#' grd$z <- c(volcano)
#' image(volcano)
#' points(grd, pch = 20, col = val2col(grd$z), cex = 1)
#' 
val2col<-function(z, zlim, col = hcl.colors(12, "YlOrRd", rev = TRUE), breaks){
 if(!missing(breaks)){
  if(length(breaks) != (length(col)+1)){stop("must have one more break than color")}
 }
 if(missing(breaks) & !missing(zlim)){
  breaks <- seq(zlim[1], zlim[2], length.out=(length(col)+1)) 
 }
 if(missing(breaks) & missing(zlim)){
  zlim <- range(z, na.rm=TRUE)
  breaks <- seq(zlim[1], zlim[2], length.out=(length(col)+1))
 }
 CUT <- cut(z, breaks=breaks, include.lowest = TRUE)
 colorlevels <- col[match(CUT, levels(CUT))] # assign colors to heights for each point
 return(colorlevels)
}