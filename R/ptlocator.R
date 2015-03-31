#' @title Data point locator
#' @description the \code{ptlocator} function allows for the selection of data points in 
#' an open graphical device via the \code{\link[graphics]{locator}} function. Closest data points
#' to the selected positions are determined via Euclidean distance following a scaling of x and y 
#' axes in order to give them equal weighting and remove the influence of differing units or ranges. 
#' Colored points are filled in for the data point that has the lowest distance to the 
#' clicked location, and the result gives the vector positions of the closest x, y data points.
#' 
#' @param n Number of points to select
#' @param x Vector of x-axis values for the plotted data
#' @param y Vector of y-axis values for the plotted data
#' @param col Colors to use for plotting closest data points
#' @param pch pch values to use for plotting closest data points
#' @param ... additional parameters for plotting closest data points
#' 
#' @return A vector of point indices
#' 
#' @examples
#' \donttest{set.seed(1)
#' n <- 200
#' x <- sort(runif(n, min=0, max=10*pi))
#' y <- sin(x) + rnorm(n, sd=0.2)
#' 
#' # Select 10 points at maxima and minima 
#' plot(x, y)
#' pos <- ptlocator(10, x, y)
#' pos}
#' 
#' @export
#' 
ptlocator <- function(n=1, x, y, col=rgb(1,0,0,0.5), pch=20, ...){
  xsc <- scale(x)
  ysc <- scale(y)
  pos <- seq(n)*NaN
  for(i in seq(n)){
    print(paste("choose point", i))
    pt <- locator(1)
    ptxsc <- scale(pt$x, center=attr(xsc,"scaled:center"), scale=attr(xsc,"scaled:scale"))
    ptysc <- scale(pt$y, center=attr(ysc,"scaled:center"), scale=attr(ysc,"scaled:scale"))
    pos.i <- which.min(sqrt((c(ptxsc)-c(xsc))^2 + (c(ptysc)-c(ysc))^2))
    points(x[pos.i], y[pos.i], col=col, pch=pch, ...)
    pos[i] <- pos.i
  }
  pos    
}