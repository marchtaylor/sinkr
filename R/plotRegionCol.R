
#' Add color to plot region
#'
#' @param col Color string or number. Color of plot region (Defaut: "grey90")
#' @param grid Logical. Add grid on top of plot region color (Default: FALSE)
#' @param grid.col color of grid lines (Default: "white")
#' @param ... Other arguments to pass to \code{\link[graphics]{grid}}
#'
#' @return adds color to plot region and optional grid
#' 
#' @importFrom graphics grid rect
#' 
#' 
#' @export
#'
#' @examples
#' plot(dist~speed, data=cars)
#' plotRegionCol()
#' points(dist~speed, data=cars, pch=21, bg="white")
#' 
#' # with grid
#' plot(dist~speed, data=cars)
#' plotRegionCol(grid=TRUE)
#' points(dist~speed, data=cars, pch=21, bg="cyan")
#' 
#' # with grid plus addition settings
#' plot(dist~speed, data=cars)
#' plotRegionCol(col="grey30", grid=TRUE, grid.col="grey50", lwd=0.5)
#' points(dist~speed, data=cars, pch=21, bg="white")
#' 
plotRegionCol <- function(col="grey90", grid=FALSE, grid.col="white", ...){
  usr <- par()$usr
  rect(usr[1], usr[3], usr[2], usr[4], col=col)
  if(grid){
    grid(col=grid.col, ...)
  }
}
