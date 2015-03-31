#' Make polygons from a matrix
#' 
#' \code{matrixPoly} creates a list of polygon coordinates given a matrix \code{z} and 
#' corresponding \code{x} and \code{y} coordinates for the dimensions of \code{z}.
#' 
#' @param x,y Optional vectors of values for matrix \code{z} rows (\code{x}) and columns 
#' (\code{y}). If excluded, the function assumes the values to be a evenly-spaced 
#' sequence from 0 to 1.
#' @param z A matrix
#' @param n An optional vector of element positions of \code{z} for polygon creation 
#' 
#' @return List of polygon coordinates for each element of \code{z}
#' 
#' @examples
#' # Make sythetic data
#' set.seed(1)
#' m=8
#' n=10
#' x <- seq(m)
#' y <- seq(n)
#' z <- matrix(runif(m*n), nrow=m, ncol=n)
#' 
#' # Ex 1 - add another image layer
#' image(x, y, z, col=grey.colors(20))
#' N <- sample(1:(m*n),20)
#' z2 <- NaN*z
#' z2[N] <- 1
#' image(x, y, z2, col=rgb(0,0,1,0.4), add=TRUE)
#' box()
#' 
#' # Ex 2 - add polygons
#' image(x, y, z, col=grey.colors(20))
#' poly <- matrixPoly(x, y, z=z, n=N)
#' sapply(poly, function(X){polygon(X, col=rgb(1,1,0,0.4), border=1)})
#' box()
#' 
#' # Ex 3 - add polygons to unequal grid
#' x2 <- cumsum(round(runif(m, min=1, max=10)))
#' y2 <- cumsum(round(runif(n, min=1, max=10)))
#' image(x2, y2, z, col=grey.colors(20))
#' poly <- matrixPoly(x2, y2, z=z, n=N)
#' sapply(poly, function(X){polygon(X, col=rgb(1,0,0,0.4), border=1)})
#' box()
#' 
#' @export
#' 
matrixPoly <- function(x, y, z, n=NULL){
	if(missing(z)) stop("Must define matrix 'z'")
	if(missing(n)) n=seq(z)
	if(missing(x)) x <- seq(0,1,,dim(z)[1])
	if(missing(y)) y <- seq(0,1,,dim(z)[2])
	poly <- vector(mode="list", length(n))
	for(i in seq(n)){
		ROW <- ((n[i]-1) %% dim(z)[1]) +1
		COL <- ((n[i]-1) %/% dim(z)[1]) +1

		dist.left <- (x[ROW]-x[ROW-1])/2
		dist.right <- (x[ROW+1]-x[ROW])/2
		if(ROW==1) dist.left <- dist.right
		if(ROW==dim(z)[1]) dist.right <- dist.left

		dist.down <- (y[COL]-y[COL-1])/2
		dist.up <- (y[COL+1]-y[COL])/2
		if(COL==1) dist.down <- dist.up
		if(COL==dim(z)[2]) dist.up <- dist.down
		
		xs <- c(x[ROW]-dist.left, x[ROW]-dist.left, x[ROW]+dist.right, x[ROW]+dist.right, x[ROW]-dist.left)
		ys <- c(y[COL]-dist.down, y[COL]+dist.up, y[COL]+dist.up, y[COL]-dist.down, y[COL]-dist.down)
		poly[[i]] <- data.frame(x=xs, y=ys)
	}
	return(poly)
}
