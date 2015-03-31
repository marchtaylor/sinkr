#' @title Stacked plot
#' @description \code{plotStacked} makes a stacked plot where each \code{y} 
#' series is plotted on top of each other using filled polygons.
#' @param x A vector of values
#' @param y A matrix of data series (columns) corresponding to x
#' @param order.method Method of ordering y plotting order. One of the 
#'   following: \code{c("as.is", "max", "first")}. \code{"as.is"} - plot in 
#'   order of y column. \code{"max"} - plot in order of when each y series 
#'   reaches maximum value. \code{"first"} - plot in order of when each y series
#'   first value > 0.
#' @param col Fill colors for polygons corresponding to y columns (will recycle).
#' @param border Border colors for polygons corresponding to y columns (will recycle) (see ?polygon for details)
#' @param lwd Border line width for polygons corresponding to y columns (will recycle)
#' @param xlab x-axis labels
#' @param ylab y-axis labels
#' @param ylim y-axis limits. If \code{ylim=NULL}, defaults to \code{c(0, 1.2*max(apply(y,1,sum)}.
#' @param ... Other plot arguments
#' 
#' 
#' @examples
#' #Create data
#'set.seed(1)
#'m <- 500
#'n <- 30
#'x <- seq(m)
#'y <- matrix(0, nrow=m, ncol=n)
#'colnames(y) <- seq(n)
#'for(i in seq(ncol(y))){
  #'  mu <- runif(1, min=0.25*m, max=0.75*m)
  #'  SD <- runif(1, min=5, max=20)
  #'  TMP <- rnorm(1000, mean=mu, sd=SD)
  #'  HIST <- hist(TMP, breaks=c(0,x), plot=FALSE)
  #'  fit <- smooth.spline(HIST$counts ~ HIST$mids)
  #'  y[,i] <- fit$y
  #'}
#'y <- replace(y, y<0.01, 0)
#'
#'#Ex.1 : Color by max value)
#'pal <- colorRampPalette(c(rgb(0.85,0.85,1), rgb(0.2,0.2,0.7)))
#'BREAKS <- pretty(apply(y,2,max),8)
#'LEVS <- levels(cut(1, breaks=BREAKS))
#'COLS <- pal(length(BREAKS )-1)
#'z <- val2col(apply(y,2,max), col=COLS)
#'
#'#Create stacked plot (plot order = "as.is")
#'plotStacked(x,y, xlim=c(100, 400), ylim=c(0, 1.2*max(apply(y,1,sum), na.rm=TRUE)), 
#'yaxs="i", col=z, border="white", lwd=0.5)
#'
#'#Create stacked plot (plot order = "max")
#'plotStacked(x,y, xlim=c(100, 400), ylim=c(0, 1.2*max(apply(y,1,sum), na.rm=TRUE)), 
#'order.method="max", yaxs="i", col=z, border="white", lwd=0.5)
#'
#'#Ex. 2 : Color by first value
#'ord <- order(apply(y, 2, function(r) min(which(r>0))))
#'y2 <- y[, ord]
#'pal <- colorRampPalette(c("blue", "cyan", "yellow", "red"))
#'z <- pal(ncol(y2))
#'
#'#Create stacked plot (plot order = "as.is")
#'plotStacked(x,y2, xlim=c(100, 400), ylim=c(0, 1.2*max(apply(y2,1,sum), na.rm=TRUE)), 
#'yaxs="i", col=z, border=1, lwd=0.25)
#'
#'#Create stacked plot (plot order = "max")
#'plotStacked(x,y2, xlim=c(100, 400), ylim=c(0, 1.2*max(apply(y2,1,sum), na.rm=TRUE)), 
#'order.method="max", yaxs="i", col=z, border=1, lwd=0.25)
#'
#' @export
#' 
plotStacked <- function(
	x, y, 
	order.method="as.is",
	ylab="", xlab="", 
	border = NULL, lwd=1, 
	col=rainbow(length(y[1,])),
	ylim=NULL,
	...
){

	if(sum(y < 0) > 0) stop("y cannot contain negative numbers")

	if(is.null(border)) border <- par("fg")
	border <- as.vector(matrix(border, nrow=ncol(y), ncol=1))
	col <- as.vector(matrix(col, nrow=ncol(y), ncol=1))
	lwd <- as.vector(matrix(lwd, nrow=ncol(y), ncol=1))

  if(is.null(ylim)) ylim=c(0, 1.2*max(apply(y,1,sum)))
  
	if(order.method == "max") {
		ord <- order(apply(y, 2, which.max))
		y <- y[, ord]
		col <- col[ord]
		border <- border[ord]
	}

	if(order.method == "first") {
		ord <- order(apply(y, 2, function(x) min(which(x>0))))
		y <- y[, ord]
		col <- col[ord]
		border <- border[ord]
	}

	top.old <- x*0
	polys <- vector(mode="list", ncol(y))
	for(i in seq(polys)){
		top.new <- top.old + y[,i]
		polys[[i]] <- list(x=c(x, rev(x)), y=c(top.old, rev(top.new)))
		top.old <- top.new
	}

	if(is.null(ylim)) ylim <- range(sapply(polys, function(x) range(x$y, na.rm=TRUE)), na.rm=TRUE)
	plot(x,y[,1], ylab=ylab, xlab=xlab, ylim=ylim, t="n", ...)
	for(i in seq(polys)){
		polygon(polys[[i]], border=border[i], col=col[i], lwd=lwd[i])
	}

}
