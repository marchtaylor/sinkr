#' @title Stream plot
#' @description \code{plotStream} makes a "stream plot" where each \code{y} 
#' series is plotted as stacked filled polygons on 
#' alternating sides of a baseline. A random wiggle is applied through the
#' arguments \code{frac.rand} and \code{spar} such that each plot will
#' be different unless preceeded by a random seed (e.g. \code{set.seed(1)}).
#' @param x A vector of values
#' @param y A matrix of data series (columns) corresponding to x
#' @param order.method Method of ordering y plotting order. One of the 
#'   following: \code{c("as.is", "max", "first")}. \code{"as.is"} - plot in 
#'   order of y column. \code{"max"} - plot in order of when each y series 
#'   reaches maximum value. \code{"first"} - plot in order of when each y series
#'   first value > 0.
#' @param center Logical. If TRUE, the stacked polygons will be centered 
#' so that the middle, i.e. baseline (\code{g0}), of the stream is 
#' approximately equal to zero. Centering is done before the 
#' addition of random wiggle to the baseline. 
#' @param frac.rand Fraction of the overall data "stream" range used to 
#' define the range of random wiggle (uniform distrubution) 
#' to be added to the baseline \code{g0}
#' @param spar Setting for smooth.spline function to make a smoothed version of 
#' baseline \code{"g0"}
#' @param col Fill colors for polygons corresponding to y columns (will recycle).
#' @param border Border colors for polygons corresponding to y columns (will recycle)
#' (see \code{?polygon} for details)
#' @param lwd Border line width for polygons corresponding to y columns (will recycle)
#' @param xlab x-axis labels
#' @param ylab y-axis labels
#' @param ylim y-axis limits. If \code{ylim=NULL}, defaults to 
#' \code{c(-0.7, 0.7)*max(apply(y,1,sum))}
#' @param ... Other plot arguments
#' 
#' @return A plot with stream visualization added
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
#'#Plot order = "as.is"
#'plotStream(x,y, xlim=c(100, 400), center=TRUE, spar=0.3, frac.rand=0.2, 
#'col=z, border="white", lwd=0.5)
#'
#'#Plot order = "max"
#'plotStream(x,y, xlim=c(100, 400), center=TRUE, order.method="max", spar=0.3, 
#'frac.rand=0.2, col=z, border="white", lwd=0.5)
#'
#'#Ex. 2 : Color by first value
#'ord <- order(apply(y, 2, function(r) min(which(r>0))))
#'y2 <- y[, ord]
#'pal <- colorRampPalette(c("blue", "cyan", "yellow", "red"))
#'z <- pal(ncol(y2))
#'
#'#Plot order = "as.is"
#'plotStream(x,y2, xlim=c(100, 400), center=FALSE, spar=0.1, frac.rand=0.05, 
#'col=z, border=1, lwd=0.25)
#'
#'#Plot order = "max"
#'plotStream(x,y2, xlim=c(100, 400), center=FALSE, order.method="max", spar=0.1, 
#'frac.rand=0.05, col=z, border=1, lwd=0.25)
#'
#'#Extremely wiggly, no borders, no box, no axes, black background
#'op <- par(bg=1, mar=c(0,0,0,0))
#'plotStream(x,y2, xlim=c(100, 400), center=FALSE, spar=0.3, frac.rand=1, col=z, 
#'border=NA, axes=FALSE)
#'par(op)
#'
#' @export
#' 
plotStream <- function(
	x, y, 
	order.method = "as.is", frac.rand=0.1, spar=0.2,
	center=TRUE,
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

	if(is.null(ylim)) ylim=c(-0.7*max(apply(y,1,sum)), 0.7*max(apply(y,1,sum)))
	                                    
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

	bottom.old <- x*0
	top.old <- x*0
	polys <- vector(mode="list", ncol(y))
	for(i in seq(polys)){
		if(i %% 2 == 1){ #if odd
			top.new <- top.old + y[,i]
			polys[[i]] <- list(x=c(x, rev(x)), y=c(top.old, rev(top.new)))
			top.old <- top.new
		}
		if(i %% 2 == 0){ #if even
			bottom.new <- bottom.old - y[,i]
			polys[[i]] <- list(x=c(x, rev(x)), y=c(bottom.old, rev(bottom.new)))
			bottom.old <- bottom.new
		}
	}

	ylim.tmp <- range(sapply(polys, function(x) range(x$y, na.rm=TRUE)), na.rm=TRUE)
	outer.lims <- sapply(polys, function(r) rev(r$y[(length(r$y)/2+1):length(r$y)]))
	mid <- apply(outer.lims, 1, function(r) mean(c(max(r, na.rm=TRUE), min(r, na.rm=TRUE)), na.rm=TRUE))
	
	#center and wiggle
	if(center) {
		g0 <- -mid + runif(length(x), min=frac.rand*ylim.tmp[1], max=frac.rand*ylim.tmp[2])
	} else {
		g0 <- runif(length(x), min=frac.rand*ylim.tmp[1], max=frac.rand*ylim.tmp[2])
	}
	
	fit <- smooth.spline(g0 ~ x, spar=spar)

	for(i in seq(polys)){
		polys[[i]]$y <- polys[[i]]$y + c(fit$y, rev(fit$y))
	}

	if(is.null(ylim)) ylim <- range(sapply(polys, function(x) range(x$y, na.rm=TRUE)), na.rm=TRUE)
	plot(x,y[,1], ylab=ylab, xlab=xlab, ylim=ylim, t="n", ...)
	for(i in seq(polys)){
		polygon(polys[[i]], border=border[i], col=col[i], lwd=lwd[i])
	}

}
