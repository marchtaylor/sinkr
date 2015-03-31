#' @title Make a color scale to accompany an image or other plot
#' @description The \code{imageScale} function is wrapper for 
#' \code{imageScale} and accepts the same arguments. 
#' It converts a vector of values (\code{z}) 
#' to a vector of color levels. One must define the number of colors. 
#' The limits of the color scale ("zlim") or the break points for the 
#' color changes("breaks") can also be defined. When breaks and zlim are 
#' defined, breaks overrides zlim. All arguments are similar to those in the
#' \code{image} function. Appearance is best when incorporated with \code{\link[graphics]{layout}}.
#' @param z A vector or matrix of values.
#' @param zlim Limits of the color scale values.
#' @param col Vector of color values (default is 12 colors 
#' from the \code{\link[grDevices]{heat.colors}} palette).
#' @param breaks Break points for color changes. If breaks is specified then \code{zlim} 
#' is unused and the algorithm used follows \code{\link[base]{cut}}, so intervals are 
#' closed on the right and open on the left except for the lowest interval 
#' which is closed at both ends.
#' @param axis.pos Position of the axis (1=bottom, 2=left, 3=top, 4=right) (default = 1).
#' @param add.axis Logical (TRUE/FALSE). Defines whether the axis is added (default: TRUE).
#' @param xlim Limits for the x-axis.
#' @param ylim Limits for the y-axis.
#' @param ... Additional graphical parameters to pass to the \code{\link[graphics]{image}} function.
#' 
#' @export
#' @examples
#' # Make color palettes
#' pal.1=colorRampPalette(c("green4", "orange", "red", "white"), space="rgb", bias=0.5)
#' pal.2=colorRampPalette(c("blue", "cyan", "yellow", "red", "pink"), space="rgb")
#' 
#' # Make images with corrsponding scales
#' op <- par(no.readonly = TRUE)
#' layout(matrix(c(1,2,3,0,4,0), nrow=2, ncol=3), widths=c(4,4,1), heights=c(4,1))
#' #layout.show(4)
#' #1st image
#' breaks <- seq(min(volcano), max(volcano),length.out=100)
#' par(mar=c(1,1,1,1))
#' image(seq(dim(volcano)[1]), seq(dim(volcano)[2]), volcano, 
#' col=pal.1(length(breaks)-1), breaks=breaks-1e-8, xaxt="n", yaxt="n", ylab="", xlab="")
#' #Add additional graphics
#' levs <- pretty(range(volcano), 5)
#' contour(seq(dim(volcano)[1]), seq(dim(volcano)[2]), volcano, levels=levs, add=TRUE)
#' #Add scale
#' par(mar=c(3,1,1,1))
#' imageScale(volcano, col=pal.1(length(breaks)-1), breaks=breaks-1e-8,axis.pos=1)
#' abline(v=levs)
#' box()
#' 
#' #2nd image
#' breaks <- c(0,100, 150, 170, 190, 200)
#' par(mar=c(1,1,1,1))
#' image(seq(dim(volcano)[1]), seq(dim(volcano)[2]), volcano, 
#' col=pal.2(length(breaks)-1), breaks=breaks-1e-8, xaxt="n", yaxt="n", ylab="", xlab="")
#' #Add additional graphics
#' levs=breaks
#' contour(seq(dim(volcano)[1]), seq(dim(volcano)[2]), volcano, levels=levs, add=TRUE)
#' #Add scale
#' par(mar=c(1,1,1,3))
#' imageScale(volcano, col=pal.2(length(breaks)-1), breaks=breaks-1e-8, 
#' axis.pos=4, add.axis=FALSE)
#' axis(4,at=breaks, las=2)
#' box()
#' abline(h=levs)
#' par(op)
#' 
#' 
imageScale <- function(z, zlim, col = heat.colors(12),
breaks, axis.pos=1, add.axis=TRUE, xlim=NULL, ylim=NULL, ...){
 if(!missing(breaks)){
  if(length(breaks) != (length(col)+1)){stop("must have one more break than colour")}
 }
 if(missing(breaks) & !missing(zlim)){
  breaks <- seq(zlim[1], zlim[2], length.out=(length(col)+1)) 
 }
 if(missing(breaks) & missing(zlim)){
  zlim <- range(z, na.rm=TRUE)
  breaks <- seq(zlim[1], zlim[2], length.out=(length(col)+1))
 }
 poly <- vector(mode="list", length(col))
 for(i in seq(poly)){
  poly[[i]] <- c(breaks[i], breaks[i+1], breaks[i+1], breaks[i])
 }
 if(axis.pos %in% c(1,3)){YLIM<-c(0,1); XLIM<-range(breaks)}
 if(axis.pos %in% c(2,4)){YLIM<-range(breaks); XLIM<-c(0,1)}
 if(!missing(ylim)){ YLIM <- ylim }
 if(!missing(xlim)){ XLIM <- xlim }

 plot(1, 1, t="n", ylim=YLIM, xlim=XLIM, axes=FALSE, xlab="", ylab="", xaxs="i", yaxs="i", ...)  
 for(i in seq(poly)){
  if(axis.pos %in% c(1,3)){
   polygon(poly[[i]], c(0,0,1,1), col=col[i], border=col[i])
  }
  if(axis.pos %in% c(2,4)){
   polygon(c(0,0,1,1), poly[[i]], col=col[i], border=col[i])
  }
 }
 box()
 if(add.axis) {axis(axis.pos)}
}
