#' Make a sprirograph-like design
#' 
#' \code{spirographR} will produce sprirograph-like design as either 
#' a hypotrochoid or an epitrochoid (depending on whether radius A or B is larger).
#' 
#' @param x,y Center coordinates of stationary circle 'a' 
#' @param a.rad Radius of stationary circle 'a'
#' @param b.rad Radius of circle 'b' travelling around stationary circle 'a'
#' @param bc Distance from the center of 'b' to a point 'c' which will turn with b as if
#' attached to a stick.
#' @param rev Number of revolutions that 'b' should travel around 'a'
#' @param n.per.rev Number of radial increments to be calculated per revolution
#' 
#' @details
#' A positive value for 'b' will result in a epitrochoid, 
#' while a negative value will result in a hypotrochoid. 
#' 
#' @examples
#' op <- par(mar=c(0,0,0,0), bg=1)
#' plot(spirographR(), t="l", col=6, lwd=3)
#' plot(spirographR(a.rad=1, b.rad=3.5, rev=7), t="l", col=7, lwd=3)
#' plot(spirographR(a.rad=4.1, b.rad=-6, rev=100, bc=2.3), t="l", col=5, lwd=1)
#' par(op)
#' 
#' @export
#' 
spirographR <- function(
  x=0,
  y=0,
  a.rad=1,
  b.rad=-4, 
  bc=-2, 
  rev=4, 
  n.per.rev=360
){

	b.cen.start <- list(x=0, y=y+a.rad+b.rad) # Starting position of B circle
	a.angle <- seq(0, 2*pi*rev,, rev*n.per.rev) # Radians around A for calculation
	a.cir <- 2*pi*a.rad # Circumference of A
	b.cir <- 2*pi*b.rad # Circumference of B

	###Find position of b.cen
	b.cen <- c()
	hyp <- a.rad + b.rad
	adj <- sin(a.angle) * hyp
	opp <- cos(a.angle) * hyp
	b.cen$x <- x + adj 
	b.cen$y <- y + opp 

	###Find position of c.point
	c.point <- c()
	a.cir.dist <- a.cir * a.angle / (2*pi)
	b.point.angle <- a.cir.dist / b.cir * 2*pi
	hyp <- bc
	adj <- sin(b.point.angle) * hyp
	opp <- cos(b.point.angle) * hyp
	c.point$x <- b.cen$x + adj 
	c.point$y <- b.cen$y + opp 

	###Return trajectory of C
	c.point
}


