#' Round to defined resolution increment
#' 
#' \code{round2res} rounds a value to a given resolution (e.g. increments of 0.5) rather than the typical
#' decimal place
#' 
#' @param val Vector. The values to be rounded
#' @param reso Numeric. The resolution or increment to use for rounding
#' 
#' @examples
#' set.seed(1)
#' n <- 10
#' x <- runif(n, min=0, max=20)
#' xr <- round2reso(x, 5) # rounded values to increments of 5
#' plot(x, t="n", ylim=c(0,20), pch=20)
#' abline(h=seq(0,20,5), col=8, lty=3)
#' points(x, col=1, pch=1)
#' points(xr, col=2, pch=20)
#' arrows(x0=seq(n), x1=seq(n), y0=x, y1=xr, length=0.1)
#' 
#' @export
#' 
round2reso <- function(val, reso){
	round(val*(1/reso), 0) / (1/reso)
}
