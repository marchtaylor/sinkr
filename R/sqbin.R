#' Square binning of xy data
#'
#' The \code{sqbin} function calculates frequencies in xy bins
#' 
#' @param x a vector of x values
#' @param y a vector of y values
#' @param xint a vector of x intervals (end values). Defaults to values as specified
#' by \code{pretty(x, n=nxbin)}
#' @param yint a vector of y intervals (end values). Defaults to values as specified
#' by \code{pretty(y, n=nybin)}
#' @param nxbin number of x bins. Default=50. Not used when \code{xint}
#' is defined.
#' @param nybin number of y bins. Default=50. Not used when \code{yint}
#' is defined.
#' 
#' @return A matrix containing interval mid points (\code{x,y}), bin frequencies (\code{z}), 
#' and intervals (\code{xint, yint}). 
#'  
#' @examples
#' # Synthetic data
#' set.seed(1)
#' n <- 1e6
#' x <- runif(n, min=-3, max=3)
#' y <- 4*x^2 + rnorm(n, sd=5)
#' 
#' sqbin.res <- sqbin(x,y)
#' 
#' # Plot
#' op <- par(mar=c(4,4,1,1))
#' image(sqbin.res, col=jetPal(20))
#' par(op)
#' 
#' # Plot with legend
#' op <- par(no.readonly = TRUE)
#' lo <- matrix(1:2, nrow=1, ncol=2)
#' layout(lo, widths=c(4,1), heights=c(4), respect=TRUE)
#' par(cex=1)
#' par(mar=c(3,3,1,1))
#' image(sqbin.res, col=jetPal(20))
#' par(mar=c(3,0,1,3))
#' imageScale(sqbin.res$z, col=jetPal(20), axis.pos=4)
#' par(op)
#' 
#' @export
#'
sqbin <- function(x, y, xint=NULL, yint=NULL, nxbin=50, nybin=50){
  if(is.null(xint)){xint <- pretty(x, n=nxbin)}
  if(is.null(yint)){yint <- pretty(y, n=nybin)}
  z <- matrix(0, nrow=length(xint)-1, ncol=length(yint)-1)
  xbin <- cut(x, xint)
  ybin <- cut(y, yint)
  tmp <- data.frame(row=match(xbin, levels(xbin)), col=match(ybin, levels(ybin)))
  tmp$pos <- ((tmp$col-1)*dim(z)[1])+tmp$row
  zbincount <- table(tmp$pos)
  z[as.numeric(rownames(zbincount))] <- zbincount
  z[z==0] <- NaN
  list(
    x=xint[-length(xint)]+(diff(xint)/2),
    y=yint[-length(yint)]+(diff(yint)/2),
    z=z,
    xint=xint,
    yint=yint
  )
}
