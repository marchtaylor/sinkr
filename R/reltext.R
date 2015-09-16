#' @title Add text using relative x,y plot coordinates
#' @description \code{reltext} adds text using relative x,y plot coordinates. Wrapper for 
#' \code{\link[graphics]{text}}.
#'
#' @param relx,rely numeric vectors of coordinates where the text labels should be written.
#' @param labels label argument passed to code{\link[graphics]{text}}
#' @param ... other parameters passed to code{\link[graphics]{text}}
#' 
#' @keywords text, plotting
#' @export
#' @examples
#' # Make data
#' set.seed(1)
#' n <- 100
#' a <- 3
#' b <- 5
#' x <- rnorm(n)
#' y <- a + b*x + rnorm(n, sd=2)
#' fit <- lm(y ~ x)
#' 
#' # Plot with regression results in top left corner
#' op <- par(mar=c(3,3,0.5,0.5), mgp=c(1.5,0.5,0), tcl=-0.25, las=1, bty="l")
#' plot(x, y)
#' abline(fit, lty=2, col=4)
#' reltext(0, 0.95, pos=4, col=4,
#'    labels=bquote( italic(y) == .(sprintf("%0.2f", (coef(fit)[1])))+
#'      .(sprintf("%0.2f", (coef(fit)[2])))~"*"~italic(x) 
#'    )
#' )
#' reltext(0, 0.85, pos=4, col=4,
#'         labels=bquote( bolditalic(R)^2 == .(round(summary(fit)$adj.r.squared,2)) )
#' )
#' par(op)
#' 
#' # Plot multiple labels at once
#' plot(x, y)
#' abline(fit, lty=2, col=8)
#' reltext(c(0,0.95), c(0.95, 0.05), pos=c(4,3), col=c(3,4), labels=c("text1", "text2"))
#' 
reltext <- function(relx=0.5, rely=0.5, labels = seq_along(relx), ...){
  usr <- par("usr")
  x <- usr[1] + relx*(usr[2]-usr[1])
  y <- usr[3] + rely*(usr[4]-usr[3])
  text(x, y, labels, ...)
}





