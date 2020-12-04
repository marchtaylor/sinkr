#' Add border to plotted text
#'
#' @param x,y numeric vectors of coordinates where the text labels should 
#'   be written. If the length of x and y differs, the shorter one is recycled.
#' @param labels a character vector or expression specifying the text to be 
#'   written. An attempt is made to coerce other language objects 
#'   (names and calls) to expressions, and vectors and other classed objects 
#'   to character vectors by as.character. If labels is longer than x and y, 
#'   the coordinates are recycled to the length of labels.
#' @param col the color of the text.
#' @param bg the color of the text border
#' @param theta a sequence of angles be which to cycle around the text. 
#'   (Default: \code{theta = seq(0, 2*pi, length.out = 50)}) 
#' @param r the width of the border (fraction of character width).
#' @param ... other pars passed to \code{\link[graphics]{text}}.
#'
#' @return lower level plotting command to add text to existing device.
#' 
#' @references  \url{https://stackoverflow.com/a/25632536/1199289}
#' 
#' @importFrom graphics strwidth strheight
#' @importFrom grDevices xy.coords
#' 
#' @export
#'
#' @examples
#' 
#' plot(c(0,1), c(0,1), type="n", lwd=20, axes=FALSE, xlab="", ylab="")
#' 
#' rect(xleft = 0.5, xright = 1, ybottom = 0, ytop = 1, col=1)
#' text(1/6, 1/6, "Test 1")
#' shadowtext(2/6, 2/6, "Test 2", col='red', bg="blue")
#' shadowtext(3/6, 3/6, "Test 3", cex=2)
#' 
#' # `r` controls the width of the border
#' shadowtext(5/6, 5/6, "Test 4", col="black", bg="white", cex=4, r=0.2)
#' 
shadowtext <- function(x, y = NULL, labels, col = 'white', bg = 'black', 
  theta = seq(0, 2*pi, length.out = 50), r = 0.1, ...){

    xy <- xy.coords(x, y) # coordinates for text
    xo <- r*strwidth('A') # character border width in plot units
    yo <- r*strheight('A') # character border height in plot units

    # draw background text with small shift in x and y in background colour
    for (i in theta) {
        text(x = xy$x + cos(i)*xo, y = xy$y + sin(i)*yo, labels = labels, 
          col = bg, ...)
    }
    
    # draw actual text in exact xy position in foreground colour
    text(x = xy$x, y = xy$y, labels = labels, col = col, ...)
}