#' Select colors visually
#' 
#' The \code{getcolors2} function allows one to select from a visual palette of 
#' 1200 colors (i.e. color combinations from 6 levels of red, green, and blue).
#' 
#' @param n Numeric value of the number of colors to select
#' 
#' @return A vector of hexadecimal codings for colors 
#' (as in \code{\link[grDevices]{rgb}}).
#' 
#' @importFrom graphics par layout image polygon mtext box locator lines axis plot
#' @importFrom grDevices rainbow
#' 
#' @examples
#' \donttest{
#' # Make synthetic data
#' set.seed(1)
#' n <- 100
#' x <- seq(n)
#' y1 <- cumsum(rnorm(n))
#' y2 <- cumsum(rnorm(n))
#' y3 <- cumsum(rnorm(n))
#' y4 <- cumsum(rnorm(n))
#' ylim <- range(c(y1,y2,y3,y4))
#' 
#' # Select colors
#' COLS <- getcolors2(4)
#' 
#' # Plot data with selected colors
#' plot(x, y1, ylim=ylim, t="l", col=COLS[1], lwd=3, ylab="")
#' lines(x, y2, col=COLS[2], lwd=3)
#' lines(x, y3, col=COLS[3], lwd=3)
#' lines(x, y4, col=COLS[4], lwd=3)
#' legend("topleft", legend=paste("y", 1:4, sep=""), col=COLS, lwd=3)
#' }
#' 
#' @export
#' 
getcolors2 <- function(n){
M <- 60
N <- M/3
hue <- col2rgb(rainbow(M))/255
sat <- data.frame(lower=c(rep(0,N/2), seq(0,1,,N/2)), upper=c(seq(0,1,,N/2), rep(1,N/2)))

RGB <- array(NaN, dim=c(M,N,3))
for(i in seq(M)){
  for(j in seq(N)){
    RGB[i,j,] <- newRange(hue[,i], new.min=sat$lower[j], new.max=sat$upper[j])
  }
}
X <- seq(M)
Y <- seq(N)

# plot
op <- par(no.readonly = TRUE)
layout(matrix(1:2, nrow=2, ncol=1), widths=c(6), heights=c(3,4), respect=FALSE)
par(mar=c(1,3,2,1))

COL <- array(rgb(RGB[,,1], RGB[,,2], RGB[,,3]), dim=dim(RGB)[1:2])
polys <- matrixPoly(X, Y, RGB[,,1])
image(X, Y, RGB[,,1], col=NA, xlab="", ylab="", xaxt="n", yaxt="n")
for(i in seq(polys)){
  polygon(polys[[i]], col=COL[i], border=COL[i])
}
mtext(paste("Click on", n, "colors [please]"), side=3, line=0.5)
box()

CC <- NA*seq(n)
for(i in seq(n)){
  print(paste0("Click on color #", i))
  coord <- locator(1)
  CC[i] <- COL[round(coord$x), round(coord$y)]
}

par(mar=c(1,3,0,1))
pal <- colorRampPalette(c("black", "white"))
image(x=1:100, y=seq(n), z=matrix(rep(1:100,n), nrow=100, ncol=n), col=pal(100), xlab="", ylab="", xaxt="n", yaxt="n")
box()
for(i in seq(n)){
  lines(x=c(1,100), y=c(i,i), col=CC[i], lwd=4)
}
axis(2, at=seq(n))

par(op)
CC
}
