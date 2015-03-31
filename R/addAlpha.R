#' @title Add alpha channel (transparency) to colors
#' @description Takes a vector of colors and adds an alpha channel 
#' at the given level of transparency.
#' @param COLORS Vector of any of the three kinds of R color specifications, 
#' i.e., either a color name (as listed by colors()), 
#' a hexadecimal string of the form "#rrggbb" or "#rrggbbaa" (see rgb), 
#' or a positive integer i meaning palette()[i].
#' @param ALPHA A value (between 0 and 1) indicating the alpha channel
#' (opacity) value.
#' 
#' @examples
#'# Make background image
#'x <- seq(-180, 180,, 30)
#'y <- seq(-90, 90,, 30)
#'grd <- expand.grid(x=x,y=y)
#'z <- sqrt(grd$x^2+grd$y^2)
#'dim(z) <- c(length(x), length(y))
#'pal <- colorRampPalette(c(rgb(1,1,1), rgb(0,0,0)))
#'COLORS <- pal(20)
#'image(x,y,z, col=COLORS)
#'
#'# Add semi-transparent layer
#'z2 <- grd$x^2+grd$y
#'dim(z2) <- c(length(x), length(y))
#'pal <- colorRampPalette(c(rgb(0.5,1,0), rgb(0,1,1), rgb(1,1,1)))
#'COLORS <- addAlpha(pal(20), 0.4) # alpha chanel equals 0.4
#'image(x,y,z2, col=COLORS, add=TRUE)
#'
#' @keywords color
#'
#' @export

addAlpha <- function(COLORS, ALPHA){
	if(missing(ALPHA)) stop("provide a value for alpha between 0 and 1")
	RGB <- col2rgb(COLORS, alpha=TRUE)
	RGB[4,] <- round(RGB[4,]*ALPHA)
	NEW.COLORS <- rgb(RGB[1,], RGB[2,], RGB[3,], RGB[4,], maxColorValue = 255)
	return(NEW.COLORS)
}