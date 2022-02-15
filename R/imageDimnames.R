#' Create an image of matrix or 2-D array with labels
#'
#' @param mat matrix or 2-D array
#' @param xlab label for x-axis
#' @param ylab label for y-axis
#' @param xaxisSide side for x-axis
#' @param yaxisSide side for y-axis 
#' @param ... other arguments to pass to image
#' @param axisLas text direction for axes
#' @param drawBorders logical. Include borders between grids.
#' @param borderCol if drawborders is true, color for borders
#' @param borderLty  if drawborders is true, line type for borders
#' @param borderLwd  if drawborders is true, line width for borders
#' @param addLabels add labels for matrix values
#' @param labels string of labels (default is taken from matrix)
#' @param labelCol color for grid labels 
#' @param labelFont font for grid labels
#'
#' @return
#' @export
#' 
#' @importFrom graphics image abline
#'
#' @examples
#' 
#' mat <- matrix(-8:7, 4, 4)
#' imageDimnames(mat)
#' imageDimnames(mat, labelCol = c(1,2)[(c(mat) < 0)+1])
#' imageDimnames(mat, labelCol = c(1,2)[(c(mat) < 0)+1], borderLwd = 2)
#' 
#' imageDimnames(volcano, drawBorders = FALSE, addLabels = FALSE, axisLas = 2)
#' 
#' tmp <- tapply(warpbreaks$breaks, warpbreaks[,-1], sum)
#' imageDimnames(tmp, labelCol = 4)
#' 
imageDimnames <- function(mat, 
  xlab = NULL, ylab = NULL, xaxisSide = 1, yaxisSide = 2,
  axisLas = 1,
  drawBorders = TRUE, borderCol = 1, borderLty = 1, borderLwd = 1,
  addLabels = TRUE, labels = sinkr::ac(c(mat)), labelCol = "black", labelFont = 1,
  ...){
  if(is.null(dimnames(mat))){
    dimnames(mat) <- list(seq(nrow(mat)), seq(ncol(mat)))
  }
  if(is.null(dimnames(mat)[[1]])){
    dimnames(mat)[[1]] <- seq(nrow(mat))
  }
  if(is.null(dimnames(mat)[[2]])){
    dimnames(mat)[[2]] <- seq(ncol(mat))
  }
  
  x <- seq(dimnames(mat)[[1]])
  y <- seq(dimnames(mat)[[2]])
  z <- mat
  if(is.null(xlab)) xlab = names(dimnames(mat))[1]
  if(is.null(ylab)) ylab = names(dimnames(mat))[2]
  if(is.null(xlab)) xlab = ""
  if(is.null(ylab)) ylab = ""
  
  image(x = x, y = y, z = z, axes = FALSE, xlab = xlab, ylab = ylab, ...)
  if(drawBorders){
    abline(h = seq(length(y))-0.5, 
      col = borderCol, lty = borderLty, lwd = borderLwd)
    abline(v = seq(length(x))-0.5, 
      col = borderCol, lty = borderLty, lwd = borderLwd)
    box(col = borderCol, lty = borderLty, lwd = borderLwd)
  }
  
  axis(side = xaxisSide, at = x, labels = dimnames(mat)[[1]], las = axisLas)
  axis(side = yaxisSide, at = y, labels = dimnames(mat)[[2]], las = axisLas)
  
  if(addLabels){
    txt <- expand.grid(x = x, y = y)
    txt$val <- c(z)
    text(x = txt$x, y = txt$y, labels = labels, 
      col = labelCol, font = labelFont)
  }
}
