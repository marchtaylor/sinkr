#' Rotate a matrix by 90°
#'
#' @param mat matrix to be rotated.
#'
#' @return matrix
#' @export
#'
#' @examples
#' op <- par(mfcol = c(1,2))
#' image(volcano)
#' image(rotate(volcano))
#' par(op)
#' 
rotate <- function(mat){
  t(mat[nrow(mat):1,,drop=FALSE])
}
