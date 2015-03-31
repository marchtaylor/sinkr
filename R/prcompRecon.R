#' prcomp object reconstruction
#' 
#' This function reconstructs the original field from an EOF object of the 
#' function \code{\link[stats]{prcomp}}.
#' 
#' @param pca An object resulting from the function \code{\link[stats]{prcomp}}.
#' @param pcs The principal components ("PCs") to use in the reconstruction 
#' (defaults to the full set of PCs: \code{pcs=seq(pca$sdev))})
#' 
#' @examples
#' # prcomp
#' P <- prcomp(iris[,1:4])
#' 
#' # Full reconstruction
#' R <- prcompRecon(P)
#' plot(as.matrix(iris[,1:4]), R, xlab="original data", ylab="reconstructed data")
#' abline(0, 1, col=2)
#' 
#' # Partial reconstruction
#' RMSE <- NaN*seq(P$sdev)
#' for(i in seq(RMSE)){
#'   Ri <- prcompRecon(P, pcs=seq(i))
#'   RMSE[i] <- sqrt(mean((as.matrix(iris[,1:4]) - Ri)^2))
#' }
#' plot(RMSE, t="o", xlab="Number of pcs")
#' abline(h=0, lty=2)
#' 
#' @export
#' 
prcompRecon <- function(pca, pcs=NULL){
  if(is.null(pcs)) pcs <- seq(pca$sdev)
  recon <- as.matrix(pca$x[,pcs]) %*% t(as.matrix(pca$rotation[,pcs]))
  if(pca$scale[1] != FALSE){
  	recon <- scale(recon , center=FALSE, scale=1/pca$scale)
  }
  if(pca$center[1] != FALSE){
	recon <- scale(recon , center=-pca$center, scale=FALSE)
  }
  recon
}