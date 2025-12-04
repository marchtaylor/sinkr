#' prcomp object reconstruction
#' 
#' This function reconstructs the original field from an EOF object of the 
#' function \code{\link[stats]{prcomp}}.
#' 
#' @param pca An object resulting from the function \code{\link[stats]{prcomp}}.
#' @param pcs The principal components ("PCs") to use in the reconstruction 
#' (defaults to the full set of PCs: \code{pcs=seq(pca$sdev))})
#' @param unscale logical. Should reconstruction be unscaled 
#'   (reverses scaling; Default: `unscale = TRUE`).
#' @param uncenter logical. Should reconstruction be uncentered 
#'   (reverses centering; Default: `uncenter = TRUE`).
#' 
#' @examples
#' # prcomp
#' P <- prcomp(iris[,1:4])
#' 
#' # Full reconstruction
#' R <- prcompRecon(P)
#' plot(as.matrix(iris[,1:4]), R, xlab = "original data", 
#'   ylab = "reconstructed data")
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
prcompRecon <- function(pca, pcs = NULL, unscale = TRUE, uncenter = TRUE){
  if(is.null(pcs)) pcs <- seq(pca$sdev)
  recon <- as.matrix(pca$x[,pcs]) %*% t(as.matrix(pca$rotation[,pcs]))
  
	# add center and scale attributes
  if(pca$center[1] != FALSE){attr(recon, "scaled:center") <- pca$center}
  if(pca$scale[1] != FALSE){attr(recon, "scaled:scale") <- pca$scale}
  
  # uncenter and unscale
	if(unscale | uncenter){
	  recon <- unscale(x = recon, unscale = unscale, uncenter = uncenter)
	}

  recon
}