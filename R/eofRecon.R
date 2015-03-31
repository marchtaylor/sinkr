#' EOF reconstruction (Empirical Orthogonal Functions analysis)
#' 
#' This function reconstructs the original field from an EOF object of the 
#' function \code{eof}.
#' 
#' @param EOF An object resulting from the function \code{eof}.
#' @param pcs The principal components (PCs) to use in the reconstruction
#'   (defaults to the full set of PCs: \code{pcs=seq(ncol(EOF$u))})
#'   
#' @examples
#' set.seed(1)
#' iris.gappy <- as.matrix(iris[,1:4])
#' iris.gappy[sample(length(iris.gappy), 0.25*length(iris.gappy))] <- NaN
#' Er <- eof(iris.gappy, method="svd", recursive=TRUE) # recursive (RSEOF)
#' Enr <- eof(iris.gappy, method="svd", recursive=FALSE) # non-recursive (LSEOF)
#' iris.gappy.recon.r <- eofRecon(Er)
#' iris.gappy.recon.nr <- eofRecon(Enr)
#' 
#' # Reconstructed values vs. observed values
#' op <- par(mfrow=c(1,2))
#' lim <- range(iris.gappy, na.rm=TRUE)
#' plot(iris.gappy, iris.gappy.recon.r, 
#' col=c(2:4)[iris$Species], main="recursive=TRUE", xlim=lim, ylim=lim)
#' abline(0, 1, col=1, lwd=2)
#' plot(iris.gappy, iris.gappy.recon.nr, 
#' col=c(2:4)[iris$Species], main="recursive=FALSE", xlim=lim, ylim=lim)
#' abline(0, 1, col=1, lwd=2)
#' par(op)
#' 
#' # Reconstructed values from gappy data vs. all original values
#' op <- par(mfrow=c(1,2))
#' plot(as.matrix(iris[,1:4]), iris.gappy.recon.r, 
#' col=c(2:4)[iris$Species], main="recursive=TRUE")
#' abline(0, 1, col=1, lwd=2) 
#' plot(as.matrix(iris[,1:4]), iris.gappy.recon.nr, 
#' col=c(2:4)[iris$Species], main="recursive=FALSE")
#' abline(0, 1, col=1, lwd=2) 
#' 
#' @export
#' 
eofRecon <- function(EOF, pcs=NULL){

	F1_center=EOF$F1_center
	F1_scale=EOF$F1_scale
	F1_cols_incl=EOF$F1_cols_incl

	if(is.null(pcs)){pcs=seq(ncol(EOF$u))}

	#F1 reconstruction then reverse scale then reverse center
	F1_recon <- EOF$A[,pcs] %*% t(EOF$u[,pcs])

	if(!is.null(F1_scale)){
		F1_recon <- scale(F1_recon, center=FALSE, scale=1/F1_scale)
	}
	if(!is.null(F1_center)){
		F1_recon <- scale(F1_recon, center=-1*F1_center, scale=FALSE)
	}

	F1_recon

}

