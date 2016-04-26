#' EOF reconstruction (Empirical Orthogonal Functions analysis)
#' 
#' This function reconstructs the original field from an EOF object of the 
#' function \link[sinkr]{eof}.
#' 
#' @param EOF An object resulting from the function \link[sinkr]{eof}.
#' @param pcs The principal components (PCs) to use in the reconstruction
#'   (defaults to the full set of PCs: \code{pcs=seq(ncol(EOF$u))})
#' @param newpcs An (optional) matrix of new principal coordinates to use in the 
#' reconstruction. This would typically come from a gappy dataset whose missing 
#' values are to be predicted based on the EOF loadings of a the EOF object 
#' (see \link[sinkr]{eofPred}).
#' @param uncenter Logical. Should reconstructed matrix be un-centered (e.g. if 
#' \code{centered = TRUE} was used in link{eof}). Default is \code{TRUE}.
#' @param unscale Logical. Should reconstructed matrix be un-centered (e.g. if 
#' \code{scaled = TRUE} was used in link{eof}). Default is \code{TRUE}.
#' 
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
eofRecon <- function(EOF, pcs=NULL, newpcs=NULL, uncenter=TRUE, unscale=TRUE){

	if(is.null(pcs) & is.null(newpcs)){
	  pcs <- seq(ncol(EOF$u))
	}
	if(is.null(pcs) & !is.null(newpcs)){
	  pcs <- seq(ncol(newpcs))
	}

	#F1 reconstruction then reverse scale then reverse center
	if(is.null(newpcs)){
	  F1_recon <- EOF$A[,pcs] %*% t(EOF$u[,pcs])  
	}
	if(!is.null(newpcs)){
	  F1_recon <- newpcs[,pcs] %*% t(EOF$u[,pcs])  
	}
	
	# add center and scale attributes
	attr(F1_recon, "scaled:center") <- EOF$F1_center
  attr(F1_recon, "scaled:scale") <- EOF$F1_scale

  #un-center and un-scale
	if(uncenter | unscale){
	  F1_recon <- unscale(x=F1_recon, uncenter=uncenter, unscale=unscale)
	}

	return(F1_recon)

}

