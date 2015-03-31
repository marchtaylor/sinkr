#' @title Exponentiation of a matrix
#' @description The expmat function performs can calculate the pseudoinverse (i.e. "Moore-Penrose pseudoinverse") 
#' of a matrix (\code{EXP=-1}) and other exponents of matrices, 
#' such as square roots (\code{EXP=0.5}) or square root of its inverse (\code{EXP=-0.5}). 
#' The function arguments are a matrix (\code{MAT}), an exponent (\code{EXP}), and a tolerance
#' level for non-zero singular values. 
#' The function follows three steps: 
#' 1) Singular Value Decomposition (SVD) of the matrix; 
#' 2) Exponentiation of the singular values; 
#' 3) Re-calculation of the matrix with the new singular values
#' 
#' @param MAT A matrix.
#' @param EXP An exponent to apply to the matrix \code{MAT}.
#' @param tol Tolerance level for non-zero singular values.
#' 
#' @return A matrix
#' 
#' @examples
#' # Example matrix from Wilks (2006)
#' A <- matrix(c(185.47,110.84,110.84,77.58),2,2)
#' A
#' solve(A) #inverse
#' expmat(A, -1) # pseudoinverse
#' expmat(expmat(A, -1), -1) #inverse of the inverse -return to original A matrix
#' expmat(A, 0.5) # square root of a matrix
#' expmat(A, -0.5) # square root of its inverse
#' expmat(expmat(A, -1), 0.5) # square root of its inverse (same as above)
#' 
#' # Pseudoinversion of a non-square matrix
#' set.seed(1)
#' D <- matrix(round(runif(24, min=1, max=100)), 4, 6)
#' D
#' expmat(D, -1)
#' expmat(t(D), -1)
#' 
#' # Pseudoinversion of a square matrix
#' set.seed(1)
#' D <- matrix(round(runif(25, min=1, max=100)), 5, 5)
#' solve(D)
#' expmat(D, -1)
#' solve(t(D))
#' expmat(t(D), -1)
#' 
#' ### Examples from "corpcor" package manual
#' # a singular matrix
#' m = rbind(
#'   c(1,2),
#'   c(1,2)
#' )
#' 
#' # not possible to invert exactly
#' # solve(m) # produces an error
#' p <- expmat(m, -1)
#' 
#' # characteristics of the pseudoinverse
#' zapsmall( m %*% p %*% m )  ==  zapsmall( m )
#' zapsmall( p %*% m %*% p )  ==  zapsmall( p )
#' zapsmall( p %*% m )  ==  zapsmall( t(p %*% m ) )
#' zapsmall( m %*% p )  ==  zapsmall( t(m %*% p ) )
#' 
#' # example with an invertable matrix
#' m2 = rbind(
#'   c(1,1),
#'   c(1,0)
#' )
#' zapsmall( solve(m2) ) == zapsmall( expmat(m2,-1) )
#' 
#' @export
#' 
expmat <- function(MAT, EXP, tol=NULL){
	MAT <- as.matrix(MAT)
	matdim <- dim(MAT)
	if(is.null(tol)){
		tol=min(1e-7, .Machine$double.eps*max(matdim)*max(MAT))
	}
	if(matdim[1]>=matdim[2]){ 
		svd1 <- svd(MAT)
		keep <- which(svd1$d > tol)
		res <- t(svd1$u[,keep]%*%diag(svd1$d[keep]^EXP, nrow=length(keep))%*%t(svd1$v[,keep]))
	}
	if(matdim[1]<matdim[2]){ 
		svd1 <- svd(t(MAT))
		keep <- which(svd1$d > tol)
		res <- svd1$u[,keep]%*%diag(svd1$d[keep]^EXP, nrow=length(keep))%*%t(svd1$v[,keep])
	}
	return(res)
}

