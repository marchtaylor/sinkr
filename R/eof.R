#' EOF (Empirical Orthogonal Functions analysis)
#' 
#' This function conducts an Empirical Orthogonal Function analysis (EOF) 
#' via a covariance matrix (\code{cov4gappy} function) and is especially designed to handle 
#' gappy data (i.e. containing missing values - NaN)
#' 
#' @param F1 A data field. The data should be arraunged as samples in the column 
#' dimension (typically each column is a time series for a spatial location).
#' @param centered Logical (\code{TRUE/FALSE}) to define if \code{F1} should be 
#' centered prior to the analysis. Defaults to 'TRUE'
#' @param scaled Logical (\code{TRUE/FALSE}) to define if \code{F1} should be 
#' scaled prior to the analysis. Defaults to 'TRUE'
#' @param nu Numeric value. Defines the number of EOFs to return. Defaults to 
#' return the full set of EOFs.
#' @param method Method for matrix decomposition ('\code{svd}', '\code{eigen}', 
#' '\code{irlba}'). Defaults to 'svd' when \code{method = NULL}. Use of 'irlba' can 
#' dramatically speed up computation time when \code{recursive = TRUE} but may
#' produce errors in computing trailing EOFs. Therefore, this option is only advisable when
#' the field \code{F1} is large and when only a partial decomposition is desired
#' (i.e. \code{nu << dim(F1)[2]}). All methods should give identical 
#' results when \code{recursive=TRUE}.
#' \code{svd} and \code{eigen} give similar results for non-gappy fields, 
#' but will differ slightly with gappy fields due to decomposition of a 
#' nonpositive definite covariance matrix. Specifically, \code{eigen}  will produce 
#' negative eigenvalues for trailing EOFs, while singular values derived from \code{svd} 
#' will be strictly positive.
#' @param recursive Logical. When \code{TRUE}, the function follows the method of
#' "Recursively Subtracted Empirical Orthogonal Functions" (RSEOF) (Taylor et al. 2013). 
#' RSEOF is a modification of a least squares EOF approach for gappy data (LSEOF)
#' (see von Storch and Zwiers 1999)
#' 
#' @details Taylor et al. (2013) demonstrated that the RSEOF approach more accurately 
#' estimates EOFs from a gappy field than the traditional LSEOF method. 
#' Pre-treatment of gappy fields through in EOF interpolation (\code{\link[sinkr]{dineof}}) 
#' may provide the most accurate EOFs, although computation time is substantially longer 
#' than RSEOF. 
#' 
#' 
#' @return Results of \code{eof} are returned as a list containing the following components:
#' \tabular{rll}{
#' \tab \code{u} \tab EOFs.\cr
#' \tab \code{Lambda} \tab Singular values.\cr
#' \tab \code{A} \tab EOF coefficients (i.e. 'Principal Components').\cr
#' \tab \code{F1_dim} \tab Dimensions of field \code{F1}.\cr
#' \tab \code{F1_center} \tab Vector of center values from each column in field \code{F1}.\cr
#' \tab \code{F1_scale} \tab Vector of scale values from each column in field \code{F1}.\cr
#' }
#' 
#' @keywords EOF PCA gappy
#' @examples
#' 
#' # EOF of 'iris' dataset
#' Et <- eof(iris[,1:4])
#' plot(Et$A, col=iris$Species)
#' 
#' # Compare to results of 'prcomp'
#' Pt <- prcomp(iris[,1:4])
#' plot(Et$A, Pt$x) # Sign may be different
#' 
#' # Compare to a gappy dataset (sign of loadings may differ between methods)
#' iris.gappy <- as.matrix(iris[,1:4])
#' set.seed(1)
#' iris.gappy[sample(length(iris.gappy), 0.25*length(iris.gappy))] <- NaN
#' Eg <- eof(iris.gappy, method="svd", recursive=TRUE) # recursive ("RSEOF")
#' op <- par(mfrow=c(1,2))
#' plot(Et$A, col=iris$Species)
#' plot(Eg$A, col=iris$Species)
#' par(op)
#' 
#' # Compare Non-gappy vs. Gappy EOF loadings
#' op <- par(no.readonly = TRUE)
#' layout(matrix(c(1,2,1,3),2,2), widths=c(3,3), heights=c(1,4))
#' par(mar=c(0,0,0,0))
#' plot(1, t="n", axes=FALSE, ann=FALSE)
#' legend("center", ncol=4, legend=colnames(iris.gappy), border=1, bty="n", 
#' fill=rainbow(4))
#' par(mar=c(6,3,2,1))
#' barplot(Et$u, beside=TRUE, col=rainbow(4), ylim=range(Et$u)*c(1.15,1.15))
#' mtext("Non-gappy", side=3, line=0)
#' axis(1, labels=paste("EOF", 1:4), at=c(3, 8, 13, 18), las=2, tick=FALSE)
#' barplot(Eg$u, beside=TRUE, col=rainbow(4), ylim=range(Et$u)*c(1.15,1.15))
#' mtext("Gappy", side=3, line=0)
#' axis(1, labels=paste("EOF", 1:4), at=c(3, 8, 13, 18), las=2, tick=FALSE)
#' par(op)
#'
#' 
#' @references
#'Bjoernsson, H. and Venegas, S.A. (1997). "A manual for EOF and SVD 
#'analyses of climate data", McGill University, CCGCR Report No. 97-1, 
#'Montreal, Quebec, 52pp.
#'
#'von Storch, H, Zwiers, F.W. (1999). Statistical analysis in climate 
#'research. Cambridge University Press.
#'
#'Taylor, Marc H., Martin Losch, Manfred Wenzel, Jens Schroeter (2013). 
#'On the Sensitivity of Field Reconstruction and Prediction Using 
#'Empirical Orthogonal Functions Derived from Gappy Data. J. Climate, 
#'26, 9194-9205. \href{http://dx.doi.org/10.6084/m9.figshare.732650}{pdf}
#'
#' @export
#' 
eof <- function(F1,
centered=TRUE, scaled=FALSE,
nu=NULL, method=NULL, recursive=FALSE
){

  if(is.null(method)){
    method <- "svd"
  }
  
	#if(method == "irlba"){
		#require(irlba)
	#}

	F1 <- as.matrix(F1)
	F1 <- scale(F1, center=centered, scale=scaled)
	
	F1_center <- attr(F1,"scaled:center")
	F1_scale <- attr(F1,"scaled:scale")
	F1_dim <- dim(F1)
	
	if(is.null(nu)){
		nu <- F1_dim[2]
	}	

	if(recursive){
		u <- matrix(0, dim(F1)[2], nu)
		Lambda <- rep(0, nu)
		A <- matrix(0, dim(F1)[1], nu)
		F1.i <- F1
		C.i <- cov4gappy(F1.i)
    tot.var <- sum(diag(C.i))
		for(i in seq(nu)){
			if(method == "eigen"){
				tmp.decomp <- eigen(C.i)
				L <- list()
				L$u <- tmp.decomp$vectors
				L$d <- tmp.decomp$values
				rm("tmp.decomp")
			}
			if(method == "svd"){
				L <- svd(C.i)
			}
			if(method == "irlba"){
				L <- irlba::irlba(C.i, nu=1, nv=1) 
			}
						
			u[,i] <- L$u[,1]
			Lambda[i] <- L$d[1]
	
			#setup for norm
			F1.i_val <- replace(F1.i, which(!is.na(F1.i)), 1)
			F1.i_val <- replace(F1.i_val, which(is.na(F1.i_val)), 0)
	
			#calc of expansion coefficient and scaling norm
			A_coeff <- replace(F1.i, which(is.na(F1.i)), 0) %*% as.matrix(L$u[,1])
			A_norm <- F1.i_val %*% as.matrix(L$u[,1])^2
			A_tmp <- A_coeff / A_norm
			A[,i] <- A_tmp[,1]
	
			#truncated reconstruction
			F1.trunc <- A_tmp[,1] %*% t(as.matrix(L$u[,1]))
			F1.i <- F1.i - F1.trunc
	
			#calc new covariance matrix
			C.i <- cov4gappy(F1.i)

			rm(L)

			print(paste(i, "(", round(i/nu*100), "% )", "of", nu, "iterations completed"))
		}
	}

	if(!recursive){
		C <- cov4gappy(F1)
		tot.var <- sum(diag(C))
		if(method == "eigen"){
			tmp.decomp <- eigen(C)
			L <- list()
			L$u <- tmp.decomp$vectors
			L$d <- tmp.decomp$values
			rm("tmp.decomp")
		}
		if(method == "svd"){
			L <- svd(C)
		}
		if(method == "irlba"){
			L <- irlba::irlba(C, nu=nu, nv=nu)
		}

		#setup for norm
		F1_val <- replace(F1, which(!is.na(F1)), 1)
		F1_val <- replace(F1_val, which(is.na(F1_val)), 0)

		#calc of expansion coefficient and scaling norm
		A_coeff <- replace(F1, which(is.na(F1)), 0) %*% as.matrix(L$u[,1:nu])
		A_norm <- F1_val %*% as.matrix(L$u[,1:nu])^2
		A <- A_coeff / A_norm
		
		u <- L$u[,1:nu]
		Lambda <- L$d

		rm(L)
	}

	RESULT <- list(
		u=u, Lambda=Lambda, A=A,
		nu=nu, tot.var=tot.var,
		F1_dim=F1_dim,
		F1_center=F1_center, F1_scale=F1_scale
	)
	
	RESULT
}
