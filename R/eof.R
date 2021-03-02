#' EOF (Empirical Orthogonal Functions analysis)
#' 
#' This function conducts an Empirical Orthogonal Function analysis (EOF) 
#' via a covariance matrix (\code{cov4gappy} function) and is especially designed 
#' to handle gappy data (i.e. containing missing values - NaN)
#' 
#' @param F1 A data field. The data should be arraunged as samples in the column 
#' dimension (typically each column is a time series for a spatial location).
#' @param centered Logical (\code{TRUE/FALSE}) to define if \code{F1} should be 
#' centered prior to the analysis. Defaults to \code{TRUE}
#' @param scaled Logical (\code{TRUE/FALSE}) to define if \code{F1} should be 
#' scaled prior to the analysis. Defaults to \code{TRUE}
#' @param nu Numeric value. Defines the number of EOFs to return. Defaults to 
#' return the full set of EOFs.
#' @param method Method for matrix decomposition (\code{\link[base]{svd}}, 
#' \code{\link[base]{eigen}}, \code{\link[irlba]{irlba}}, 
#' \code{\link[RSpectra]{svds}}). 
#' Defaults to \code{"svd"} when \code{method = NULL} and \code{"svds"} when 
#' \code{method = NULL} and \code{recursive = TRUE}.
#' Use of \code{"svds"} or \code{"irlba"} calculates a partial SVD, which is 
#' recommended when \code{recursive = TRUE} due to faster computation speed.
#' @param recursive Logical. When \code{TRUE}, the function follows the method of
#' "Recursively Subtracted Empirical Orthogonal Functions" (RSEOF) (Taylor et al. 2013). 
#' RSEOF is a modification of a least squares EOF approach for gappy data (LSEOF)
#' (see von Storch and Zwiers 1999)
#' 
#' @details Taylor et al. (2013) demonstrated that the RSEOF approach 
#' (i.e. \code{recursive = TRUE}) more accurately estimates EOFs from a 
#' gappy field than the traditional LSEOF method. 
#' Pre-treatment of gappy fields through in EOF interpolation 
#' (\code{\link[sinkr]{dineof}}) may provide the most accurate estimate of EOFs; 
#' however, RSEOF can be much faster in cases where the number of columns in \code{F1} 
#' is smaller than the number of rows.
#' 
#' All methods should give identical results when \code{recursive = TRUE}, despite 
#' differences in calculation time. 
#' When \code{recursive = FALSE}, \code{"svd"} and \code{"eigen"} give similar 
#' results for non-gappy fields, but will differ with gappy fields 
#' due to decomposition of a nonpositive definite covariance matrix, as calculated 
#' with \code{\link[sinkr]{cov4gappy}}. 
#' Specifically, \code{"eigen"} will produce negative eigenvalues for trailing 
#' EOFs, while singular values derived from \code{"svd"} will be strictly positive.
#' Faster computation time with \code{"svds"} over 
#' \code{"irlba"} may not result when  
#' \code{recursive = TRUE} due to the iterative computation of leading vectors only. 
#' 
#' @return Results of \code{eof} are returned as a list containing the following components:
#' \tabular{rll}{
#' \tab \code{u} \tab EOFs.\cr
#' \tab \code{Lambda} \tab Singular values.\cr
#' \tab \code{A} \tab EOF coefficients (i.e. 'Principal Components').\cr
#' \tab \code{tot.var} \tab Total variance of field \code{F1} (from \code{cov4gappy}). \cr
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
#' SIGN <- diag(sign(diag(cor(Et$A, Pt$x)))) # correction for differing sign
#' matplot(Et$A %*% SIGN, Pt$x)
#' abline(0,1, col=8)
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
#' ### EOF of climate field example
#' library(maps) # required package to add map
#' 
#' # load data
#' data(sst)
#' names(sst)
#' 
#' # EOF
#' E <- eof(sst$field, nu=10)
#' 
#' # Plot of EOF loading and PC
#' eof.num <- 2 # EOF number to plot
#' op <- par(no.readonly=TRUE)
#' layout(matrix(c(1,3,2,3),nrow=2, ncol=2), widths=c(5,1), heights=c(3,3), respect=TRUE)
#' op <- par(ps=10, cex=1)
#' par(mar=c(4,4,1,1))
#' PAL <- colorPalette(c("blue", "cyan", "grey90", "yellow", "red"), c(10,1,1,10))
#' ZLIM <- c(-1,1)*max(abs(E$u[,eof.num]))
#' COL <- val2col(E$u[,eof.num], col=PAL(100), zlim=ZLIM)
#' plot(lat ~ lon, data=sst$grid, pch=22, bg=COL, col=COL, cex=2)
#' map("world", add=TRUE)
#' par(mar=c(4,0,1,4))
#' imageScale(E$u[,eof.num], col=PAL(100), zlim=ZLIM, axis.pos=4)
#' par(mar=c(4,4,1,4))
#' plot(sst$date, E$A[,eof.num], t="l", xlab="date", ylab="")
#' lines(loess.smooth(sst$date, E$A[,eof.num], span=1/3), col=rgb(0.5,0.5,1), lwd=2) # smoothed signal
#' abline(h=0, v=seq(as.Date("1000-01-01"), as.Date("2100-01-01"), by="10 years"), col=8, lty=3)
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

  if(is.null(method) & recursive){
    method <- "svds"
  } else {
    if(is.null(method)){
          method <- "svd"
    }
  }

	F1 <- as.matrix(F1)
	F1 <- scale(F1, center=centered, scale=scaled)
	
	F1_center <- attr(F1,"scaled:center")
	F1_scale <- attr(F1,"scaled:scale")
	F1_dim <- dim(F1)
	
	if(is.null(nu)){
		nu <- F1_dim[2]
	}	

	if(recursive){
  	u <- matrix(0, F1_dim[2], nu)
		Lambda <- rep(0, nu)
		A <- matrix(0, F1_dim[1], nu)
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
		  if(method == "svds"){
		    L <- RSpectra::svds(C.i, k=1, nu=1, nv=1) 
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
		if(method == "svds"){
		  L <- RSpectra::svds(C, k=nu, nu=nu, nv=nu) 
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
		F1_center=F1_center, F1_scale=F1_scale,
    method=method, recursive=recursive
	)
	
	RESULT
}
