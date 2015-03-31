#' DINEOF (Data Interpolating Empirical Orthogonal Functions)
#' 
#' This function is based on the 
#' DINEOF (Data Interpolating Empirical Orthogonal Functions) 
#' procedure described by Beckers and Rixon (2003). The procedure has been
#' shown to accurately determine Emprirical Orthogonal Functions (EOFs) from 
#' gappy data sets (Taylor et al. 2013). Rather than directly return the EOFs, 
#' the results of the \code{dineof} function is a fully interpolated 
#' matrix which can then be subjected to a final EOF decomposition with 
#' \code{eof}, \code{prcomp}, or other EOF/PCA function of preference.
#' 
#' @param Xo A gappy data field.
#' @param n.max A maximum number of EOFs to iterate 
#' (leave equalling "NULL" if algorithm shold proceed until convergence)
#' @param ref.pos A vector of non-gap reference positions by which 
#' errors will be assessed via root mean squared error ("RMS"). 
#' If ref.pos = NULL, then either 30 or 1\% of the non-gap values 
#' (which ever is larger) will be sampled at random.
#' @param delta.rms The threshold for RMS convergence.
#' 
#' @return Results of \code{dineof} are returned as a list 
#' containing the following components:
#' \tabular{rll}{
#' \tab \code{Xa} \tab The data field with interpolated values (via EOF reconstruction) included.\cr
#' \tab \code{n.eof} \tab The number of EOFs used in the final solution.\cr
#' \tab \code{RMS} \tab A vector of the RMS values from the iteration.\cr
#' \tab \code{NEOF} \tab A vector of the number of EOFs used at each iteration.\cr
#' }
#' 
#' @keywords EOF PCA gappy algorithm
#' @examples
#'# Make synthetic data field
#'m=50
#'n=100
#'frac.gaps <- 0.5 # the fraction of data with NaNs
#'N.S.ratio <- 0.1 # the Noise to Signal ratio for adding noise to data
#'x <- (seq(m)*2*pi)/m
#'t <- (seq(n)*2*pi)/n
#'Xt <- 
#'  outer(sin(x), sin(t)) + 
#'  outer(sin(2.1*x), sin(2.1*t)) + 
#'  outer(sin(3.1*x), sin(3.1*t)) +
#'  outer(tanh(x), cos(t)) + 
#'  outer(tanh(2*x), cos(2.1*t)) + 
#'  outer(tanh(4*x), cos(0.1*t)) + 
#'  outer(tanh(2.4*x), cos(1.1*t)) + 
#'  tanh(outer(x, t, FUN="+")) + 
#'  tanh(outer(x, 2*t, FUN="+")
#')
#'
#'# Color palette
#'pal <- colorRampPalette(c("blue", "cyan", "yellow", "red"))
#'
#'#  The "true" fieldd
#'Xt <- t(Xt)
#'
#'# The "noisy" field
#'set.seed(1)
#'RAND <- matrix(runif(length(Xt), min=-1, max=1), nrow=nrow(Xt), ncol=ncol(Xt))
#'R <- RAND * N.S.ratio * Xt
#'Xp <- Xt + R
#'
#'# The "observed" gappy field field
#'set.seed(1)
#'gaps <- sample(seq(length(Xp)), frac.gaps*length(Xp))
#'Xo <- replace(Xp, gaps, NaN)
#'
#'# The dineof "interpolated" field
#'set.seed(1)
#'RES <- dineof(Xo)
#'Xa <- RES$Xa
#'
#'# Visualization all fields
#'ZLIM <- range(Xt, Xp, Xo, Xa, na.rm=TRUE)
#'op <- par(mfrow=c(2,2), mar=c(3,3,3,1))
#'image(z=Xt, zlim=ZLIM, main="A) True", col=pal(100), xaxt="n", yaxt="n", xlab="", ylab="")
#'box()
#'mtext("t", side=1, line=0.5)
#'mtext("x", side=2, line=0.5)
#'image(z=Xp, zlim=ZLIM, main=paste("B) True + Noise (N/S = ", N.S.ratio, ")", sep=""), 
#'col=pal(100), xaxt="n", yaxt="n", xlab="", ylab="")
#'box()
#'mtext("t", side=1, line=0.5)
#'mtext("x", side=2, line=0.5)
#'box()
#'image(z=Xo, zlim=ZLIM, main=paste("C) Observed (", frac.gaps*100, " % gaps)", sep=""), 
#'col=pal(100), xaxt="n", yaxt="n", xlab="", ylab="")
#'mtext("t", side=1, line=0.5)
#'mtext("x", side=2, line=0.5)
#'image(z=Xa, zlim=ZLIM, main="D) Reconstruction", col=pal(100), xaxt="n", yaxt="n", 
#'xlab="", ylab="")
#'box()
#'mtext("t", side=1, line=0.5)
#'mtext("x", side=2, line=0.5)
#'par(op)
#'
#'
#' @references
#' Beckers, J-M, and M. Rixen. "EOF Calculations and Data Filling from 
#' Incomplete Oceanographic Datasets." Journal of Atmospheric and Oceanic 
#' Technology 20.12 (2003): 1839-1856.
#' 
#' Taylor, Marc H., Martin Losch, Manfred Wenzel, Jens Schroeter (2013). 
#' On the Sensitivity of Field Reconstruction and Prediction Using 
#' Empirical Orthogonal Functions Derived from Gappy Data. J. Climate, 
#' 26, 9194-9205.
#'
#' @export
#' 
#'
dineof <- function(Xo, n.max=NULL, ref.pos=NULL, delta.rms=1e-5){

	#require(irlba)

	if(is.null(n.max)){
		n.max=dim(Xo)[2]
	}	

	na.true <- which(is.na(Xo))
	na.false <- which(!is.na(Xo))
	if(is.null(ref.pos)) ref.pos <- sample(na.false, max(30, 0.01*length(na.false)))

	Xa <- replace(Xo, c(ref.pos, na.true), 0)
	rms.prev <- Inf
	rms.now <- sqrt(mean((Xa[ref.pos] - Xo[ref.pos])^2))
	n.eof <- 1
	RMS <- rms.now
	NEOF <- n.eof
	Xa.best <- Xa
	n.eof.best <- n.eof	
	while(rms.prev - rms.now > delta.rms & n.max > n.eof){ #loop for increasing number of eofs
		while(rms.prev - rms.now > delta.rms){ #loop for replacement
			rms.prev <- rms.now
			SVDi <- irlba::irlba(Xa, nu=n.eof, nv=n.eof)	
			RECi <- as.matrix(SVDi$u[,seq(n.eof)]) %*% as.matrix(diag(SVDi$d[seq(n.eof)], n.eof, n.eof)) %*% t(as.matrix(SVDi$v[,seq(n.eof)]))
			Xa[c(ref.pos, na.true)] <- RECi[c(ref.pos, na.true)]
			rms.now <- sqrt(mean((Xa[ref.pos] - Xo[ref.pos])^2))
			print(paste(n.eof, "EOF", "; RMS =", round(rms.now, 8)))
			RMS <- c(RMS, rms.now)
			NEOF <- c(NEOF, n.eof)
			gc()
			if(rms.now == min(RMS)) {
				Xa.best <- Xa
				n.eof.best <- n.eof
			}
		}
		n.eof <- n.eof + 1
		rms.prev <- rms.now
		SVDi <- irlba::irlba(Xa, nu=n.eof, nv=n.eof)	
		RECi <- as.matrix(SVDi$u[,seq(n.eof)]) %*% as.matrix(diag(SVDi$d[seq(n.eof)], n.eof, n.eof)) %*% t(as.matrix(SVDi$v[,seq(n.eof)]))
		Xa[c(ref.pos, na.true)] <- RECi[c(ref.pos, na.true)]
		rms.now <- sqrt(mean((Xa[ref.pos] - Xo[ref.pos])^2))
		print(paste(n.eof, "EOF", "; RMS =", round(rms.now, 8)))
		RMS <- c(RMS, rms.now)
		NEOF <- c(NEOF, n.eof)
		gc()
		if(rms.now == min(RMS)) {
			Xa.best <- Xa
			n.eof.best <- n.eof
		}
	}
	
	Xa <- Xa.best
	n.eof <- n.eof.best
	rm(list=c("Xa.best", "n.eof.best", "SVDi", "RECi"))

	Xa[ref.pos] <- Xo[ref.pos]

	RESULT <- list(
		Xa=Xa, n.eof=n.eof, RMS=RMS, NEOF=NEOF
	)
	
	RESULT
}