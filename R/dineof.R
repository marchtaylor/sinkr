#' DINEOF (Data Interpolating Empirical Orthogonal Functions)
#' 
#' This function is based on the 
#' DINEOF (Data Interpolating Empirical Orthogonal Functions) 
#' procedure described by Beckers and Rixon (2003). The procedure has been
#' shown to accurately determine Emprirical Orthogonal Functions (EOFs) from 
#' gappy data sets (Taylor et al. 2013) that are used for data 
#' reconstruction. Rather than directly return the EOFs, 
#' the results of the \code{dineof} function is a fully interpolated 
#' matrix which can then be subjected to a final EOF decomposition with 
#' \code{eof}, \code{prcomp}, or other EOF/PCA function of preference.
#' 
#' @param Xo A gappy data field.
#' @param n.max A maximum number of EOFs to iterate 
#' (leave equalling "NULL" if algorithm shold proceed until convergence)
#' @param ref.pos A vector of non-gap reference positions by which 
#' errors will be assessed via root mean squared error ("RMS"). 
#' If ref.pos = NULL, then either 30 or 1 \% of the non-gap values 
#' (which ever is larger) will be sampled at random.
#' @param delta.rms The threshold for RMS convergence.
#' @param method Method to use for matrix decomposition (\code{\link[base]{svd}}, 
#' \code{\link[irlba]{irlba}}, \code{\link[RSpectra]{svds}}).
#' Default is \code{method="svds"}, which is more computationally efficient
#' for large matrices. \code{method="irlba"} can also be used for partial 
#' decomposition, and is included for consistency with 
#' previous versions of the sinkr package.
#' 
#' @details Method \code{"svds"} is now the default as it provides 
#' better estimates of trailing EOFs than \code{"irlba"} and can be 
#' computationally faster during later iterations where multiple singular vectors 
#' are calculated. 
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
#' # Make synthetic data field
#' m <- 50
#' n <- 100
#' frac.gaps <- 0.5 # the fraction of data with NaNs
#' N.S.ratio <- 0.1 # the Noise to Signal ratio for adding noise to data
#' x <- (seq(m)*2*pi)/m
#' t <- (seq(n)*2*pi)/n
#' Xt <- 
#'   outer(sin(x), sin(t)) + 
#'   outer(sin(2.1*x), sin(2.1*t)) + 
#'   outer(sin(3.1*x), sin(3.1*t)) +
#'   outer(tanh(x), cos(t)) + 
#'   outer(tanh(2*x), cos(2.1*t)) + 
#'   outer(tanh(4*x), cos(0.1*t)) + 
#'   outer(tanh(2.4*x), cos(1.1*t)) + 
#'   tanh(outer(x, t, FUN="+")) + 
#'   tanh(outer(x, 2*t, FUN="+")
#' )
#' 
#' # Color palette
#' pal <- colorRampPalette(c("blue", "cyan", "yellow", "red"))
#' 
#' #  The "true" fieldd
#' Xt <- t(Xt)
#' 
#' # The "noisy" field
#' set.seed(1)
#' RAND <- matrix(runif(length(Xt), min=-1, max=1), nrow=nrow(Xt), ncol=ncol(Xt))
#' R <- RAND * N.S.ratio * Xt
#' Xp <- Xt + R
#' 
#' # The "observed" gappy field field
#' set.seed(1)
#' gaps <- sample(seq(length(Xp)), frac.gaps*length(Xp))
#' Xo <- replace(Xp, gaps, NaN)
#' 
#' # The dineof "interpolated" field
#' set.seed(1)
#' RES <- dineof(Xo, delta.rms = 1e-05) # lower 'delta.rms' for higher resolved interpolation
#' Xa <- RES$Xa
#' 
#' # Visualization all fields
#' ZLIM <- range(Xt, Xp, Xo, Xa, na.rm=TRUE)
#' op <- par(mfrow=c(2,2), mar=c(3,3,3,1))
#' image(z=Xt, zlim=ZLIM, main="A) True", col=pal(100), xaxt="n", yaxt="n", xlab="", ylab="")
#' box()
#' mtext("t", side=1, line=0.5)
#' mtext("x", side=2, line=0.5)
#' image(z=Xp, zlim=ZLIM, main=paste("B) True + Noise (N/S = ", N.S.ratio, ")", sep=""), 
#' col=pal(100), xaxt="n", yaxt="n", xlab="", ylab="")
#' box()
#' mtext("t", side=1, line=0.5)
#' mtext("x", side=2, line=0.5)
#' box()
#' image(z=Xo, zlim=ZLIM, main=paste("C) Observed (", frac.gaps*100, " % gaps)", sep=""), 
#' col=pal(100), xaxt="n", yaxt="n", xlab="", ylab="")
#' mtext("t", side=1, line=0.5)
#' mtext("x", side=2, line=0.5)
#' image(z=Xa, zlim=ZLIM, main="D) Reconstruction", col=pal(100), xaxt="n", yaxt="n", 
#' xlab="", ylab="")
#' box()
#' mtext("t", side=1, line=0.5)
#' mtext("x", side=2, line=0.5)
#' par(op)
#' 
#' 
#' 
#' 
#' ### Example with iris dataset
#' iris2 <- as.matrix(iris[,1:4]) # only use numeric morphometric data
#' frac.gaps <- 0.3 # fraction NaN values
#' 
#' # make gappy dataset
#' set.seed(1)
#' gaps <- sample(seq(length(iris2)), frac.gaps*length(iris2))
#' iris2g <- replace(iris2, gaps, NaN)
#' 
#' # The dineof "interpolated" field
#' # using method = "svd" is more stable if neof might reach nmax
#' set.seed(1)
#' RES <- dineof(iris2g, delta.rms = 1e-05, method="svd")  
#' 
#' # plot results
#' op <- par(mfrow = c(1,2), mar = c(3,3,3,1))
#' plot(iris2, RES$Xa, 
#'    col = rep(rainbow(ncol(iris2)), each = nrow(iris2)),
#'    pch = as.numeric(iris$Species), main = "Imputation w/ DINEOF"
#' )
#' abline(0,1,col=8, lty=1)
#' legend("topleft", legend=colnames(iris2), col=rainbow(ncol(iris2)), lty=1, bty = "n")
#' legend("bottomright", legend=levels(iris$Species), pch=1:3, bty = "n")
#' sqrt(mean((iris2[gaps] - RES$Xa[gaps])^2, na.rm=TRUE)) # root mean square error
#' 
#' 
#' # Note: The use of dineof on small matrices may result in
#' # an overfitted interpolation if too many reference points are used.
#' # Use of eof(, recursive=TRUE) may provide better estimates - i.e. "RSEOF" method
#' # Example:
#' 
#' # EOF
#' E <- eof(iris2g, recursive = TRUE)
#' 
#' # Determine number of significant EOFs
#' En <- eofNull(iris2g, recursive = TRUE, nperm = 99)
#' En$n.sig
#' 
#' # reconstruction with significant EOFs
#' R <- eofRecon(E, pcs = seq(En$n.sig))
#' R[-gaps] <- iris2g[-gaps] # replace non-gap values
#' 
#' # plot interpolated values
#' plot(iris2, R, 
#'    col = rep(rainbow(ncol(iris2)), each = nrow(iris2)),
#'    pch = as.numeric(iris$Species), main = "Recon. w/ sig. EOFS only"
#' )
#' abline(0,1,col=8, lty=1)
#' legend("topleft", legend=colnames(iris2), col=rainbow(ncol(iris2)), lty=1, bty = "n")
#' legend("bottomright", legend=levels(iris$Species), pch=1:3, bty = "n")
#' sqrt(mean((iris2[gaps] - R[gaps])^2, na.rm=TRUE)) # root mean square error
#' 
#' par(op)
#' 
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
dineof <- function(Xo, n.max = NULL, ref.pos = NULL, delta.rms = 1e-5, method = "svds") {
  
  if (is.null(n.max)) n.max <- ncol(Xo)

  na.true <- which(is.na(Xo))
  na.false <- which(!is.na(Xo))
  if (is.null(ref.pos)) ref.pos <- sample(na.false, max(30, 0.01 * length(na.false)))

  Xa <- Xo
  Xa[c(ref.pos, na.true)] <- 0
  attributes(Xa) <- NULL

  # function to compute SVD
  compute_svd <- function(X, n.eof, method) {
    if (method == "irlba") return(irlba::irlba(X, nu = n.eof, nv = n.eof))
    if (method == "svd") return(svd(X))
    if (method == "svds") return(RSpectra::svds(X, k = n.eof))
    stop("Unknown method")
  }

  # function to reconstruct using EOFs
  reconstruct_eof <- function(SVDi, n.eof) {
    SVDi$u[, seq_len(n.eof), drop = FALSE] %*%
      diag(SVDi$d[seq_len(n.eof)], n.eof, n.eof) %*%
      t(SVDi$v[, seq_len(n.eof), drop = FALSE])
  }

  # function to compute rms over reference positions
  compute_rms <- function(Xa, Xo, ref.pos) {
    sqrt(mean((Xa[ref.pos] - Xo[ref.pos])^2))
  }

  # Helper function to print RMS status
  print_rms <- function(n.eof, rms, refining = TRUE) {
    if (refining) {
      cat(sprintf("%d EOF (refining); RMS = %f\r", n.eof, rms))
    } else {
      cat(sprintf("%d EOF (finalized); RMS = %f\n", n.eof, rms))
    }
    flush.console()
  }

  rms.prev <- Inf
  rms.now <- compute_rms(Xa, Xo, ref.pos)
  n.eof <- 1
  RMS <- rms.now
  NEOF <- n.eof
  Xa.best <- Xa
  n.eof.best <- n.eof
  best_rms <- rms.now

  # Outer loop: increase number of EOFs
  while ((rms.prev - rms.now) > delta.rms && n.eof <= n.max) {

    # Inner loop: refine current EOF reconstruction
    while ((rms.prev - rms.now) > delta.rms) {
      rms.prev <- rms.now

      SVDi <- compute_svd(Xa, n.eof, method)
      RECi <- reconstruct_eof(SVDi, n.eof)
      Xa[c(ref.pos, na.true)] <- RECi[c(ref.pos, na.true)]

      rms.now <- compute_rms(Xa, Xo, ref.pos)
      print_rms(n.eof, rms.now, refining = TRUE)

      RMS <- c(RMS, rms.now)
      NEOF <- c(NEOF, n.eof)

      # Update best reconstruction if improved
      if (rms.now < best_rms) {
        best_rms <- rms.now
        Xa.best <- Xa
        n.eof.best <- n.eof
      }
    }

    # Print finalized RMS for current EOF
    print_rms(n.eof, rms.now, refining = FALSE)

    # Prepare for next EOF
    n.eof <- n.eof + 1
    if (n.eof > n.max) break

    rms.prev <- rms.now
    SVDi <- compute_svd(Xa, n.eof, method)
    RECi <- reconstruct_eof(SVDi, n.eof)
    Xa[c(ref.pos, na.true)] <- RECi[c(ref.pos, na.true)]
    rms.now <- compute_rms(Xa, Xo, ref.pos)
    print_rms(n.eof, rms.now, refining = TRUE)

    RMS <- c(RMS, rms.now)
    NEOF <- c(NEOF, n.eof)

    if (rms.now < best_rms) {
      best_rms <- rms.now
      Xa.best <- Xa
      n.eof.best <- n.eof
    }
  }

  # Finalize results
  Xa <- Xa.best
  n.eof <- n.eof.best
  rm(list = c("Xa.best", "n.eof.best", "SVDi", "RECi"))

  Xa[ref.pos] <- Xo[ref.pos]

  list(
    Xa = Xa,
    n.eof = n.eof,
    RMS = RMS,
    NEOF = NEOF,
    ref.pos = ref.pos
  )
}
