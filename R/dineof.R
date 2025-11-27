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
#' @param delta.rms The threshold for RMS convergence. Technically, the 
#'   improvement in the ratio of root-mean-squared error to standard deviation
#'   of the known values in Xo.
#' @param method Method to use for matrix decomposition (\code{\link[base]{svd}}, 
#' \code{\link[irlba]{irlba}}, \code{\link[RSpectra]{svds}}).
#' Default is \code{method="svds"}, which is more computationally efficient
#' for large matrices. \code{method="irlba"} can also be used for partial 
#' decomposition, and is included for consistency with 
#' previous versions of the sinkr package.
#' @param verbose logical. Print progress (Default: verbose = TRUE).
#' 
#' @importFrom stats sd
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
#' data(Xt)
#' 
#' # The "noisy" field
#' # noise standard deviation at 10% noise-to-signal ratio
#' noise_sd <- 0.1 * sd(as.vector(Xt))
#' 
#' # Add Gaussian noise
#' set.seed(123)  # For reproducibility
#' Xn <- Xt + rnorm(length(Xt), mean = 0, sd = noise_sd)
#' Xn <- array(Xn, dim = dim(Xt))
#' 
#' # The "observed" gappy field field
#' set.seed(1)
#' frac.gaps <- 0.5
#' gaps <- sample(seq(length(Xn)), frac.gaps*length(Xn))
#' Xo <- replace(Xn, gaps, NaN)
#' 
#' # The dineof "interpolated" field
#' set.seed(1)
#' RES <- dineof(Xo, delta.rms = 1e-05) # lower 'delta.rms' for higher resolved interpolation
#' Xa <- RES$Xa
#' 
#' # Visualization all fields
#' pal <- colorRampPalette(c("blue", "cyan", "yellow", "red"))
#' ZLIM <- range(Xt, Xn, Xo, Xa, na.rm=TRUE)
#' 
#' op <- par(mfrow=c(2,2), mar=c(1,1,3,1))
#' image(z=Xt, zlim=ZLIM, main="A) True", 
#'   col=pal(100), xaxt="n", yaxt="n", xlab="", ylab="")
#' box()
#' image(z=Xn, zlim=ZLIM, main= "B) True + Noise", 
#'   col=pal(100), xaxt="n", yaxt="n", xlab="", ylab="")
#' box()
#' image(z=Xo, zlim=ZLIM, main="C) Observed (gappy)", 
#'   col=pal(100), xaxt="n", yaxt="n", xlab="", ylab="")
#' box()
#' image(z=Xa, zlim=ZLIM, main="D) Reconstruction", 
#'   col=pal(100), xaxt="n", yaxt="n", xlab="", ylab="")
#' box()
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
#' RES <- dineof(iris2g, method="svd")  
#' 
#' # plot results
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
dineof <- function(Xo, n.max = NULL, ref.pos = NULL, delta.rms = 1e-3, method = "svds", verbose = TRUE) {
  
  if (is.null(n.max)) n.max <- ncol(Xo)

  na.true <- which(is.na(Xo))
  na.false <- which(!is.na(Xo))
  if (is.null(ref.pos)) ref.pos <- sample(na.false, max(30, 0.01 * length(na.false)))

  Xa <- Xo
  Xa[c(ref.pos, na.true)] <- 0
  sdXo <- sd(Xo, na.rm = TRUE)
  
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
    sqrt(mean((Xa[ref.pos] - Xo[ref.pos])^2)) / sd(Xo, na.rm = TRUE)
  }

  # Helper function to print RMS status
  print_rms <- function(n.eof, rms, refining = TRUE) {
    if (refining) {
      cat(sprintf("%d EOF (refining)\t |\t RMS ratio = %f\r", n.eof, rms))
    } else {
      cat(sprintf("%d EOF (finalized)\t |\t RMS ratio = %f\n", n.eof, rms))
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

  # Outer loop: increase number of EOFs as long as rms ratio improvement is greater than delta.rms
  while ((rms.prev - rms.now) > delta.rms && n.eof <= n.max) {

    # Inner loop: refine current EOF reconstruction as long as rms ratio improvement is greater than delta.rms
    while ((rms.prev - rms.now) > delta.rms) {
      rms.prev <- rms.now

      SVDi <- compute_svd(Xa, n.eof, method)
      RECi <- reconstruct_eof(SVDi, n.eof)
      Xa[c(ref.pos, na.true)] <- RECi[c(ref.pos, na.true)]

      rms.now <- compute_rms(Xa, Xo, ref.pos)
      if(verbose) print_rms(n.eof, rms.now, refining = TRUE)

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
    if(verbose) print_rms(n.eof, rms.now, refining = FALSE)

    # Prepare for next EOF
    n.eof <- n.eof + 1
    if (n.eof > n.max) break

    rms.prev <- rms.now
    SVDi <- compute_svd(Xa, n.eof, method)
    RECi <- reconstruct_eof(SVDi, n.eof)
    Xa[c(ref.pos, na.true)] <- RECi[c(ref.pos, na.true)]
    rms.now <- compute_rms(Xa, Xo, ref.pos)
    if(verbose) print_rms(n.eof, rms.now, refining = TRUE)

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
