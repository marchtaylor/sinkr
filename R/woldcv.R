#' Wold's PCA cross-validation procedure
#' 
#' Performs a cross-validation procedure for EOF/PCA according 
#' to an interpolation routine for substituted missing values
#'
#' @param dat Dataset (matrix)
#' @param fracValid Fraction of data to use for each validation. Will determine the
#' number of cross-validation groups (Default=0.2)
#' @param centered Center data (logical). Passed to \link{eof}. Default = TRUE.
#' @param scaled Scale data (logical). Passed to \link{eof} Default = FALSE.
#' @param uncenter.recon Logical. Compare eof reconstruction to uncentered dat. If 
#' centered = FALSE, then this has no effect. Default = TRUE.
#' @param unscale.recon Logical. Compare eof reconstruction to unscaleded dat. If 
#' scaled = FALSE, then this has no effect. Default = TRUE.
#' @param ... Additional parameters to pass to \link{eof}
#'
#' @return List of sum of square error (SS), root mean square error (RMSECV) of 
#' cross-validation, number of values used in validation (npred), number of groups
#' used in cross-validation (ngroups)
#' 
#' @references Wold S (1978) Technometrics 20:397-405
#' 
#' @export
#'
#' @examples
#' 
#' ### wine dataset example
#' data(wine)
#' dat <- as.matrix(wine[,-1])
#' 
#' # uncenter and unscale reconstruction
#' res <- woldcv(dat, fracValid=0.1, centered=TRUE, scaled=TRUE, 
#'   uncenter.recon = TRUE, unscale.recon = TRUE)
#' op <- par(mfrow=c(1,2))
#' plot(res$SS, t="b"); which.min(res$SS)
#' plot(res$RMSECV, t="b"); which.min(res$RMSECV)
#' par(op)
#' res$ngroups
#' res$npred
#' 
#' # Comparison to centered and scaled data
#' res <- woldcv(dat, fracValid=0.1, centered=TRUE, scaled=TRUE,
#'   uncenter.recon = FALSE, unscale.recon = FALSE)
#' op <- par(mfrow=c(1,2))
#' plot(res$SS, t="b"); which.min(res$SS)
#' plot(res$RMSECV, t="b"); which.min(res$RMSECV)
#' par(op)
#' res$ngroups
#' res$npred
#' 
#' 
#' 
#' ### iris dataset example
#' data(iris)
#' dat <- as.matrix(iris[,-5])
#' 
#' # uncenter and unscale reconstruction
#' res <- woldcv(dat, fracValid=0.1, centered=TRUE, scaled=TRUE, 
#'   uncenter.recon = TRUE, unscale.recon = TRUE)
#' op <- par(mfrow=c(1,2))
#' plot(res$SS, t="b"); which.min(res$SS)
#' plot(res$RMSECV, t="b"); which.min(res$RMSECV)
#' par(op)
#' res$ngroups
#' res$npred
#' 
#' # comparison to centered and scaled data
#' res <- woldcv(dat, fracValid=0.2, centered=TRUE, scaled=TRUE,
#'   uncenter.recon = FALSE, unscale.recon = FALSE)
#' op <- par(mfrow=c(1,2))
#' plot(res$SS, t="b"); which.min(res$SS)
#' plot(res$RMSECV, t="b"); which.min(res$RMSECV)
#' par(op)
#' res$ngroups
#' res$npred
#' 
woldcv <- function(dat, fracValid=0.2, 
  centered = TRUE, scaled = FALSE, 
  uncenter.recon = TRUE, unscale.recon = TRUE,
  ...
){

  nwoldgroups <- function(dat, fracValid=fracValid) {
    xs <- seq(from = ncol(dat), by = ncol(dat), length.out = nrow(dat))
    x <- xs[which.min((1/xs - fracValid)^2)]
    x <- as.integer(x)
    div <- seq_len(abs(x))
    notFactor <- div[x %% div != 0L] # which are not factors
    return(notFactor[which.min(((notFactor / x) - (1-fracValid))^2)])
  }

  ngroups <- nwoldgroups(dat, fracValid=fracValid)
  SS <- 0 * seq(ncol(dat))
  npred <- 0
  dat.sc <- scale(dat, center = centered, scale = scaled)
  dat.compare <- unscale(dat.sc, uncenter = uncenter.recon, unscale = unscale.recon) 
  for(i in seq(ngroups)){
    mult <- array(1,dim=dim(dat))
    mult <- t(replace(t(mult), seq(from = i, to = length(dat), by = ngroups), NaN))
    tmp <- dat * mult
    NAs <- which(is.na(tmp))

    Etmp <- eof(tmp, centered = centered, scaled = scaled, recursive = TRUE, ...)
    for(j in seq(ncol(dat))){
      Rtmp <- eofRecon(Etmp, pcs = seq(j), uncenter = uncenter.recon, unscale = unscale.recon)
      SS[j] <- SS[j] + sum((Rtmp[NAs] - dat.compare[NAs])^2, na.rm=TRUE)
      npred <- npred + length(NAs)
    }
  }
  
  RMSECV <- sqrt(SS/npred)
  return(list(SS=SS, RMSECV=RMSECV, ngroups=ngroups, npred=npred))

}

