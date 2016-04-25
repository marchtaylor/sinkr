#' Wold's PCA cross-validation procedure
#'
#' @param dat Dataset (matrix)
#' @param fracValid Fraction of data to use for each validation. Will determine the
#' number of cross-validation groups (Default=0.2)
#' @param centered Center data (logical). Passed to \link{eof}
#' @param scaled Scale data (logical). Passed to \link{eof}
#' @param ... Additional parameters to pass to \link{eof}
#'
#' @return List of sum of square error (SS), root mean square error (RMSECV) of 
#' cross-validation, number of values used in validation (npred), number of groups
#' used in cross-validation (ngroups)
#' 
#' @references Wold S (1978) Technometrics 20:397–405
#' 
#' @export
#'
#' @examples
#' 
#' data(wine)
#' dat <- as.matrix(wine[,-1])
#' res <- woldcv(dat, fracValid=0.2, centered=TRUE, scaled=TRUE)
#' plot(res$SS, t="b"); which.min(res$SS)
#' plot(res$RMSECV, t="b"); which.min(res$RMSECV)
#' 
#' data(iris)
#' dat <- as.matrix(iris[,-5])
#' res <- woldcv(dat, fracValid=0.3, centered=TRUE, scaled=FALSE)
#' plot(res$SS, t="b"); which.min(res$SS)
#' plot(res$RMSECV, t="b"); which.min(res$RMSECV)
#' 
woldcv <- function(dat, fracValid=0.2, centered = TRUE, scaled = FALSE, ...){

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
  for(i in seq(ngroups)){
    mult <- array(1,dim=dim(dat))
    mult <- t(replace(t(mult), seq(from = i, to = length(dat), by = ngroups), NaN))
    tmp <- dat * mult
    NAs <- which(is.na(tmp))
    Etmp <- eof(tmp, centered = centered, scaled = scaled, recursive = TRUE, ...)
    for(j in seq(ncol(dat))){
      Rtmp <- eofRecon(Etmp, pcs = seq(j))
      SS[j] <- SS[j] + sum((Rtmp[NAs] - dat[NAs])^2, na.rm=TRUE)
      npred <- npred + length(NAs)
    }
  }
  
  RMSECV <- sqrt(SS/npred)
  return(list(SS=SS, RMSECV=RMSECV, ngroups=ngroups, npred=npred))

}

