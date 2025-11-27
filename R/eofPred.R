#' EOF prediction
#' 
#' The \code{eofPred} function predics the principal component loadings
#' of a new dataset with an \code{\link[sinkr]{eof}} object
#' 
#' @param EOF An object resulting from the function \code{\link[sinkr]{eof}}
#' @param pcs The principal components (PCs) to use in the reconstruction
#'   (defaults to the full set of PCs: \code{pcs=seq(ncol(EOF$u))})
#' @param newdata new data to project onto eofs
#' 
#' @return A matrix of principal component loadings 
#' (as in \code{EOF$A})
#' 
#' @examples
#' ### Non-gappy example
#' # Data
#' set.seed(1)
#' ptrain <- 0.5 # portion of data to use for training set
#' tmp <- sample(nrow(iris), nrow(iris)*ptrain)
#' train <- iris[tmp,1:4] # training set
#' valid <- iris[-tmp,1:4] # validation set
#' 
#' # EOF analysis
#' Efull <- eof(iris[,1:4], centered=TRUE, scaled=TRUE) # EOF of full data
#' Etrain <- eof(train, centered=TRUE, scaled=TRUE) # EOF of training data
#' 
#' 
#' # Predict PCs of validation set
#' pred <- eofPred(Etrain, newdata=valid)
#' # plot against full data-derived PCs
#' SIGN <- diag(sign(diag(cor(Efull$A[-tmp,], pred)))) # correction for differing sign
#' matplot(Efull$A[-tmp,] %*% SIGN, pred)
#' abline(0,1, col=8)
#' legend("topleft", legend=paste("PC", seq(ncol(pred))), 
#'        col=seq(ncol(pred)), lty=0, pch=1)
#' 
#' # Predict PCs of full set
#' pred <- eofPred(Efull, newdata=iris[,1:4])
#' # plot against full data-derived PCs (should be equal)
#' SIGN <- diag(sign(diag(cor(Efull$A, pred)))) # correction for differing sign
#' matplot(Efull$A %*% SIGN, pred)
#' abline(0,1, col=8)
#' legend("topleft", legend=paste("PC", seq(ncol(pred))), 
#'   col=seq(ncol(pred)), lty=0, pch=1)
#' 
#' 
#' ### gappy example
#' # Data
#' set.seed(1) # 
#' ptrain <- 0.5 # portion of data to use for training set
#' pgap <- 0.2 # portion of data that are gaps (e.g. NaNs)
#' irisg <- as.matrix(iris[,1:4])
#' irisg[sample(length(irisg), length(irisg)*pgap)] <- NaN
#' tmp <- sample(nrow(irisg), nrow(irisg)*ptrain)
#' traing <- irisg[tmp,] # training set
#' validg <- irisg[-tmp,] # validation set
#' 
#' # EOF analysis
#' Efullg <- eof(irisg, centered=TRUE, scaled=TRUE, recursive=TRUE) # EOF of full data
#' Etraing <- eof(traing, centered=TRUE, scaled=TRUE, recursive=TRUE) # EOF of training data
#' 
#' 
#' # Predict PCs of validation set
#' pred <- eofPred(Etrain, newdata=validg)
#' # plot against full data-derived PCs
#' SIGN <- diag(sign(diag(cor(Efullg$A[-tmp,], pred)))) # correction for differing sign
#' matplot(Efullg$A[-tmp,] %*% SIGN, pred)
#' abline(0,1, col=8)
#' legend("topleft", legend=paste("PC", seq(ncol(pred))), 
#'   col=seq(ncol(pred)), lty=0, pch=1)
#' 
#' # Reconstruction and measurement of error against non-gappy data
#' usePCs <- seq(1) # Efull.n.sig = 1
#' Rg <- eofRecon(Etrain, pcs=seq(usePCs), newpcs=pred)
#' plot( c(as.matrix(iris[-tmp,1:4])), c(Rg) )
#' abline(0,1, col=8)
#' rmse <- sqrt( mean( ( c(as.matrix(iris[-tmp,1:4])) - c(Rg) )^2, na.rm=TRUE) )
#' rmse
#' 
#' 
#' @export
#'
eofPred <- function(EOF, newdata=NULL, pcs=NULL){
  if(is.null(newdata)){ stop("Must provide 'newdata'") }
  if(is.null(pcs)){ pcs=seq(ncol(EOF$u)) }
  
  # center and scale newdata
  F1_center=EOF$F1_center
  F1_scale=EOF$F1_scale
  if(!is.null(F1_center)){
    newdata <- scale(newdata, center=F1_center, scale=FALSE)
  }
  if(!is.null(F1_scale)){
    newdata <- scale(newdata, center=FALSE, scale=F1_scale)
  }
  
#   # Predict principal components 
#   A_coeff <- replace(newdata, which(is.na(newdata)), 0) %*% as.matrix(EOF$u[,pcs])  
#   #A_coeff
  
  #setup for norm
  newdata_val <- replace(newdata, which(!is.na(newdata)), 1)
  newdata_val <- replace(newdata_val, which(is.na(newdata_val)), 0)
  
  #calc of expansion coefficient and scaling norm
  A_coeff <- replace(newdata, which(is.na(newdata)), 0) %*% as.matrix(EOF$u[,pcs])  
  A_norm <- newdata_val %*% as.matrix(EOF$u[,pcs])^2
  A <- A_coeff / A_norm
  
  A
}


