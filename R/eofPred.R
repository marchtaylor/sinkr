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
#' # Null model
#' Efull.n.sig <- eofNull(iris[,1:4], centered=TRUE, scaled=TRUE, nperm = 99)$n.sig
#' Etrain.n.sig <- eofNull(train, centered=TRUE, scaled=TRUE, nperm = 99)$n.sig
#' 
#' # Predict PCs of validation set
#' pred <- eofPred(Etrain, newdata=valid)
#' # plot against full data-derived PCs 
#' SIGN <- sign(diag(cor(Efull$A[-tmp,], pred)))
#' plot(c(Efull$A[-tmp,]), c( t(t(pred)*SIGN) ), col=rep(seq(ncol(Efull$A)), 
#'  each=nrow(Efull$A[-tmp,])) )
#' abline(0,1, col=8)
#' legend("topleft", legend=paste("PC", seq(ncol(Efull$A[-tmp,]))), 
#'  col=seq(ncol(Efull$A[-tmp,])), lty=0, pch=1)
#' 
#' 
#' # Predict PCs of full set
#' pred <- eofPred(Efull, newdata=iris[,1:4])
#' # plot against full data-derived PCs (should be equal)
#' SIGN <- sign(diag(cor(Efull$A, pred)))
#' plot(c(Efull$A), c( t(t(pred)*SIGN) ), col=rep(seq(ncol(Efull$A)), 
#'  each=nrow(Efull$A)) )
#' abline(0,1, col=8)
#' legend("topleft", legend=paste("PC", seq(ncol(Efull$A))), 
#'  col=seq(ncol(Efull$A)), lty=0, pch=1)
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
#' # EOF Null model
#' EfullgNull <- eofNull(irisg, centered=TRUE, scaled=TRUE, recursive=TRUE, nperm=99)
#' EtraingNull <- eofNull(traing, centered=TRUE, scaled=TRUE, recursive=TRUE, nperm=99)
#' 
#' # Predict PCs of validation set
#' pred <- eofPred(Etrain, newdata=validg)
#' # plot against full data-derived PCs 
#' SIGN <- sign(diag(cor(Efullg$A[-tmp,], pred)))
#' plot(c(Efullg$A[-tmp,]), c( t(t(pred)*SIGN) ), col=rep(seq(ncol(Efullg$A[-tmp,])), 
#'  each=nrow(Efullg$A[-tmp,])))
#' legend("topleft", legend=paste("PC", seq(ncol(Efullg$A[-tmp,]))), 
#'  col=seq(ncol(Efullg$A[-tmp,])), lty=0, pch=1)
#' abline(0,1, col=8)
#' 
#' # Reconstruction and measurement of error against non-gappy data
#' usePCs <- seq(1) # Efull.n.sig = 1
#' Rg <- eofRecon(Etrain, pcs=seq(usePCs), newpcs=pred)
#' plot( c(as.matrix(iris[-tmp,1:4])), c(Rg) )
#' abline(0,1, col=8)
#' rmse <- sqrt( mean( ( c(as.matrix(iris[-tmp,1:4])) - c(Rg)   )^2, na.rm=TRUE) )
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
  
  # Predict principal components 
  A_coeff <- replace(newdata, which(is.na(newdata)), 0) %*% as.matrix(EOF$u[,pcs])  
  A_coeff
}


