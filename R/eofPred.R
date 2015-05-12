#' EOF prediction
#' 
#' The \code{eofPred} function predics the principal component loadings
#' of a new dataset with an \code{eof} object
#' 
#' @param EOF An object resulting from the function \code{eof}
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
#' Efull <- eof(iris[,1:4]) # EOF of full data
#' Etrain <- eof(train) # EOF of training data
#' 
#' # Predict PCs of validation set
#' pred <- eofPred(Etrain, newdata=valid)
#' # plot against full data-derived PCs 
#' plot(c(Efull$A[-tmp,]), c(pred))
#' abline(0,1, col=2)
#' 
#' # Predict PCs of full set
#' pred <- eofPred(Efull, newdata=iris[,1:4])
#' # plot against full data-derived PCs (should be equal)
#' plot(c(Efull$A), c(pred))
#' abline(0,1, col=2)
#' 
#' 
#' ### gappy example
#' # Data
#' set.seed(1)
#' ptrain <- 0.5 # portion of data to use for training set
#' pgap <- 0.2 # portion of data that are gaps (e.g. NaNs)
#' irisg <- as.matrix(iris[,1:4])
#' irisg[sample(length(irisg), length(irisg)*pgap)] <- NaN
#' tmp <- sample(nrow(irisg), nrow(irisg)*ptrain)
#' traing <- irisg[tmp,] # training set
#' validg <- irisg[-tmp,] # validation set
#' 
#' # EOF analysis
#' Efullg <- eof(irisg, recursive=TRUE) # EOF of full data
#' Etraing <- eof(traing, recursive=TRUE) # EOF of training data
#' 
#' # Predict PCs of validation set
#' pred <- eofPred(Etrain, newdata=validg)
#' # plot against full data-derived PCs 
#' plot(c(Efullg$A[-tmp,]), c(pred))
#' abline(0,1, col=3)
#' # Root mean squared error
#' rmse <- sqrt(mean((c(Efullg$A[-tmp,])-c(pred))^2, na.rm=TRUE))
#' rmse
#' 
#' # Predict PCs of full set
#' pred <- eofPred(Etrain, newdata=irisg)
#' plot(c(Efullg$A), c(pred))
#' rmse <- sqrt(mean((c(Efullg$A)-c(pred))^2, na.rm=TRUE))
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


