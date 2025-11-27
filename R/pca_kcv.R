#' Principal component analysis k-fold cross-validation
#'
#' @param X Matrix to be subjected to svd
#' @param ks Number of k-fold groups to use. Default=2. (see \code{\link[sinkr]{kfold}})
#' @param npc.max The maximum number of principal components to test. Default=ncol(X)
#' @param verbose logical. Should progress be printed 
#'
#' @return Matrix of square error values for each element in X  
#' 
#' @references \url{http://stats.stackexchange.com/a/115477/10675}
#' 
#' @export
#'
#' @examples
#' dat <- as.matrix(iris[,1:4])
#' res <- pca_kcv(dat, ks=10)
#' res2 <- lapply(res, colSums)
#' res2
#' 
#' COL <- 2:4
#' LTY <- 1:3
#' op <- par(mar=c(4,4,2,1), tcl=-0.25, mgp=c(2.5,0.5,0))
#' for(i in seq(res)){
#'   if(i==1) {
#'     plot(res2[[i]], t="n", ylim=range(unlist(res2)), 
#'      main="iris", xlab="n PCs", ylab="PRESS")
#'     grid()
#'   } 
#'   lines(res2[[i]], t="b", bg=c(NaN,COL[i])[(res2[[i]]==min(res2[[i]])) + 1],
#'    col=COL[i], lty=LTY[i], pch=21)
#' }
#' legend("topright", legend=c("naive", "approximate", "pseudoinverse"),
#'  col=COL, lty=LTY, pch=21, bty="n")
#' par(op)
#' 
#' 
pca_kcv <- function(X, ks=2, npc.max=ncol(X), verbose = TRUE){
  kgroups <- kfold(n = nrow(X), k = ks)
  error1 <- matrix(0, nrow=dim(X)[1], ncol=min(dim(X)[2],npc.max))
  error2 <- matrix(0, nrow=dim(X)[1], ncol=min(dim(X)[2],npc.max))
  error3 <- matrix(0, nrow=dim(X)[1], ncol=min(dim(X)[2],npc.max))
  for(n in seq(kgroups)){
    Xtrain = X[-kgroups[[n]],]
    Xtrain = scale(Xtrain, center=TRUE, scale=FALSE)
    V = svd(Xtrain)$v
    Xtest = X[kgroups[[n]],,drop = FALSE]
    Xtest = scale(Xtest, center=attr(Xtrain, "scaled:center"), scale=FALSE)
    for(j in 1:min(dim(V)[2],npc.max)){
      P = V[,1:j] %*% t(V[,1:j])
      err1 <- Xtest %*% (diag(length(diag(P))) - P)
      err2 <- Xtest %*% (diag(length(diag(P))) - P + diag(diag(P)))
      err3 <- array(NaN, dim=dim(Xtest))
      for(k in 1:dim(Xtest)[2]){
        proj = Xtest[,-k] %*% t(expmat(V[-k,1:j])) %*% t(V[,1:j])
        err3[,k] = Xtest[,k] - proj[,k]
      }
      error1[kgroups[[n]],j] <- error1[kgroups[[n]],j] + rowSums(sqrt(err1^2))
      error2[kgroups[[n]],j] <- error2[kgroups[[n]],j] + rowSums(sqrt(err2^2))
      error3[kgroups[[n]],j] <- error3[kgroups[[n]],j] + rowSums(sqrt(err3^2))
      
      if(verbose){
  			cat(sprintf("n = %d; j = %d\r", n, j))
  			flush.console()
      }
    }
  }
  res <- list(
    naive=error1,
    approximate=error2,
    pseudoinverse=error3
  )
  return(res)
}
