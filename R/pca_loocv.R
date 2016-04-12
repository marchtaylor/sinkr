#' Principal component analysis "leave-one-out" cross-validation
#'
#' @param X Matrix to be subjected to svd
#' @param npc.max The maximum number of principal components to test.
#'
#' @return Matrix of square error values for each element in X  
#' 
#' @references \url{http://stats.stackexchange.com/a/115477/10675}
#' @export
#'
#' @examples
#' 
#' library(MASS)
#' X <- as.matrix(iris[,1:4])
#' res <- pca_loocv(X)
#' res <- lapply(res, colSums)
#' COL <- 2:4
#' 
#' op <- par(mar=c(4,4,2,1), tcl=-0.25, mgp=c(2.5,0.5,0))
#' for(i in seq(res)){
#'   if(i==1) {
#'     plot(res[[i]], t="n", ylim=range(unlist(res)), main="iris", xlab="n PCs", ylab="PRESS")
#'     grid()
#'   } 
#'   lines(res[[i]], t="b", bg=c(NaN,COL[i])[(res[[i]]==min(res[[i]])) + 1], col=COL[i], pch=21)
#' }
#' legend("topleft", legend=c("naive", "approximate", "pseudoinverse"), col=COL, lty=1, pch=21, bty="n")
#' par(op)
#' 
#' 
pca_loocv <- function(X, npc.max=25){
  error1 <- matrix(NaN, nrow=dim(X)[1], ncol=min(dim(X)[2],npc.max))
  error2 <- matrix(NaN, nrow=dim(X)[1], ncol=min(dim(X)[2],npc.max))
  error3 <- matrix(NaN, nrow=dim(X)[1], ncol=min(dim(X)[2],npc.max))
  for(n in 1:dim(X)[1]){
    Xtrain = X[-n,]
    Xtrain = scale(Xtrain, center=TRUE, scale=FALSE)
    V = svd(Xtrain)$v
    Xtest = X[n,,drop = FALSE]
    Xtest = scale(Xtest, center=attr(Xtrain, "scaled:center"), scale=FALSE)
    for(j in 1:min(dim(V)[2],npc.max)){
        P = V[,1:j] %*% t(V[,1:j])
        err1 <- Xtest %*% (diag(length(diag(P))) - P)
        err2 <- Xtest %*% (diag(length(diag(P))) - P + diag(diag(P)))
        err3 <- array(NaN, dim=dim(Xtest))
        for(k in 1:dim(Xtest)[2]){
          proj = Xtest[,-k] %*% t(ginv(V[-k,1:j])) %*% t(V[,1:j])
          err3[k] = Xtest[k] - proj[k]
        }
        error1[n,j] <- sum(err1^2)
        error2[n,j] <- sum(err2^2)
        error3[n,j] <- sum(err3^2)
    }
  }
  res <- list(
    error1=error1,
    error2=error2,
    error3=error3
  )
  return(res)
}
