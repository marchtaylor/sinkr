#' Determine k-fold partitions for a given number of samples
#'
#' @param n number of samples
#' @param k number of partitions (Defaults to n; i.e. "leave-one-out")
#'
#' @return a list containing sample numbers in each partition
#' @details For specific use in cross validation (see \code{\link{cv.nperm}})
#' @export
#'
#' @examples
#' res <- kfold(100,6)
#' res
#' length(res) # partition indices
#' unlist(lapply(res, length)) # number of samples in each partition
#' 
kfold <- function(n, k=NULL){
  if(is.null(k)){ k <- n} # if undefined, assume leave-one-out
  res <- vector(mode="list", k)
  n.remain <- seq(n)
  for(i in seq(k)){
    samp <- sample(seq(length(n.remain)), ceiling(length(n.remain)/(k-i+1)))
    res[[i]] <- n.remain[samp]
    n.remain <- n.remain[-samp]
  }
  return(res)
}