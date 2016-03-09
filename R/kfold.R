#' Determine k-fold partitions for a given number of samples
#'
#' @param n number of samples
#' @param k number of partitions
#'
#' @return a list containing sample numbers in each partition
#' @details For specific use in cross validation (see \code{\link{cv.nperm}})
#' @export
#'
#' @examples
#' res <- kfold(100,6)
#' res
#' length(res) # partition indices
#' lapply(res, length) # number of samples in each partition
#' 
kfold <- function(n, k){
  res <- vector(mode="list", k)
  n.remain <- seq(n)
  for(i in seq(k)){
    samp <- sample(n.remain, ceiling(length(n.remain)/(k-i+1)))
    res[[i]] <- samp
    n.remain <- n.remain[-match(samp, n.remain)]
  }
  return(res)
}