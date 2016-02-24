#' Identify shared values between two vectors
#'
#' @param x first vector of values
#' @param y second vector of values
#'
#' @return Vector of shared values
#' @export
#'
#' @examples
#' 
#' x <- 1:5
#' y <- c(1,3,5,7,9)
#' sharedVals(x,y)
#' 
sharedVals <- function(x, y){
  xincl <- x[which(x %in% y)]
  incl <- y[which(y %in% xincl)]
  return(incl)
}


