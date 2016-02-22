#' Create a sequence of defined length over range of a vector
#'
#' @param x Numeric vector
#' @param length.out Length of sequence to create, Defaults to length
#' of \code{x}.
#' @param rel.ext Relative extention of sequence limits. 
#' Default \code{rel.xt = c(0,0)} maintains original range of \code{x} 
#' @param ... Additional parameters to pass to \code{\link[base]{seq}}
#'
#' @return Vector
#' @export
#'
#' @examples
#' x <- c(0,1,3,4)
#' seqRan(x) # default settings. Returns vector of length of x
#' seqRan(x, 10) # using original range of x
#' seqRan(x, 10, c(-0.1, 0.1)) # using extended range of x
#' 
seqRan <- function(x, length.out=NULL, rel.ext=c(0,0), ...){
  if(is.null(length.out)) length.out <- length(x)
  xran <- range(x)
  xspan <- diff(xran)
  xran <- xran + xspan*rel.ext
  return(seq(xran[1], xran[2], length.out = length.out, ...))
}

