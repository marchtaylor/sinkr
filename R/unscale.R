#' Unscale a matrix
#' 
#' The \code{unscale} function unscales a numeric marteix that has been either 
#' centered or scaled by the \code{\link[base]{scale}} function. This is done by 
#' reversing the first unscaling and then uncentering based on the 
#' object's attributes.
#' 
#' @param x a numeric matrix(like object) that has been centered and/or scaled by the 
#'  \code{\link[base]{scale}} function.
#' @param uncenter a logical value defining whether to uncenter \code{x}.
#' @param unscale a logical value defining whether to unscale \code{x}.
#' 
#' @examples
#' x <- matrix(1:16, 4, 4)
#' xcs <- scale(x, center=TRUE, scale=TRUE) # centered and scaled
#' xc <- scale(x, center=TRUE, scale=FALSE) # centered only
#' xs <- scale(x, center=FALSE, scale=TRUE) # scaled only
#' 
#' # compare difference to original
#' x - unscale(xcs)
#' x - unscale(xc)
#' x - unscale(xs)
#' 
#' @export
#' 
unscale <- function(x, unscale=TRUE, uncenter=TRUE){
	if(unscale & !is.null(attr(x, "scaled:scale"))){
		x <- scale(x, center=FALSE, scale=1/attr(x,"scaled:scale"))
		attr(x,"scaled:scale") <- NULL
	}
	if(uncenter & !is.null(attr(x, "scaled:center"))){
		x <- scale(x, center=-1*attr(x,"scaled:center"), scale=FALSE)
		attr(x,"scaled:center") <- NULL
	}
	return(x)
}
