#' Calculate the nearest element in a vector as compared to a reference value
#' 
#' the \code{nearest} function returns the position of a vector that is closest to a defined 
#' value by determining the element with the smallest squared distance.
#' 
#' @param value A numeric reference value
#' @param lookup_vector The vector to compare to the reference value
#' 
#' @return Vector element index of nearest value
#' 
#' @examples
#' set.seed(1)
#' x <- runif(10, min=0, max=100)
#' res <- nearest(50, x)
#' plot(x)
#' abline(h=50, col=8, lty=2)
#' points(res, x[res], pch=20, col=2)
#' 
#' @export
#' 
nearest <- function (value, lookup_vector) 
{
	diff_sq=(value-lookup_vector)^2
	order(diff_sq, decreasing = FALSE)[1]
}
