#' Scale an array along a single dimension
#'
#' @param arr array. Object to scale.
#' @param dim_to_scale numeric. Dimension
#' @param center logical. Passed to \code{\link[base]{scale}}
#' @param scale logical. Passed to \code{\link[base]{scale}}
#'
#' @return array
#' @export
#'
#' @examples
#' 
#' set.seed(1111)
#' A <- array(rnorm(24), dim = c(2,3,4), 
#'   dimnames = list(
#'     paste("a", seq(2), sep = "~"),
#'     paste("b", seq(3), sep = "~"),
#'     paste("c", seq(4), sep = "~")
#'   )
#' )
#' A
#' Asc <- scale_along(A, 3)
#' Asc
#' apply(Asc, 1:2, mean) # mean = 0.0
#' apply(Asc, 1:2, sd) # sd = 1.0
#' 
scale_along <- function(arr, dim_to_scale = 3, center = TRUE, scale = TRUE) {
  stopifnot(is.array(arr), dim_to_scale %in% seq_along(dim(arr)))
  
  dims <- dim(arr)
  dn   <- dimnames(arr)
  other_dims <- setdiff(seq_along(dims), dim_to_scale)
  
  tmp <- apply(arr, other_dims, function(x) {
    if (all(is.na(x))) return(rep(NA, length(x)))
    as.numeric(scale(x, center = center, scale = scale))
  })
  
  # tmp has dims: (scaled dimension) × other dimensions
  # need to move first dimension back to dim_to_scale position
  perm <- c(seq_along(other_dims) + 1L, 1L)
  tmp2 <- aperm(tmp, perm)
  
  array(tmp2, dim = dims, dimnames = dn)
}
