

#' ac - short-cut to 'as.character'
#' 
#' @description Short-cut wrapper for (\code{\link[base]{as.numeric}}
#'
#' @param x object to be coerced or tested.
#' @param ... further arguments passed to or from other methods.
#'
#' @return character type vector
#' @export
#'
#' @examples 
#' 
#' x <- 1:4
#' ac(x)
#' 
ac <- function(x, ...){
  as.character(x, ...)
}
  

# 
#' an - short-cut to 'as.numeric'
#'
#' @description Short-cut wrapper for (\code{\link[base]{as.character}}
#' 
#' @param x object to be coerced or tested.
#' @param ... further arguments passed to or from other methods.
#'
#' @return numeric type vector
#' @export
#'
#' @examples
#' 
#' x <- c("1", "2", "3", "4")
#' an(x)
#' 
an <- function(x, ...){
  as.numeric(x, ...)
}
  