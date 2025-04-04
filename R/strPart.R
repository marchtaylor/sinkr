#' Extract a defined part of a text string with separated elements
#' 
#' @description
#' Wrapper for \code{\link[base]{strsplit}}. One defines the element of the string
#'   to return after splitting, with option to convert back to a character vector 
#'   or leave as list. See \code{\link[base]{strsplit}} for further description of
#'   most arguments. 
#' 
#'
#' @param x character vector, each element of which is to be split. Other inputs, 
#'   including a factor, will give an error.
#' @param part numeric. Part of string (between splits) to return.
#' @param split character. The splitting character. See \code{\link[base]{strsplit}}
#' @param fixed logical. Default TRUE. See \code{\link[base]{strsplit}}
#' @param returnVector logical. Function relies on \code{\link[base]{lapply}}, 
#'   and thus returns a list. Set to 'TRUE' (Default) to return a vector. 
#' @param ... Further arguments passed to \code{\link[base]{strsplit}}.
#'
#' @return vector or list of character strings
#' @export
#'
#' @examples
#' 
#' # make example text string
#' txtStr <- paste(LETTERS[1:10], 13:22, letters[11:20], sep = "_")
#' txtStr
#' 
#' strPart(x = txtStr, part = 1, split = "_")
#' strPart(x = txtStr, part = 2, split = "_")
#' strPart(x = txtStr, part = 3, split = "_")
#' 
#' strPart(x = txtStr, part = 3, split = "_", returnVector = FALSE)
#' 
strPart <- function(x, part = 1, split = NULL, fixed = TRUE, returnVector = TRUE, ...){
  if(is.null(split)) stop("Must define 'split'")

  res <- strsplit(x = x, split = split, fixed = fixed)
  res <- lapply(res, function(x){x[part]})
  if(returnVector){ res <- unlist(res) }
  
  return(res)
}
