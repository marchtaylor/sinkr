#' List top n objects by size
#' 
#' The function \code{lsos} lists the top \code{n} objects in memory.
#' 
#' @param pos where to look for the object (see \code{\link[base]{get}}, 'Details'); 
#' if omitted search as if the name of the object appeared unquoted in an expression.
#' @param pattern an optional regular expression (see \code{\link[base]{ls}}). Only names matching pattern are returned. 
#' see \code{\link[utils]{glob2rx}} can be used to convert wildcard patterns to regular expressions.
#' @param order.by Order list by one of the following: \code{"Type", "Size", "Rows", "Columns"}
#' @param decreasing Logical. Decreasing in size (i.e. largest objects at top of list.) 
#' (Default=\code{TRUE})
#' @param head Logical (Default=\code{TRUE}). Should only the \code{\link[utils]{head}} be 
#' returned in the results of a \code{data.frame}. Argument \code{n} defines how many objects to include.
#' @param n Numeric value (Default=\code{10}). Defines how many objects to include in results
#' 
#' @return a \code{data.frame} object
#' 
#' @references
#' \url{http://stackoverflow.com/questions/1358003/tricks-to-manage-the-available-memory-in-an-r-session}
#' 
#' @examples
#' x1 <- matrix(rnorm(10000), 100, 100)
#' x2 <- as.data.frame(matrix(rnorm(1000), 100, 10))
#' x3 <- rnorm(1000)
#' lsos()
#' rm(list=c("x1", "x2", "x3"))
#' 
#' @export
#' 
#' 
lsos <- function(pos = 1, pattern, order.by="Size", decreasing=TRUE, head=TRUE, n=10) {

  .ls.objects <- function (pos = 1, pattern, order.by, decreasing=TRUE, head=TRUE, n) {
    napply <- function(names, fn) sapply(names, function(x) fn(get(x, pos = pos)))
    names <- ls(pos = pos, pattern = pattern)
    obj.class <- napply(names, function(x) as.character(class(x))[1])
    obj.mode <- napply(names, mode)
    obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
    obj.size <- napply(names, object.size)
    obj.dim <- t(napply(names, function(x)
                        as.numeric(dim(x))[1:2]))
    vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
    obj.dim[vec, 1] <- napply(names, length)[vec]
    out <- data.frame(obj.type, obj.size, obj.dim)
    names(out) <- c("Type", "Size", "Rows", "Columns")
    if (!missing(order.by))
        out <- out[order(out[[order.by]], decreasing=decreasing), ]
    if (head)
        out <- head(out, n)
    out
  }
  
  .ls.objects(pos=pos, order.by=order.by, decreasing=decreasing, head=head, n=n)

}

