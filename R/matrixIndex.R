#' Convert matrix element index to row/column coordinate or vice-versa
#'
#' @param idx numeric. Index of a matrix element
#' @param coord numeric vector or matrix. Coordinate of an index (row,col)
#' @param dim.mat numeric vector. Dimensions of matrix (rows, ncols)
#'
#' @return either a vector (indices) or matrix (coordinate) element
#' @export
#'
#' @examples
#' M <- matrix(1:20, 4,5)
#' M
#' matrixIndex(idx = 15, dim.mat = dim(M))
#' matrixIndex(coord = c(3,4), dim.mat = dim(M))
#' matrixIndex(coord = matrix(c(3,4,4,5), ncol=2, byrow=TRUE), 
#'   dim.mat = dim(M))
#' 
#' 
matrixIndex <- function(idx = NULL, coord = NULL, dim.mat = NULL){
	if(is.null(idx) & is.null(coord) | is.null(dim.mat)){
		stop("must supply either 'idx' or 'coord', and 'dim.mat'")
	}
	if(is.null(idx) & !is.null(coord) & !is.null(dim.mat)){
		if(!is.matrix(coord)) coord <- matrix(coord, ncol = 2, byrow = TRUE)
		idx <- ((coord[,2]-1)*dim.mat[1])+coord[,1] # position
		return(idx)
	}
	if(!is.null(idx) & is.null(coord) & !is.null(dim.mat)){
    coord <- matrix(NA, nrow = length(idx), ncol=2)
    colnames(coord) <- c("row", "col")
		coord[,1] <- ((idx-1) %% dim.mat[1]) +1
		coord[,2] <- ((idx-1) %/% dim.mat[1]) +1
		return(coord)
	}
}