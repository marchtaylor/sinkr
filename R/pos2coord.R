#A function that is sometimes useful in determining the 
#coordinate(i.e. row and column number) of a matrix position
#(and vice-versa). 
#Either a vector of positions ("pos") 
#OR a 2 column matrix of matrix coordinates, ("coord", i.e. cbind(row,col)), 
#AND the matrix dimentions must be supplied (dim.mat, i.e. c(nrow,ncol)).
pos2coord<-function(pos=NULL, coord=NULL, dim.mat=NULL){
	if(is.null(pos) & is.null(coord) | is.null(dim.mat)){
		stop("must supply either 'pos' or 'coord', and 'dim.mat'")
	}
	if(is.null(pos) & !is.null(coord) & !is.null(dim.mat)){
		pos <- ((coord[,2]-1)*dim.mat[1])+coord[,1] 
		return(pos)
	}
	if(!is.null(pos) & is.null(coord) & !is.null(dim.mat)){
		coord <- matrix(NA, nrow=length(pos), ncol=2)
		coord[,1] <- ((pos-1) %% dim.mat[1]) +1
		coord[,2] <- ((pos-1) %/% dim.mat[1]) +1
		return(coord)
	}
}

