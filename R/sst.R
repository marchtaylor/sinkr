#' @name sst 
#' @title Kaplan monthly mean anomaly dataset
#' 
#' 
#' @description The \code{sst} data set contains monthly sea surface temperature anomaly data 
#' for the Equatorial Pacific
#' within the lat/lon range 180 W - 70 W and 30 S - 30 N, and spanning the years 1956-2014.
#' 
#' \itemize{
#'   \item grid. Dateframe of lon/lat coordinates corresponding to columns of sst$field (5 deg resolution)
#'   \item date. Vector of monthly date values corresponding to rows of sst$field
#'   \item field. Matrix of sea level pressure values by month and lon/lat position.
#' }
#' 
#' @docType data
#' @format A list consisting of: 1. a dataframe for lon/lat positions, 2. a vector of
#' monthly date values, and 3. a matrix of sst anomaly values by month and lon/lat position 
#' (1906 rows, 264 columns)
#' @source \url{http://www.esrl.noaa.gov/psd/data/gridded/data.kaplan_sst.html}
#' @usage data(sst)
#' @keywords datasets field climate
#' @examples
#' 
#' ### Ex 1. Plot of single month
#' data(sst)
#' plot(sst$grid, col=val2col(sst$field[1000,]), pch=".", cex=30)
#' 
#' 
#' 
#' ### Ex 2. EOF of sst
#' \donttest{
#' library(maps)
#' 
#' # PCA
#' incl <- which(colSums(is.na(sst$field)) == 0) # remove columns corresponding to land
#' pca <- prcomp(sst$field[,incl])
#' 
#' # Plot
#' neof <- 1 # EOF mode number to plot
#' 
#' op <- par(no.readonly=TRUE)
#' layout(matrix(c(1:2,3,3), nrow=2, ncol=2, byrow=TRUE), widths=c(4,1), heights=c(3,2))
#' layout.show(3)
#' # plot 1: map of PC loadings
#' par(mar=c(3,3,1,1))
#' zlim <- c(-1,1) * max(abs(pca$rotation[,neof]))
#' pal <- colorRampPalette(c(4,5,"grey90",7,2))
#' ncolor <- 25
#' plot(sst$grid[incl,], col=val2col(pca$rotation[,neof], zlim=zlim, col=pal(ncolor)), 
#'      pch=".", cex=30,  ylab="", xlab="")
#' map("world", add=TRUE)
#' # plot 2: color scale
#' par(mar=c(3,0,1,4))
#' imageScale(pca$rotation[,neof], axis.pos=4, zlim=zlim, col=pal(ncolor))
#' # plot 3: time series of PC
#' par(mar=c(3,3,1,1))
#' plot(sst$date, pca$x[,neof], t="l", ylab="", xlab="")
#' abline(h=0, col="grey", lty=2)
#' par(op)
#' }
#' 
#' 
NULL

