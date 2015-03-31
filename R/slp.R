#' @name slp 
#' @title Hadley SLP monthly mean dataset
#' 
#' @description The \code{slp} data set contains monthly sea level pressure data for the Equatorial Pacific
#' within the lat/lon range 180 W - 70 W and 30 S - 30 N, and spanning the years 1971-1998.
#' 
#' \itemize{
#'   \item grid. Dateframe of lon/lat coordinates corresponding to columns of slp$field (5 deg resolution)
#'   \item date. Vector of monthly date values corresponding to rows of slp$field
#'   \item field. Matrix of sea level pressure values by month and lon/lat position.
#' }
#' 
#' @docType data
#' @format A list consisting of: 1. a dataframe for lon/lat positions, 2. a vector of
#' monthly date values, and 3. a matrix of slp values by month and lon/lat position 
#' (1536 rows, 231 columns)
#' @source \url{http://www.esrl.noaa.gov/psd/gcos_wgsp/Gridded/data.hadslp2.html}
#' @usage data(slp)
#' @keywords datasets field climate
#' @examples
#' 
#' ### Ex 1. Plot of single month
#' data(slp)
#' plot(slp$grid, col=val2col(slp$field[1000,]), pch=".", cex=30)
#' 
#' 
#' 
#' ### Ex 2. EOF of slp
#' \donttest{
#' library(maps)
#' 
#' # Calculate monthly anomaly
#' slp.anom <- fieldAnomaly(slp$field, as.POSIXlt(slp$date), level="monthly")
#' 
#' # PCA
#' pca <- prcomp(slp.anom)
#' 
#' # Plot
#' neof <- 2 # EOF mode number to plot
#' op <- par(no.readonly=TRUE)
#' layout(matrix(c(1:2,3,3), nrow=2, ncol=2, byrow=TRUE), widths=c(4,1), heights=c(3,2))
#' layout.show(3)
#' # plot 1: map of PC loadings
#' par(mar=c(3,3,1,1))
#' zlim <- c(-1,1) * max(abs(pca$rotation[,neof]))
#' pal <- colorRampPalette(c(4,5,"grey90",7,2))
#' ncolor <- 25
#' plot(slp$grid, col=val2col(pca$rotation[,neof], zlim=zlim, col=pal(ncolor)), 
#'      pch=".", cex=30,  ylab="", xlab="")
#' map("world", add=TRUE)
#' # plot 2: color scale
#' par(mar=c(3,0,1,4))
#' imageScale(pca$rotation[,neof], axis.pos=4, zlim=zlim, col=pal(ncolor))
#' # plot 3: time series of PC
#' par(mar=c(3,3,1,1))
#' plot(slp$date, pca$x[,neof], t="l", ylab="", xlab="")
#' abline(h=0, col="grey", lty=2)
#' par(op)
#' }
#' 
#' 
NULL

