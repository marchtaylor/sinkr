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
#' library(maps)
#' data(slp)
#' plot(slp$grid, col=val2col(slp$field[1000,]), pch=".", cex=20)
#' map("world", add=TRUE)
#' 
#' 
#' 
NULL

