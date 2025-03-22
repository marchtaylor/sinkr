#' @name sst 
#' @title Kaplan monthly mean anomaly dataset
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
#' library(maps)
#' data(sst)
#' plot(sst$grid, col=val2col(sst$field[1000,]), pch=".", cex=20)
#' map("world", add=TRUE)
#' 
#' 
#' 
NULL

