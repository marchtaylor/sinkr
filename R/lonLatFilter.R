#' @title Filter lon/lat positions that fall within defined boundaries
#' @description \code{lonLatFilter} Tests whether lon/lat positions fall within a defined 
#' box of lon/lat borders (location on border returns \code{TRUE}
#' 
#' @param lon_vector Longitude 1 (in decimal degrees)
#' @param lat_vector Latitude 1 (in decimal degrees)
#' @param west West longitude border (decimal degrees)
#' @param east East longitude border (decimal degrees)
#' @param north North latitude border (decimal degrees)
#' @param south South latitude border (decimal degrees)
#' 
#' 
#' @return Vector of position inclusion in the defined lon/lat borders
#' 
#' @examples
#' set.seed(1)
#' n <- 1000
#' Pos <- list(lon=runif(n, min=-180, max=180), lat=runif(n, min=-180, max=180))
#' # Check to see if positions are withing boundaries
#' res <- lonLatFilter(Pos$lon, Pos$lat, -20, 20, -40, 40) 
#' # Plot
#' op <- par(mar=c(4,4,1,1))
#' plot(Pos$lon, Pos$lat, pch=21, col=1, bg=2*res)
#' rect(-20, -40, 20, 40, border=3)
#' par(op)
#' 
#' @keywords geographic
#' 
#' @export
#' 
lonLatFilter <- function (lon_vector, lat_vector, west, east, south, north) 
{
	if(west>east) {
		lon_vector_new=replace(lon_vector, which(lon_vector<0), lon_vector[which(lon_vector<0)]+360)
		east_new=east+360
	} else {
	lon_vector_new=lon_vector
	east_new=east
	}

	res <- (lon_vector_new < east_new & lon_vector_new > west & lat_vector < north & lat_vector > south)
  res
  
}
