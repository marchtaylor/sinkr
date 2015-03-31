#' @title Directional bearing between two geographic locations
#' @description \code{newLonLat} calculates a new lon/lat position given an sarting lon/lat 
#' position, a bearing and a distance to the new lon/lat position. 
#' Either the lon and lat arguments or the bearing and distance arguments can be a vector, 
#' whereby a vector of new locations will be calculated.
#' 
#' @param lon Longitude 1 (in decimal degrees)
#' @param lat Latitude 1 (in decimal degrees)
#' @param bearing Longitude 2 (in decimal degrees)
#' @param distance Distance to new lon/lat position (km)
#' 
#' @return List of new lon/lat locations
#' 
#' @examples
#' # Single new lon/lat position calculation
#' newLonLat(0,0,45,1000)
#' 
#' # Vector of new lon/lat positions and plot
#' startPos <- list(lon=0,lat=0)
#' endPos <- newLonLat(startPos$lon, startPos$lat, seq(0,360,20), 1000)
#' plot(1, t="n", xlim=range(endPos$lon), ylim=range(endPos$lat), xlab="lon", ylab="lat")
#' segments(startPos$lon, startPos$lat, endPos$lon, endPos$lat, col=rainbow(length(endPos$lon)))
#' points(startPos$lon, startPos$lat)
#' points(endPos$lon, endPos$lat)
#' 
#' @keywords geographic
#' 
#' @export
#' 
newLonLat <-
function (lon, lat, bearing, distance) 
{
    rad <- pi/180
    a1 <- lat * rad
    a2 <- lon * rad
    tc <- bearing * rad
    d <- distance/6378.145
    nlat <- asin(sin(a1) * cos(d) + cos(a1) * sin(d) * cos(tc))
    dlon <- atan2(sin(tc) * sin(d) * cos(a1), cos(d) - sin(a1) * 
        sin(nlat))
    nlon <- ((a2 + dlon + pi)%%(2 * pi)) - pi
    npts <- list(lon=nlon/rad, lat=nlat/rad)
    return(npts)
}

