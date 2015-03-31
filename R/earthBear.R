#' @title Directional bearing between two geographic locations
#' @description \code{earthBear} calculates bearing (in degrees) between two 
#' lon/lat positions.
#' One of the lon/lat positions (i.e. lon1 and lat1) can be a vector 
#' of positions to compare against the other lon/lat position (i.e. lon2 and lat2)
#' 
#' @param lon1 longitude 1 (in decimal degrees)
#' @param lat1 Latitude 1 (in decimal degrees)
#' @param lon2 longitude 2 (in decimal degrees)
#' @param lat2 Latitude 2 (in decimal degrees)
#' 
#' @return Vector of directional bearings (degrees)
#' 
#' @examples
#' earthBear(0,0,20,20)
#' 
#' @keywords geographic
#' 
#' @export
#' 
#' 
earthBear <- function (lon1, lat1, lon2, lat2) 
{
    rad <- pi/180
    a1 <- lat1 * rad
    a2 <- lon1 * rad
    b1 <- lat2 * rad
    b2 <- lon2 * rad
    dlon <- b2 - a2
    bear <- atan2(sin(dlon) * cos(b1), cos(a1) * sin(b1) - sin(a1) * 
        cos(b1) * cos(dlon))
    deg <- (bear%%(2 * pi)) * (180/pi)
    return(deg)
}

