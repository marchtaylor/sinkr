#' @title Earth distance between two geographic locations
#' @description \code{earthDist} calculates distance (in kilometers) between two 
#' lon/lat positions.
#' The function assumes a mean equatorial Earth radius of 6378.145 km. One of the 
#' lon/lat positions (i.e. lon1 and lat1) can be a vector of positions to compare against
#' the other lon/lat position (i.e. lon2 and lat2)
#' 
#' @param lon1 longitude 1 (in decimal degrees)
#' @param lat1 Latitude 1 (in decimal degrees)
#' @param lon2 longitude 2 (in decimal degrees)
#' @param lat2 Latitude 2 (in decimal degrees)
#' 
#' @return Vector of distances (km)
#' 
#' @examples
#' earthDist(0,0,20,20)
#' 
#' @keywords geographic
#' 
#' @export
#' 
#' 
earthDist <- function (lon1, lat1, lon2, lat2) 
{
  rad <- pi/180
  a1 <- lat1 * rad
  a2 <- lon1 * rad
  b1 <- lat2 * rad
  b2 <- lon2 * rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 6378.145
  d <- R * c
  return(d)
}
