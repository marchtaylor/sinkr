
#' Calculate grid and polygon coordinates for 4 km ISIN grid
#' 
#' @description This function is used to convert information regarding the 
#' ISIN grid information used by Globcolour as well as to construct 
#' associated polygons for use in mapping. 
#' The raw Globcolour .nc files come with column and row pointers 
#' as to the the grid's location. For 4.63 km resolution data ("L3b"), this
#' translates to 4320 latitudinal rows with varying number of 
#' associated longitudinal columns depending on the latitudinal
#' circumference. 
#' Input must be either a vector of grid numbers, \code{grd}, or a dataframe
#' with column and row identifiers, \code{coord}. ssee Globcolour's "Product 
#' User Guide" for fulther details of the ISIN grid 
#' (\url{http://www.globcolour.info/products_description.html}).
#' 
#'
#' @param grd numeric vector containing ISIN grid numbers needed for calculating 
#' lon/lat coordinates or polygons.
#' @param coord data.frame containing ISIN grid coordinates (row and column 
#' locations). Names of columns should be \code{coord$row} and \code{coord$col}.
#' @param polygons Logical. Should grid polygon corner coordinates be returned.
#'
#' @return If "\code{polygons = TRUE}", then returns grid information 
#' as a dataframe, including lon / lat values of grid centers. If 
#' "\code{polygons = TRUE}", then returns a list with
#' polygon corners in a dataframe(longitudinal coordinates of corners (e.g. 
#' \code{[[i]]$x}) and latitudinal coordinates of corners ( 
#' \code{[[i]]$y}))
#' 
#' @export
#'
#' 
isin.convert <- function(grd=NULL, coord=NULL, polygons=FALSE){

###ISIN grid definition
	Re=6378.145
	#NUMROWS
	Nlat=4320

	dr=(pi*Re)/Nlat  #lat bin width
	dPHI=pi/Nlat

	PHI = -(pi/2)+(1:Nlat)*dPHI-(dPHI/2)

	p=2*pi*Re*cos(PHI)
	Nlon=round(p/dr, 0)
	dlon=p/Nlon
	dphi=(2*pi)/Nlon
	Ntot=sum(Nlon)
	lat=PHI*180/pi

	if(!missing(grd)){
		#calc. coord
		cumNlon <- c(0,cumsum(Nlon))
		g.row <- sapply(grd, fun = function(x){max(which(cumNlon < x))})
		g.col <- grd - cumNlon[g.row]
		#calc. lonlat
		g.lat = lat[g.row]
		Nlon_row = Nlon[g.row]
		g.lon <- (360*(g.col-0.5)/Nlon_row) - 180
	}

	if(!missing(coord)){
		#calc. coord
		g.row <- coord$row
		g.col <- coord$col
		#calc. lonlat
		g.lat = lat[g.row]
		Nlon_row = Nlon[g.row]
		g.lon <- (360*(g.col-0.5)/Nlon_row) - 180
		#calc. grd
		cumNlon <- c(0,cumsum(Nlon))
		grd <- cumNlon[g.row] + g.col
	}

	if(polygons){
		Nlon_row = Nlon[g.row]
		g.width <- 360/Nlon_row
		g.height <- 180/Nlat
		tmp <- data.frame(g.lon=g.lon, g.lat=g.lat, g.width=g.width, g.height=g.height)
		polys <- vector(mode="list", length(grd))
		for(i in seq(polys)){
			xs <- c(tmp$g.lon[i]-tmp$g.width[i]/2, tmp$g.lon[i]-tmp$g.width[i]/2, tmp$g.lon[i]+tmp$g.width[i]/2, tmp$g.lon[i]+tmp$g.width[i]/2)
			ys <- c(tmp$g.lat[i]-tmp$g.height[i]/2, tmp$g.lat[i]+tmp$g.height[i]/2, tmp$g.lat[i]+tmp$g.height[i]/2, tmp$g.lat[i]-tmp$g.height[i]/2)
			polys[[i]] <- data.frame(x=xs, y=ys)
		}
		return(polys)
	}

	if(!polygons){
		data.frame(grd=grd, col=g.col, row=g.row, lon=g.lon, lat=g.lat)	
	}

}
