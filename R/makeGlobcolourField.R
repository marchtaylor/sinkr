#' Make a field from Globcolour data
#'
#' @param dataPath Path to data
#' @param yearRan Range of years to use for field
#' @param lonRan Range of longitude to use for field
#' @param latRan Range of latitude to use for field
#' @param greps Text patterns to use to filter data (i.e. filter file names
#' that only contain these text strings)
#'
#' @return Matrix field where rows and columns are time and space, respectively
#' @export
#'
makeGlobcolourField <- function(
	dataPath=getwd(),
	yearRan=c(1997,2013),
	lonRan=c(-83,-79),
	latRan=c(-16,-4),
	greps=c(".nc", "GSM", "MO")
){
	
	#Import data
	grd <- list()
	chl <- list()
	date <- list()
	iter <- 0

	tmp <- list.files(dataPath)
	for(i in seq(greps)){
		tmp <- tmp[grep(greps[i], tmp, fixed=TRUE)]
	}

	list_obs <- data.frame(file_name=tmp, date=format(strptime(substr(tmp, 5, 12), "%Y%m%d")))
	list_obs <- list_obs[(as.POSIXlt(list_obs$date, tz="GMT")$year+1900) %in% seq(yearRan[1], yearRan[2]),]

	for (j in 1:length(list_obs[,1])){  #loop_2: for each date j
		iter <- iter+1
		nc <- ncdf4::nc_open(paste(dataPath,as.character(list_obs$file_name[j]),sep=""))
		# nc <- open.ncdf(paste(dataPath,as.character(list_obs$file_name[j]),sep=""))   # old version using package "ncdf"
		idxrow = ncdf4::ncvar_get(nc, varid="row")
		idxcol = ncdf4::ncvar_get(nc, varid="col")
		val = ncdf4::ncvar_get(nc, varid="CHL1_mean")
		ncdf4::nc_close(nc)
		rm("nc")
		isin <- isin.convert(coord=data.frame(row=idxrow, col=idxcol))
		keep <- which(isin$lon >= lonRan[1] & isin$lon <= lonRan[2] & isin$lat >= latRan[1] & isin$lat <= latRan[2])
		date[[iter]] <- as.character(rep(list_obs$date[j], length(val[keep])))
		grd[[iter]] <- isin$grd[keep]
		chl[[iter]] <- val[keep]
		print(as.character(list_obs[j,2]))
	}
 
	#unite data into dataframe
	DB <- data.frame(
		date=unlist(date),
		grd=unlist(grd), 
		chl=unlist(chl)
	)
	#head(DB)
	#tail(DB)
	#dim(DB)
 
	###Fast way of filling sparse data into a field
 	#create lookup tables for unique dates and grids
	#lookup for dates
	u.date <- unique(DB$date)
	u.date <- u.date[order(u.date)]
	date.lut <- data.frame(date=u.date, id=seq(u.date))
	DB$dateid <- date.lut$id[match(DB$date, date.lut$date)]
	#lookup for grids
	u.grd <- unique(DB$grd)
	u.grd <- u.grd[order(u.grd)]
	grd.lut <- data.frame(grd=u.grd, id=seq(u.grd))
	DB$grdid <- grd.lut$id[match(DB$grd, grd.lut$grd)]
 
	#Create sparse matrix
	field <- Matrix::sparseMatrix(i=DB$dateid, j=DB$grdid, x=DB$chl)
	dim(field)
 
	#Convert field object to class matrix and give appropriate row and column names
	field <- as.matrix(field)
	field[which(field==0)] <- NaN
	rownames(field) <- as.character(date.lut$date)
	colnames(field) <- grd.lut$grd

	field 
	
}

