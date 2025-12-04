#' Calculate the daily (julian or month/day date) or monthly anomaly of a data field
#' 
#' The \code{fieldAnomaly} function calculates an anomaly field by subtracting the daily or 
#' monthly means from each spatial location (columns) of a field.
#' 
#' @param x A vector of the class "Date", "POSIXct", or "POSIXlt" containing the dates that correspond
#' to the rows of matrix \code{y}.
#' @param y A matrix of the spatio-temporal field with rows being the temporal 
#' dimension and columns being the spatial dimension
#' @param level Character string. Anomaly to compute (\code{"julian"}, \code{"day"} or 
#' \code{"month"}). 
#' \code{"julian"} will consider unique \code{as.POSIXlt(x)$yday} values, while \code{"day"} will 
#' consider unique \code{format(x,"\%m\%d")} strings.
#' 
#' @examples
#' # create synthetic data (contains annual and day signal)
#' set.seed(1)
#' Time <- seq.Date(as.Date("1990-01-01"), as.Date("1996-12-31"), by="day")
#' Space <- seq(pi,2*pi,,10)
#' Signal1 <- sin(as.POSIXlt(Time)$yday/365*(2*pi)) # annual signal
#' Signal2 <- sin(as.POSIXlt(Time)$yday/365*(24*pi)) # monthly signal
#' cumSignal <- 1*Signal1+0.3*Signal2
#' plot(Time, cumSignal, t="l")
#' abline(v=seq(as.Date("1900-01-01"), as.Date("2100-01-01"), by="year"), lty=2, col=8)
#' Z <- outer(cumSignal, sin(Space))
#' Noise <- Z * 0 * array(rnorm(length(Z)), dim=dim(Z))
#' Z <- Z + Noise
#' 
#' # anomaly by month (removes "Signal1")
#' Z.anom <- fieldAnomaly(y=Z, x=Time, level="month")
#' zran <- c(-1,1) * max(abs(range(Z, Z.anom)))
#' pal <- colorPalette(c("blue4", "cyan", "grey90", "yellow", "red4"), c(20,1,1,20))
#' op <- par(no.readonly=TRUE)
#' layout(matrix(c(1,2,3,3), nrow=2, ncol=2), widths=c(4,1), heights=c(2,2))
#' par(mar=c(5,5,3,1))
#' image(Time, Space, Z, col=pal(101), zlim=zran, main="Original")
#' image(Time, Space, Z.anom, col=pal(101), zlim=zran, main="Anomaly")
#' par(mar=c(5,0,3,5))
#' imageScale(Z, col=pal(101), zlim=zran, axis.pos=4)
#' mtext("Value", side=4, line=3)
#' par(op)
#' 
#' 
#' # anomaly by day of year (i.e. date, removes "Signal1" and "Signal2")
#' Z.anom <- fieldAnomaly(y=Z, x=Time, level="day")
#' zran <- c(-1,1) * max(abs(range(Z, Z.anom)))
#' pal <- colorPalette(c("blue4", "cyan", "grey90", "yellow", "red4"), c(20,1,1,20))
#' op <- par(no.readonly=TRUE)
#' layout(matrix(c(1,2,3,3), nrow=2, ncol=2), widths=c(4,1), heights=c(2,2))
#' par(mar=c(5,5,3,1))
#' image(Time, Space, Z, col=pal(101), zlim=zran, main="Original")
#' image(Time, Space, Z.anom, col=pal(101), zlim=zran, main="Anomaly")
#' par(mar=c(5,0,3,5))
#' imageScale(Z, col=pal(101), zlim=zran, axis.pos=4)
#' mtext("Value", side=4, line=3)
#' par(op)
#' 
#' 
#' # anomaly by julian day (day of year, removes "Signal1" and "Signal2")
#' Z.anom <- fieldAnomaly(y=Z, x=Time, level="julian")
#' zran <- c(-1,1) * max(abs(range(Z, Z.anom)))
#' pal <- colorPalette(c("blue4", "cyan", "grey90", "yellow", "red4"), c(20,1,1,20))
#' op <- par(no.readonly=TRUE)
#' layout(matrix(c(1,2,3,3), nrow=2, ncol=2), widths=c(4,1), heights=c(2,2))
#' par(mar=c(5,5,3,1))
#' image(Time, Space, Z, col=pal(101), zlim=zran, main="Original")
#' image(Time, Space, Z.anom, col=pal(101), zlim=zran, main="Anomaly")
#' par(mar=c(5,0,3,5))
#' imageScale(Z, col=pal(101), zlim=zran, axis.pos=4)
#' mtext("Value", side=4, line=3)
#' par(op)
#' 
#' 
#' # test that anomalies are removed correctly
#' Zalt <- as.matrix(apply(scale(Z),1,mean))
#' plot(Time, Zalt, t="l")
#' 
#' tmp <- aggregate(Zalt ~ as.POSIXlt(Time)$yday, FUN=mean)
#' plot(strptime(paste("2000",tmp[,1], sep="-"), format="%Y-%j"), tmp[,2],
#'      t="o", xlab="", ylab="n values", main="level = 'julian'")
#' 
#' tmp <- aggregate(Zalt ~ format(Time, "%m-%d"), FUN=mean)
#' plot(strptime(paste("2000",tmp[,1], sep="-"), format="%Y-%m-%d"), tmp[,2], 
#'      t="o", xlab="", ylab="n values", main="level = 'day'")
#' 
#' 
#' Z.anom <- fieldAnomaly(y=Zalt, x=Time, level="month")
#' sqrt(mean(Z.anom^2))
#' plot(Time, Z.anom, t="l")
#' 
#' Z.anom <- fieldAnomaly(y=Zalt, x=Time, level="day")
#' sqrt(mean(Z.anom^2))
#' plot(Time, Z.anom, t="l")
#' 
#' Z.anom <- fieldAnomaly(y=Zalt, x=Time, level="julian")
#' sqrt(mean(Z.anom^2))
#' plot(Time, Z.anom, t="l")
#' 
#' 
#' @export
#'
fieldAnomaly <- function(y, x, level="month"){ 

	y <- as.matrix(y)

	if(level=="month"){
    levs <- as.POSIXlt(x)$mon
    ulevs <- unique(levs)
    levs_lookup <- lapply(ulevs, function(x,y) which(y == x), levs)
    names(levs_lookup) <- ulevs
	}
	if(level=="julian"){
    levs <- as.POSIXlt(x)$yday
    ulevs <- unique(levs)
    levs_lookup <- lapply(ulevs, function(x,y) which(y == x), levs)
    names(levs_lookup) <- ulevs
	}
  if(level=="day"){
    levs <- format(x, format="%m%d")
    ulevs <- unique(levs)
    levs_lookup <- lapply(ulevs, function(x,y) which(y == x), levs)
    names(levs_lookup) <- ulevs
	}
  
  y.anom <- y*NaN
  for(j in seq(levs_lookup)){     # for every time level
    y.anom[levs_lookup[[j]],] <- t(t(as.matrix(y[levs_lookup[[j]],])) - apply(as.matrix(y[levs_lookup[[j]],]), 2, mean, na.rm=TRUE))
	}

	y.anom

}

