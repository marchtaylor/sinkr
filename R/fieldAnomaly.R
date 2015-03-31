#' Calculate the daily or monthly anomaly of a field
#' 
#' The \code{fieldAnomaly} function calculates an anomaly field by substraction the daily or 
#' monthly means from each spatial location (columns) of a field.
#' 
#' @param x A vector of the class "POSIXlt" containing the dates that correspond
#' to the rows of matrix \code{y}.
#' @param y A matrix of the spatio-temporal field with rows being the temporal 
#' dimension and columns being the spatial dimension
#' @param level Character string. Anomaly to compute ("daily" or "monthly")
#' 
#' @examples
#' set.seed(1)
#' Time <- seq.Date(as.Date("1990-01-01"), as.Date("1999-12-01"), by="month")
#' Space <- seq(0,pi,,30)
#' Signal <- sin(as.POSIXlt(Time)$mon/2)
#' Z <- outer(Signal, sin(Space))
#' Noise <- Z * 0.1 * array(rnorm(length(Z)), dim=dim(Z))
#' Z <- Z + Noise
#' Z.anom <- fieldAnomaly(y=Z, x=as.POSIXlt(Time), level="monthly")
#' 
#' zran <- c(-1,1) * max(abs(range(Z, Z.anom)))
#' pal <- colorRampPalette(c("blue", "cyan", "grey", "yellow", "red"))
#' op <- par(no.readonly=TRUE)
#' layout(matrix(c(1,2,3,3), nrow=2, ncol=2), widths=c(4,1), heights=c(2,2))
#' par(mar=c(5,5,3,1))
#' image(Time, Space, Z, col=pal(100), zlim=zran, main="Original")
#' image(Time, Space, Z.anom, col=pal(100), zlim=zran, main="Anomaly")
#' par(mar=c(5,0,3,5))
#' imageScale(Z, col=pal(100), zlim=zran, axis.pos=4)
#' mtext("Value", side=4, line=3)
#' par(op)
#' 
#' @export
#' 
fieldAnomaly <- function(y, x, level="daily"){ 

	y <- as.matrix(y)

	if(level=="monthly"){levs=unique(x$mon)}
	if(level=="daily"){levs=unique(x$yday)}

	levs_lookup=vector("list", length(levs))
	names(levs_lookup) <- levs
	for(i in 1:length(levs)){
		if(level=="monthly"){levs_lookup[[i]] <- which(x$mon == names(levs_lookup[i]))}
		if(level=="daily"){levs_lookup[[i]] <- which(x$yday == names(levs_lookup[i]))}
	}

	for(j in seq(levs)){     #for every time level
    y[levs_lookup[[j]],] <- t(t(as.matrix(y[levs_lookup[[j]],])) - apply(as.matrix(y[levs_lookup[[j]],]), 2, mean, na.rm=TRUE))
	}

	y

}

