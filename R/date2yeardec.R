#' @title Date - Year conversion
#'
#' @description Convert dates to numeric years with decimal as fraction of a year
#'
#' @param date a date (class 'Date')
#'
#' @examples
#' date2yeardec(Sys.Date())
#'
#' @return a scalar (class 'numeric')
#' @export

date2yeardec <- function(date){
  # adapted from lubridate::decimal_date.default
  if (any(!inherits(date, c("POSIXt", "POSIXct", "POSIXlt", "Date")))){
    stop("date(s) not in POSIXt or Date format")
  }
  Y <- as.numeric(format(date, format="%Y"))
  start <- as.POSIXct(paste0(Y,  "/01/01"), tz="UTC")
  end   <- as.POSIXct(paste0(Y+1,"/01/01"), tz="UTC")
  sofar <- as.numeric(difftime(date, start, units = "secs"))
  total <- as.numeric(difftime(end, start, units = "secs"))
  res <- Y + sofar/total
  return(res)
}
