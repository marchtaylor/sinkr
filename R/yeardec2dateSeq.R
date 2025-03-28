
#' Year-decimal to date sequence wrapper
#'
#' @param from numeric A year-decimal value to convert to the starting date.
#' @param to numeric A year-decimal value to convert to the ending date. 
#' @param by increment of the sequence. Optional. 
#'   See ‘Details’ in \code{\link[base]{seq.Date}}.
#' @param ... Further arguments to pass to \code{\link[base]{seq.Date}}.
#' 
#' @description
#' The function is helpful for quickly defining a vector of dates that e.g. can 
#'   be used to define gridlines in a plot. 
#' 
#'
#' @return A vector of class \code{"Date"}.
#' @export
#' 
#'
#' @examples
#' # the long way
#' seq.Date(as.Date("2000-01-01"), as.Date("2010-01-01"), by = "year")
#' 
#' # the shorter way 
#' yeardec2dateSeq(2000, 2010, by = "year")
#' 
#' # use other 'by' settings
#' yeardec2dateSeq(2000, 2001.99, by = "month")
#' 
#' # use in defining gridlines
#' set.seed(1)
#' x <- seq.Date(as.Date("2000-01-01"), as.Date("2003-12-31"), by = "day")
#' y <- cumsum(rnorm(length(x)))
#' plot(x,y, t = "l")
#' abline(v = yeardec2dateSeq(2000, 2010, by = "3 month"), lty = 3, col = 8)
#' 
yeardec2dateSeq <- function(from, to, by, ...){
  from. <- yeardec2date(from)
  to. <- yeardec2date(to)
  return(seq.Date(from = from., to = to., by = by, ...))
}
