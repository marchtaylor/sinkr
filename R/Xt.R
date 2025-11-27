#' @name Xt 
#' @title Synthetic matrix dataset
#' @description The \code{Xt} data is a matrix comprised of 9 signals, usfull 
#'   for exploring PCA/EOF methods. 
#' 
#' @docType data
#' @format A matrix of 100 rows and 50 columns.
#' @source Beckers, J.M., Rixen, M., 2003. EOF Calculations and Data Filling 
#'   from Incomplete Oceanographic Datasets. 
#'   J. Atmos. Oceanic Technol. 20, 1839–1856. 
#'   \url{https://doi.org/10.1175/1520-0426(2003)020<1839:ECADFF>2.0.CO;2}
#' @usage data(Xt)
#' @keywords datasets multivariate
#' @examples
#' 
#' data(Xt)
#' image(x = seq(nrow(Xt)), y = seq(ncol(Xt)), z = Xt)
#' 
#' 
NULL