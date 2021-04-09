#' Return symbols for corresponding p-values
#'
#' @param x numeric vector of p-values
#' @param breaks numeric vector of breaks (passed to \code{\link[base]{cut}})
#' @param symbols character vector of text output corresponding to cut levels 
#'
#' @return character vector
#' @export
#'
#' @examples
#' 
#' ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
#' trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
#' group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
#' weight <- c(ctl, trt)
#' lm.D9 <- lm(weight ~ group)
#' summary(lm.D9) # summary table shows stars for signifcance levels
#' Pvals <- summary(lm.D9)[[4]][,4] # extract p-values
#' stars(x = Pvals) # reproduce stars
#' 
#' 
starSym <- function(x, breaks = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
  symbols = c("***", "**", "*", ".", "")){
  siglev <- cut(x = x, breaks = breaks)
  return(symbols[as.numeric(siglev)])
}