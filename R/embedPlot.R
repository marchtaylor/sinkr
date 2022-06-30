#' Embed a new plot in a relative position of an existing plot region
#'
#' @param expr expression. Defines a call to a new plot (must contain a 
#'   high-level call). Use curly brackets to contain multiple line evaluations.
#' @param relx0 numeric. Value between 0-1 defining the position of 
#'   the left boundary of the new plot relative to the original plot region. 
#' @param relx1 numeric. Value between 0-1 defining the position of 
#'   the right boundary of the new plot relative to the original plot region
#' @param rely0 numeric. Value between 0-1 defining the position of 
#'   the lower boundary of the new plot relative to the original plot region
#' @param rely1 numeric. Value between 0-1 defining the position of 
#'   the upper boundary of the new plot relative to the original plot region
#' @param revert_mai logical. Should the function revert to the par of the 
#'   original plot on exit.
#'
#' @return An embedded plot in a relative position of the existing plot.
#' 
#' @export
#'
#' @examples
#' 
#' op <- par(no.readonly = TRUE) # original pars for resetting
#' 
#' # error given when expression argument missing
#' image(volcano)
#' embedPlot()
#' 
#' # addition of histogram
#' image(volcano)
#' embedPlot(expr = expression(hist(c(volcano), main = "", xlab = "")), 
#'   relx0 = 0.6, relx1 = 0.95, rely0 = 0.6, rely1 = 0.95)
#' 
#' # addition of elevation scale
#' image(volcano)
#' embedPlot(expr = expression({
#'     imageScale(volcano, axis.pos = 1)
#'     mtext("Elevation [m]", side = 1, line = 2)}), 
#'   relx0 = 0.5, relx1 = 0.95, rely0 = 0.9, rely1 = 0.95)
#' 
#' # revert_mai = FALSE allows further lower level calls to new plot region
#' image(volcano)
#' embedPlot(expr = expression({
#'     imageScale(volcano, axis.pos = 1)}), 
#'   relx0 = 0.5, relx1 = 0.95, rely0 = 0.9, rely1 = 0.95, 
#'   revert_mai = FALSE)
#' abline(v = seq(100,200, 20))
#' mtext("Elevation [m]", side = 1, line = 2) # additions outside of embedPlot
#' par(op) # reset to original par
#' 
#' 
embedPlot <- function(
  expr = expression({
    plot(1, t = "n", axes = FALSE, xlab = "", ylab = "")
    text(1,1, font = 3, 
      labels = "embedPlot error:\nexpression\nnot defined")}), 
  relx0 = 0.5, relx1 = 0.95, rely0 = 0.6, rely1 = 0.95,
  revert_mai = TRUE
){
  # record main plot par
  # mp <- par("mai", "mar")
  mp <- par(no.readonly = TRUE)
  
  # set new margins (par()$mai)
  mai <- par("mai")
  mai[1] <- mai[1] + rely0*par("pin")[2]
  mai[2] <- mai[2] + relx0*par("pin")[1]
  mai[3] <- mai[3] + (1-rely1)*par("pin")[2]
  mai[4] <- mai[4] + (1-relx1)*par("pin")[1]
  par(mai = mai) # define
  
  par(new = TRUE) # prevents cleaning the frame before drawing
  eval(expr = expr) # evaluate expression
  
  if(revert_mai){on.exit(par(mp))}
}


