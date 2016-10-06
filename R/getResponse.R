#' Return response variable name from a fitted model
#'
#' @param formula or fitted model object
#'
#' @return a vector of response variable names as character strings
#' @details Works for lm, glm, gam and gls model objects - maybe others as well.
#' 
#' @importFrom stats terms
#' @export
#'
#' @examples
#' getResponse(formula(y ~ x)) # single response variable
#' getResponse(formula(y1 + y2 ~ x)) # multiple response variables
#' 
#' x <- runif(10)
#' y <- 2*x + rnorm(10)
#' fit <- lm(y ~ x)
#' getResponse(fit) # usin a fitted lm model
#' 
#' fit <- glm(y ~ x)
#' getResponse(fit) # usin a fitted glm model
#' 
getResponse <- function(formula) {
  tt <- terms(formula)
  vars <- as.character(attr(tt, "variables"))[-1] ## [1] is the list call
  response <- attr(tt, "response") # index of response var
  resp <- vars[response] 
  resp <- unlist(strsplit(resp, " ", fixed=TRUE))
  if(length(which(resp == "+") > 0)){resp <- resp[-which(resp == "+")]}
  return(resp)
}