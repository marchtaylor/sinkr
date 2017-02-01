#' Genetic Algorithm Mantel
#'
#' @param fix.mat The "fixed" matrix of community or environmental sample by variable values
#' @param var.mat A "variable" matrix of community or environmental sample by variable values
#' @param fix.dist.method The method of calculating dissimilarity indices bewteen samples in the fixed
#' matrix (Uses the \code{\link[vegan]{vegdist}} function from the vegan package to calculate distance matrices. See 
#' the documentation for available methods.). Defaults to Bray-Curtis dissimularity \code{"bray"}.
#' @param var.dist.method The method of calculating dissimilarity indices bewteen samples in the variable
#' matrix. Defaults to Euclidean dissimularity \code{"euclidean"}.
#' @param scale.fix Logical. Should fixed matrix be centered and scaled (Defaults to \code{FALSE}, 
#' recommended for biologic data).
#' @param scale.var Logical. Should fixed matrix be centered and scaled (Defaults to \code{TRUE}, 
#' recommended for environmental data to correct for differing units between variables).
#' @param popSize the population size.
#' @param parallel a logical argument specifying if parallel computing should be used (TRUE) or not (FALSE, default) 
#' for evaluating the fitness function.
#' @param maxiter the maximum number of iterations to run before the GA search is halted.
#' @param nBits passed to ga
#' @param ... Additional arguments passed to ga
#'
#' @return ga results
#' @export
#'
#' @examples
#' \donttest{
#' library(vegan)
#' library(GA)
#' data("varechem")
#' data("varespec")
#' 
#' fit <- mantelGA(
#'  fix.mat = varechem,
#'  var.mat = varespec,
#'  fix.dist.method="euclidean",
#'  var.dist.method="bray",
#'  scale.fix=TRUE,
#'  scale.var=FALSE,
#'  popSize = 50,
#'  parallel = FALSE,
#'  maxiter = 50,
#'  pmutation = 0.2,
#'  seed = 1
#' )
#' 
#' fit@solution
#' fit@fitnessValue # Best = 0.5091520
#' plot(fit)
#' paste(colnames(varespec)[as.logical(fit@solution)], collapse=",")
#' }
#' 
mantelGA <- function(
  fix.mat, 
  var.mat, 
  fix.dist.method = "bray",
  var.dist.method = "euclidean",
  scale.fix = FALSE,
  scale.var = TRUE,
  popSize = 50,
  parallel = FALSE,
  maxiter = 100,
  nBits = ncol(var.mat),
  ...
){

  if(scale.fix){fix.mat<-scale(fix.mat)}else{fix.mat<-fix.mat}
	if(scale.var){var.mat<-scale(var.mat)}else{var.mat<-var.mat}

  fix.dist <- vegan::vegdist(as.matrix(fix.mat), method=fix.dist.method)
  
  PARS <- rep(0, ncol(var.mat))

  fitnessFun <- function(PARS, maxnvar=maxnvar){
    vars.incl <- seq(ncol(var.mat))[as.logical(PARS)]
    if(length(vars.incl) != 0){
      var.dist <- suppressWarnings(vegan::vegdist(as.matrix(var.mat[,vars.incl]), method=var.dist.method))
      temp <- suppressWarnings(cor.test(fix.dist, var.dist, method="spearman"))
      score <- temp$estimate
    } else {
      score <- 0
    }
    return(score)
  }
  
  ga.fit <- GA::ga(
    type = "binary",
    fitness = fitnessFun,
    nBits = nBits,
    popSize = popSize,
    parallel = parallel,
    maxiter = maxiter,
    ...
  )

  ga.fit
}