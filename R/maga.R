#' Genetic Algorithm Mantel test
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
#' ### envbio
#' fit <- maga(
#'  fix.mat = varechem,
#'  var.mat = varespec,
#'  fix.dist.method="euclidean",
#'  var.dist.method="bray",
#'  scale.fix = TRUE,
#'  scale.var = FALSE,
#'  popSize = 60,
#'  parallel = FALSE,
#'  maxiter = 200,
#'  run = 50,
#'  pmutation = 0.3,
#'  seed = 1
#' )
#' 
#' factorial(ncol(varespec)) # n combinations
#' 
#' res <- magaBest(fit, output.best = 10)
#' res$order.by.best
#' res$order.by.i.comb
#' plot(res)
#' 
#' 
#' 
#' 
#' ### Compare to bvstep
#' set.seed(1111)
#' res2 <- bvStep(
#'  fix.mat = varechem,
#'  var.mat = varespec,
#'  fix.dist.method="euclidean",
#'  var.dist.method="bray",
#'  scale.fix = TRUE,
#'  scale.var = FALSE,
#'  num.restarts = 50)
#' 
#' res2$order.by.best # mantelGA not looking in simpler solutions enough
#' 
#' # seed population with singular solutions
#' suggestedSol <- diag(ncol(varespec))
#' fit3 <- maga(
#'  fix.mat = varechem,
#'  var.mat = varespec,
#'  fix.dist.method="euclidean",
#'  var.dist.method="bray",
#'  scale.fix = TRUE,
#'  scale.var = FALSE,
#'  popSize = nrow(suggestedSol)*3,
#'  parallel = FALSE,
#'  maxiter = 200,
#'  run = 100,
#'  pcrossover = 0.05,
#'  pmutation = 0.1, #function(...) GA::ga_pmutation(..., p0=1, p=0.3),
#'  seed = 1, 
#'  suggestions = rbind(suggestedSol, suggestedSol)
#' )
#' plot(fit3)
#' 
#' res3 <- magaBest(fit3)
#' res3$order.by.best
#' res3$order.by.i.comb
#' 
#' plot(res3)
#' 
#' 
#' 
#' 
#' 
#' 
#' }
#' 
maga <- function(
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
  seed = NULL,
  ...
){

  if(scale.fix){fix.mat <- scale(fix.mat)}else{fix.mat <- fix.mat}
	if(scale.var){var.mat <- scale(var.mat)}else{var.mat <- var.mat}

  fix.dist <- vegan::vegdist(as.matrix(fix.mat), method=fix.dist.method)
  
  PARS <- rep(0, ncol(var.mat))
  
  # rm(mantelEnv)
  # gaMonitorObj <<- list()
  
  fitnessFun <- function(PARS, var.mat, fix.dist, var.dist.method){
    if(sum(PARS)==0) PARS[sample(length(PARS), 1)] <- 1
    vars.incl <- seq(ncol(var.mat))[which(as.logical(PARS))]
    if(length(vars.incl) != 0){
      var.dist <- suppressWarnings(
        vegan::vegdist(as.matrix(var.mat[,vars.incl]), 
          method = var.dist.method))
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
    var.mat = var.mat, 
    fix.dist = fix.dist, 
    var.dist.method = var.dist.method,
    nBits = nBits,
    popSize = popSize,
    parallel = parallel,
    maxiter = maxiter,
    seed = seed, 
    monitor = magaMonitor, 
    ...
  )
  
  # load(file = tmpfname)
  pop <- get("gaMonitorObj", envir = globalenv())
  # pop <- mantelEnv$gaMonitorObj
  pop <- do.call("rbind", pop)
  names(pop)[seq(PARS)] <- colnames(var.mat)
  # head(pop)
  
  rm("gaMonitorObj", envir = globalenv())
  attr(ga.fit, "pop") <- pop

  return(ga.fit)
}




#' Output Genetic Algorithm population fitness evolution 
#' 
#' @description Called internally by `GA::ga`, and used within 
#'   `maga`.
#' 
#' @param object an object of class ga-class or gaisl-class, usually resulting 
#'   from a call to function ga or gaisl, respectively.
#' @param digits minimal number of significant digits.
#' @param ... further arguments passed to or from other methods.
#'
#' @return
#' @export
#'
magaMonitor <- function(object, digits = getOption("digits"), ...){
  
  if(object@iter == 1){
    assign("gaMonitorObj", value = list(), envir = globalenv())
  }
  
  pop <- as.data.frame(object@population)
  names(pop) <- paste0("par", seq(ncol(pop)))
  pop$fitness <- object@fitness
  pop$iter <- object@iter

  POP <- get("gaMonitorObj", envir = globalenv())
  POP[[object@iter]] <- pop
  assign("gaMonitorObj", value = POP, envir = globalenv())
  
  fitness <- na.exclude(object@fitness)
  sumryStat <- c(mean(fitness), max(fitness))
  sumryStat <- format(sumryStat, digits = digits)
  cat(paste("GA | iter =", object@iter, "| Mean =",
      sumryStat[1], "| Best =", sumryStat[2]))
  cat("\n")
  flush.console()
}





#' 
#' 
#' Extract top models from an mantelGA object
#'
#' @param fit output from maga function
#' @param output.best Number of best combinations to return in the results 
#'   object (Default `output.best = 10`).
#'
#' @return list containing `order.by.best`, `order.by.i.comb`, 
#'   `best.model.vars`, `best.model.rho`
#'   
#' @export
#'
#' @examples
#' 
#' 
magaBest <- function(fit, output.best = 10){
  pop <- attr(fit, "pop")
  vars <- colnames(pop)[-which(names(pop) %in% c("fitness", "iter"))]
  pop$n.var <- rowSums(pop[,-which(names(pop) %in% c("fitness", "iter"))])
  names(pop)[which(names(pop)=="fitness")] <- "rho"

  # Order by combination
  pos <- c(by(data = pop, INDICES = pop$n.var, FUN = function(x){
      wm <- which.max(x$rho)
      rownames(x)[wm]}))
  qOBC <- pop[as.numeric(pos),]
  OBC <- data.frame(
    var.incl = apply(qOBC[,vars], 1, FUN = function(x){
        paste(which(x==1), collapse = ",")}),
    n.var = qOBC$n.var, 
    rho = qOBC$rho)
  OBC <- OBC[seq(which.max(OBC$rho)),]
  

  # Order by best
  pop2 <- unique(pop[,-which(colnames(pop)=="iter")])
  pop2 <- pop2[order(pop2$rho, decreasing = TRUE),]
  incl <- seq(output.best)
  OBB <- data.frame(
    var.incl = apply(pop2[incl,vars], 1, FUN = function(x){
        paste(which(x==1), collapse = ",")}),
    n.var = pop2$n.var[incl], 
    rho = pop2$rho[incl], 
    stringsAsFactors = FALSE)
  
	out <- list(
		order.by.best = OBB,
		order.by.i.comb = OBC,
		best.model.vars = paste(vars[
		  as.numeric(unlist(strsplit(OBB$var.incl[1], ",")))], collapse=",") ,
		best.model.rho=OBB$rho[1]
	)
	attr(out, "pop") <- pop
	class(out) <-"magaBest"
	
	return(out)
}



#' plot results from magaBest
#'
#' @param res results of call to `magaBest`
#'
#' @return a plot
#' @export
#'
#' @examples
#' 
#' 
#' 
#' 
plot.magaBest <- function(res){
  
  pop <- attr(res, "pop")
  plot(rho ~ n.var, pop, col = adjustcolor(1, 0.1), 
  pch = ".", cex = 5)
  lines(rho ~ n.var, res$order.by.i.comb, 
    t = "o", col = 2, pch = 20)
}

