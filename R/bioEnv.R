#' Clarke and Ainsworth's BIO-ENV routine
#' 
#' The \code{bioEnv} function performs Clarke and Ainsworth's (1993) "BIO-ENV" routine which compares (via a Mantel test) 
#' a fixed matrix of similarities to a variable one that test all possible variable combinations.
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
#' @param output.best Number of best combinations to return in the results object (Default=10).
#' @param var.max Maximum number of variables to include. Defaults to all, \code{var.max=ncol(var.mat)}.
#' 
#' @details The R package "vegan" contains a version of Clarke and Ainsworth's (1993) 
#' BIOENV analysis (\code{\link[vegan]{bioenv}}) which allows for the comparison of distance/similarity matrices between 
#' two sets of data having either samples or variables in common. The difference with \code{bioEnv} is that one has more
#' flexibility with methods to apply to the fixed and variable multivariate matrices.
#' The typical setup is in the exploration of environmental variables 
#' that best correlate to sample similarities of the biological community  (e.g. species biomass or abundance), 
#' called "BIOENV".
#' In this case, the similarity matrix of the community is fixed, while subsets of 
#' the environmental variables are used in the calculation of the environmental similarity matrix. 
#' A correlation coefficient (typically Spearman rank correlation coefficient, "rho") is then 
#' calculated between the two matrices and the best subset of environmental variables 
#' can then be identified and further subjected to a permutation test to determine significance.
#' The vegan package's \code{\link[vegan]{bioenv}} function assumes BIOENV setup, and the similarity matrix of environmental data 
#' is assumed to be based on normalized "euclidean" distances. 
#' This makes sense with environmental data where one normalizes the data to remove the effect 
#' of differing units between parameters, yet in cases where the variable matrix is biological, 
#' one might want more flexibility (a Bray-Curtis measure of similarity is common given its 
#' non-parametric nature). 
#' For example, beyond the typical biological to environmental comparison (BIOENV setup), 
#' one can also use the routine to explore other other types of relationships; e.g.:
#' \tabular{rll}{
#' \tab ENVBIO: \tab subset of biological variables that best correlate to the overall environmental pattern \cr
#' \tab BIOBIO: \tab subset of biological variables that best correlate to the overall biological pattern \cr
#' \tab ENVENV: \tab subset of environmental variables that best correlate to the overall environmental pattern \cr
#' } 
#' It is important to mention that one of the reasons why a variable biological similarity 
#' matrix is often less explored with the routine is that the number of possible subset combinations 
#' becomes computationally overwhelming when the number of species/groups is large - the total 
#' number of combinations being equal to 2^n - 1, where n is the total number of variables. 
#' For this reason, Clarke and Warwick (1998) presented a stepwise routine (BVSTEP) (see \code{\link[sinkr]{bvStep}} 
#' for more efficient exploration of the subset combinations). 
#' 
#' @references
#' Clarke, K. R & Ainsworth, M. 1993. A method of linking multivariate community structure to environmental 
#' variables. Marine Ecology Progress Series, 92, 205-219.
#' 
#' Clarke, K. R., Warwick, R. M., 2001. Changes in Marine Communities: 
#' An Approach to Statistical Analysis and Interpretation, 2nd edition. PRIMER-E Ltd, Plymouth, UK. 
#' 
#' @examples
#' \donttest{
#' library(vegan)
#' data(varespec)
#' data(varechem)
#' 
#' res <- bioEnv(wisconsin(varespec), varechem, 
#'               fix.dist.method="bray", var.dist.method="euclidean",
#'               scale.fix=FALSE, scale.var=TRUE
#' )
#' res
#' }
#' 
#' @keywords Mantel_test Primer
#' 
#' @importFrom utils combn
#' @importFrom stats cor.test
#' 
#' @export
#' 
bioEnv <- function(fix.mat, var.mat, 
fix.dist.method="bray", var.dist.method="euclidean",
scale.fix=FALSE, scale.var=TRUE,
output.best=10,
var.max=ncol(var.mat)
){
	if(dim(fix.mat)[1] != dim(var.mat)[1]){stop("fixed and variable matrices must have the same number of rows")}
	if(var.max > dim(var.mat)[2]){stop("var.max cannot be larger than the number of variables (columns) in var.mat")}

	#require(vegan)

	combn.sum <- sum(factorial(ncol(var.mat))/(factorial(1:var.max)*factorial(ncol(var.mat)-1:var.max)))
	
	if(scale.fix){fix.mat<-scale(fix.mat)}else{fix.mat<-fix.mat}
	if(scale.var){var.mat<-scale(var.mat)}else{var.mat<-var.mat}
	fix.dist <- vegan::vegdist(fix.mat, method=fix.dist.method)
	RES_TOT <- c()
	best.i.comb <- c()
	iter <- 0
	for(i in 1:var.max){
		var.comb <- combn(1:ncol(var.mat), i, simplify=FALSE)
		RES <- data.frame(var.incl=rep(NA, length(var.comb)), n.var=i, rho=0)
		for(f in 1:length(var.comb)){
			iter <- iter+1
			var.dist <- vegan::vegdist(as.matrix(var.mat[,var.comb[[f]]]), method=var.dist.method)
			temp <- suppressWarnings(cor.test(fix.dist, var.dist, method="spearman"))
			RES$var.incl[f] <- paste(var.comb[[f]], collapse=",")
			RES$rho[f] <- temp$estimate
			if(iter %% 100 == 0){print(paste(round(iter/combn.sum*100, 3), "% finished"))}
		}

		order.rho <- order(RES$rho, decreasing=TRUE)
		best.i.comb <- c(best.i.comb, RES$var.incl[order.rho[1]])
		if(length(order.rho) > output.best){
			RES_TOT <- rbind(RES_TOT, RES[order.rho[1:output.best],])
		} else {
			RES_TOT <- rbind(RES_TOT, RES)
		}
	}
	rownames(RES_TOT)<-NULL
		
	if(dim(RES_TOT)[1] > output.best){
		order.by.best <- order(RES_TOT$rho, decreasing=TRUE)[1:output.best]
	} else {
		order.by.best <- order(RES_TOT$rho, decreasing=TRUE)
	}
	OBB <- RES_TOT[order.by.best,]
	rownames(OBB) <- NULL

	order.by.i.comb <- match(best.i.comb, RES_TOT$var.incl)
	OBC <- RES_TOT[order.by.i.comb,]
	rownames(OBC) <- NULL

	out <- list(
		order.by.best=OBB,
		order.by.i.comb=OBC,
		best.model.vars=paste(colnames(var.mat)[as.numeric(unlist(strsplit(OBB$var.incl[1], ",")))], collapse=",") ,
		best.model.rho=OBB$rho[1]
	)
	out
}

	

