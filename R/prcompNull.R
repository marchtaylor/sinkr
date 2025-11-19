#' Calculate significance of EOFs compared to a null model (prcomp version)
#' 
#' The \code{prcompNull} function uses a randomization approach to 
#' calculate a null model for use in Empirical Orthogonal Function analysis (EOF) 
#' with the \code{\link[stats]{prcomp}} function. EOF mode significance is assessed against the 
#' distribution of EOF singular values ("Lambda") calculated by the null models
#'
#' @param x,retx,center,scale.,tol See \code{\link[stats]{prcomp}} 
#' for argument definitions.
#' @param nperm Numeric. The number of null model permutations to calculate.
#' 
#' @examples
#' # Generate data
#' m=50
#' n=100
#' frac.gaps <- 0.5 # the fraction of data with NaNs
#' N.S.ratio <- 0.1 # the Noise to Signal ratio for adding noise to data
#' x <- (seq(m)*2*pi)/m
#' t <- (seq(n)*2*pi)/n
#' 
#' # True field
#' Xt <- 
#'   outer(sin(x), sin(t)) + 
#'   outer(sin(2.1*x), sin(2.1*t)) + 
#'   outer(sin(3.1*x), sin(3.1*t)) +
#'   outer(tanh(x), cos(t)) + 
#'   outer(tanh(2*x), cos(2.1*t)) + 
#'   outer(tanh(4*x), cos(0.1*t)) + 
#'   outer(tanh(2.4*x), cos(1.1*t)) + 
#'   tanh(outer(x, t, FUN="+")) + 
#'   tanh(outer(x, 2*t, FUN="+"))
#' 
#' Xt <- t(Xt)
#' 
#' # Noise field
#' set.seed(1)
#' RAND <- matrix(runif(length(Xt), min=-1, max=1), nrow=nrow(Xt), ncol=ncol(Xt))
#' R <- RAND * N.S.ratio * Xt
#'
#' # True field + Noise field
#' Xp <- Xt + R
#' 
#' res <- prcompNull(Xp, center=FALSE, scale=FALSE, nperm=499)
#' ylim <- range(res$Lambda.orig, res$Lambda)
#' boxplot(res$Lambda, log="y", col=8, border=2, outpch="", ylim=ylim)
#' points(res$Lambda.orig)
#' abline(v=res$n.sig+0.5, lty=2, col=4)
#' mtext(paste("Significant PCs =", res$n.sig), side=3, line=0.5, col=4)
#' 
#' @importFrom stats prcomp quantile
#' 
#' @export
#' 
prcompNull <- function(x, retx = TRUE, center = TRUE, scale. = FALSE,
                          tol = NULL, nperm=99
){
  x <- as.matrix(x)
  
  E <- prcomp(x, retx = retx, center = center, scale. = scale.,
              tol = tol)
  
  Lambda <- matrix(NaN, nrow=nperm, ncol=length(E$sdev))
  #For each permutation
  for(p in seq(nperm)){
    # Randomly shuffle values in each column of scaled field
    tmp <- vector("list", ncol(x))
    NROW <- nrow(x)
    tmp <- lapply(tmp, FUN = function(x){sample(NROW)})
    tmp <- do.call("cbind", tmp)
    tmp <- ((col(tmp)-1) * nrow(tmp)) + tmp
    x.tmp <- as.matrix(array(x[c(tmp)], dim = dim(x)))
    
    # Conduct EOF
    E.tmp <- prcomp(x.tmp, retx = retx, center = center, scale. = scale.,
                    tol = tol)
    #record Lambda
    Lambda[p,] <- E.tmp$sdev^2

    cat(sprintf("permutation %d of %d is completed\r", p, nperm))
    flush.console()       
    
  }
  
  result <- list(Lambda=Lambda, Lambda.orig=E$sdev^2)
  aboveNull <- as.numeric(E$sdev^2 > apply(Lambda, 2, quantile, probs=0.95))
  RLE <- rle(aboveNull)
  if(RLE$values[1]==1){
    result$n.sig <- RLE$length[1]
  } else {
    result$n.sig <- 0
  }
  result
}
