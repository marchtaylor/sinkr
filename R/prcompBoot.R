#' Calculate number of non-mixed EOFs (prcomp version)
#' 
#' The \code{prcompBoot} function uses a bootstrap randomization approach to 
#' calculate distributions of Empirical Orthogonal Function analysis (EOF) 
#' singular values with the \code{\link[stats]{prcomp}} function.
#' EOF mode significance is assessed against the distributions of 
#' neighboring EOF singular values ("Lambda") calculated by the permutated 
#' models. A bootstrap routine follows the procedure of Babamoradi et al. (2013) whereby 
#' permutations sample rows (samples) more than once, which is a non-parametric 
#' approach does not make assumptions about the distribution of data.
#'
#' @param x,retx,center,scale.,tol See \code{\link[stats]{prcomp}} 
#' for argument definitions.
#' @param nperm Numeric. The number of null model permutations to calculate.
#'
#' @references
#' Babamoradi, H., van den Berg, F., Rinnan, A, 2013. Bootstrap based 
#' confidence limits in principal component analysis - A case study, 
#' Chemometrics and Intelligent Laboratory Systems, Volume 120,
#' pp. 97-105. doi:10.1016/j.chemolab.2012.10.007.
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
#' res <- prcompBoot(Xp, center=FALSE, scale=FALSE, nperm=499)
#' ylim <- range(res$Lambda.orig, res$Lambda)
#' boxplot(res$Lambda, log="y", col=8, border=2, outpch="", ylim=ylim)
#' points(res$Lambda.orig)
#' abline(v=res$n.sig+0.5, lty=2, col=4)
#' mtext(paste("Non-mixed PCs =", res$n.sig), side=3, line=0.5, col=4)
#' 
#' @export
#' 
prcompBoot <- function(
  x, retx = TRUE, center = TRUE, scale. = FALSE,
  tol = NULL, nperm=99
){
  E <- prcomp(x, retx = retx, center = center, scale. = scale.,
              tol = tol)
  Lambda <- matrix(NaN, nrow=nperm, ncol=length(E$sdev))
  #For each permutation
  for(p in seq(nperm)){
    # Randomly reorganize dimensions of scaled field
    samp.p <- NaN*seq(nrow(x))
    for(i in seq(nrow(x))){
      samp.p[i] <- sample(nrow(x), 1)
    }
    x.tmp  <- x[samp.p,]
    # Conduct EOF
    E.tmp <- prcomp(x.tmp, retx = retx, center = center, scale. = scale.,
                    tol = tol)
    # Record Lambda
    Lambda[p,] <- E.tmp$sdev^2
    print(paste("permutation", p, "of", nperm, "is completed")) 
  }
  
  result <- list(Lambda=Lambda, Lambda.orig=E$sdev^2)
  Qs <- apply(Lambda, 2, quantile, probs=c(0.025, 0.975))
  sig <- rep(1, length(E$sdev^2)) # innocent (1) until proven guilty (0)
  for(i in seq(sig)){
    # if upper quantile is bigger than preceeding lower quantile, then not significant
    if((i-1) %in% seq(sig)){
      if(Qs[2,i] > Qs[1,(i-1)]) sig[i] <- 0 
    }
    # if lower quantile is smaller than following upper quantile, then not significant
    if((i+1) %in% seq(sig)){
      if(Qs[1,i] < Qs[2,(i+1)]) sig[i] <- 0 
    }
  }
  result$sig <- sig
  result$n.sig <- min(which(sig == 0)) - 1
  result
  
}
