#' Calculate significance of EOFs compared to a null model (eof version)
#' 
#' The \code{eofNull} function uses a randomization approach to 
#' calculate a null model for use in Empirical Orthogonal Function analysis (EOF) 
#' with the \code{eof} function. EOF mode significance is assessed against the 
#' distribution of EOF singular values ("Lambda") calculated by the null models
#' 
#' @param F1 A data field. The data should be arraunged as samples in the column 
#' dimension (typically each column is a time series for a spatial location).
#' @param centered Logical (\code{TRUE/FALSE}) to define if \code{F1} should be 
#' centered prior to the analysis. Defaults to 'TRUE'
#' @param scaled Logical (\code{TRUE/FALSE}) to define if \code{F1} should be 
#' scaled prior to the analysis. Defaults to 'TRUE'
#' @param nu Numeric value. Defines the number of EOFs to return. Defaults to 
#' return the full set of EOFs.
#' @param method Method for matrix decomposition ('\code{svd}', '\code{eigen}', 
#' '\code{irlba}'). Defaults to 'svd' when \code{method = NULL}. Use of 'irlba' can 
#' dramatically speed up computation time when \code{recursive = TRUE} but may
#' produce errors in computing trailing EOFs. Therefore, this option is only advisable when
#' the field \code{F1} is large and when only a partial decomposition is desired
#' (i.e. \code{nu << dim(F1)[2]}). All methods should give identical 
#' results when \code{recursive=TRUE}.
#' \code{svd} and \code{eigen} give similar results for non-gappy fields, 
#' but will differ slightly with gappy fields due to decomposition of a 
#' nonpositive definite covariance matrix. Specifically, \code{eigen}  will produce 
#' negative eigenvalues for trailing EOFs, while singular values derived from \code{svd} 
#' will be strictly positive.
#' @param recursive Logical. When \code{TRUE}, the function follows the method of
#' "Recursively Subtracted Empirical Orthogonal Functions" (RSEOF). See \code{\link[sinkr]{eof}}
#' for details
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
#' res <- eofNull(Xp, method="svd", centered=FALSE, scaled=FALSE, nperm=499)
#' ylim <- range(res$Lambda.orig, res$Lambda)
#' boxplot(res$Lambda, log="y", col=8, border=2, outpch="", ylim=ylim)
#' points(res$Lambda.orig)
#' abline(v=res$n.sig+0.5, lty=2, col=4)
#' mtext(paste("Significant PCs =", res$n.sig), side=3, line=0.5, col=4)
#' 
#' @export
#' 
eofNull <- function(
  F1, centered = TRUE, scaled = FALSE, nu = NULL, method = NULL,
  recursive = FALSE, nperm=99
){
  E <- eof(F1=F1, centered = centered, scaled = scaled, nu = nu, method = method,
           recursive = recursive)
  
  Lambda <- matrix(NaN, nrow=nperm, ncol=length(E$Lambda))
  #For each permutation
  for(p in seq(nperm)){
    # Randomly reorganize dimensions of scaled field
    F1.tmp <- F1
    for(i in seq(ncol(F1.tmp))){
      F1.tmp[,i] <- F1.tmp[,i][sample(nrow(F1.tmp))]
    }
    # Conduct EOF
    E.tmp <- eof(F1.tmp, centered = centered, scaled = scaled, 
                 nu = nu, method = method, recursive = recursive)
    #record Lambda
    Lambda[p,] <- E.tmp$Lambda
    print(paste("permutation", p, "of", nperm, "is completed"))      
  }
  
  result <- list(Lambda=Lambda, Lambda.orig=E$Lambda)
  result$n.sig <- max(which(E$Lambda > apply(Lambda, 2, quantile, probs=0.95)))
  result
  
}
