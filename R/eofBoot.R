#' Calculate number of non-mixed EOFs (eof version)
#' 
#' The \code{eofBoot} function uses a bootstrap randomization approach to 
#'   calculate distributions of Empirical Orthogonal Function analysis (EOF) 
#'   singular values with the \code{\link[sinkr]{eof}} function.
#'   EOF mode significance is assessed against the distributions of 
#'   neighboring EOF singular values ("Lambda") calculated by the permutated 
#'   models. A bootstrap routine follows the procedure of 
#'   Babamoradi et al. (2013) whereby permutations sample rows (samples) more 
#'   than once, which is a non-parametric approach does not make assumptions 
#'   about the distribution of data.
#'
#' @param F1 A data field. The data should be arraunged as samples in the column 
#'   dimension (typically each column is a time series for a spatial location).
#' @param centered Logical (\code{TRUE/FALSE}) to define if \code{F1} should be 
#'   centered prior to the analysis. Defaults to 'TRUE'
#' @param scaled Logical (\code{TRUE/FALSE}) to define if \code{F1} should be 
#'   scaled prior to the analysis. Defaults to 'TRUE'
#' @param nu Numeric value. Defines the number of EOFs to return. Defaults to 
#'   return the full set of EOFs.
#' @param method Method for matrix decomposition ('\code{svd}', '\code{eigen}', 
#' '\code{irlba}'). Defaults to 'svd' when \code{method = NULL}. Use of 'irlba' can 
#'   dramatically speed up computation time when \code{recursive = TRUE} but may
#'   produce errors in computing trailing EOFs. Therefore, this option is only 
#'   advisable when the field \code{F1} is large and when only a partial 
#'   decomposition is desired (i.e. \code{nu << dim(F1)[2]}). 
#'   All methods should give identical results when \code{recursive=TRUE}.
#' \code{svd} and \code{eigen} give similar results for non-gappy fields, 
#'   but will differ slightly with gappy fields due to decomposition of a 
#'   non-positive definite covariance matrix. Specifically, \code{eigen}  will 
#'   produce negative eigenvalues for trailing EOFs, while singular values 
#'   derived from \code{svd} be strictly positive.
#' @param recursive Logical. When \code{TRUE}, the function follows the method 
#'   of "Recursively Subtracted Empirical Orthogonal Functions" (RSEOF). 
#'   See \code{\link[sinkr]{eof}} for details.
#' @param nperm Numeric. The number of null model permutations to calculate.
#' @param verbose logical. Print progress (Default: verbose = TRUE).
#' 
#' @references
#' Babamoradi, H., van den Berg, F., Rinnan, A, 2013. Bootstrap based 
#' confidence limits in principal component analysis - A case study, 
#' Chemometrics and Intelligent Laboratory Systems, Volume 120,
#' pp. 97-105. doi:10.1016/j.chemolab.2012.10.007.
#'
#' @examples
#' data(Xt)
#' 
#' # The "noisy" field
#' # noise standard deviation at 10% noise-to-signal ratio
#' noise_sd <- 0.1 * sd(as.vector(Xt))
#' 
#' # Add Gaussian noise
#' set.seed(123)  # For reproducibility
#' Xn <- Xt + rnorm(length(Xt), mean = 0, sd = noise_sd)
#' Xn <- array(Xn, dim = dim(Xt))
#' 
#' res <- eofBoot(Xn, centered=FALSE, scaled=FALSE, nperm=499)
#' ylim <- range(res$Lambda.orig, res$Lambda)
#' boxplot(res$Lambda, log="y", col=8, border=2, outpch="", ylim=ylim)
#' points(res$Lambda.orig)
#' abline(v=res$n.sig+0.5, lty=2, col=4)
#' mtext(paste("Non-mixed PCs =", res$n.sig), side=3, line=0.5, col=4)
#' 
#' 
#' @importFrom stats quantile
#' @export
#' 
eofBoot <- function(
  F1, centered = TRUE, scaled = FALSE, nu = NULL, method = NULL,
  recursive = FALSE, nperm=99, verbose = TRUE
){
  E <- eof(F1=F1, centered = centered, scaled = scaled, nu = nu, method = method,
           recursive = recursive)
  
  Lambda <- matrix(NaN, nrow=nperm, ncol=length(E$Lambda))
  # For each permutation
  for(p in seq(nperm)){
    # Randomly subsample rows of scaled field
    # (with replacement; i.e. non-parametric bootstrapping) 
    samp.p <- sample(nrow(F1), nrow(F1), replace = TRUE)
    F1.tmp  <- F1[samp.p,]
    
    # Conduct EOF
    E.tmp <- eof(F1.tmp, centered = centered, scaled = scaled, nu = nu, method = method,
      recursive = recursive, verbose = FALSE)
    
    # Record Lambda
    Lambda[p,] <- E.tmp$Lambda
    
    if(verbose){
      cat(sprintf("permutation %d of %d is completed\r", p, nperm))
      flush.console()
    }
  }
  
  result <- list(Lambda=Lambda, Lambda.orig=E$Lambda)
  Qs <- apply(Lambda, 2, quantile, probs=c(0.025, 0.975))
  sig <- rep(1, length(E$Lambda)) # innocent (1) until proven guilty (0)
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
  RLE <- rle(sig)
  if(RLE$values[1]==1){
    result$n.sig <- RLE$length[1]
  } else {
    result$n.sig <- 0
  }
  result
  
}

