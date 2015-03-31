#' North's Rule of Thumb for EOF significance
#' 
#' The \code{northTest} function assesses the uniqueness of EOF modes through
#' assumptions of error on singular values ("Lambda") as described by 
#' North et al (1982). Overlapping error limits between 
#' neighboring Lambda values indicates a possible mixure of signals. A similar test 
#' via bootstrapping method can be done with the \code{\link[sinkr]{prcompBoot}} 
#' or \code{\link[sinkr]{eofBoot}} functions.
#' 
#' @param x Matrix. Field used during EOF decomposition
#' @param Lambda Vector of singular values representing variance magnitudes 
#' explained by each EOF mode. Results of \code{\link[stats]{prcomp}} will 
#' need to be squared from their standard deviation values 
#' (e.g. \code{Lambda=res$sdev^2}).  
#' 
#' @references
#' G.R. North, T.L. Bell, R.F. Cahalan, and F.J. Moeng. (1982). 
#' Sampling errors in the estimation of empirical orthogonal functions. 
#' Mon. Wea. Rev., 110:699-706.
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
#' # eof + northTest 
#' E <- eof(Xp, centered=FALSE, scaled=FALSE)
#' L <- E$Lambda # Lambdas
#' res <- northTest(Xp, L)
#' plot(L, ylim=range(c(L+res$upper.lim, L-res$lower.lim)), log="y")
#' segments(seq(L), L-res$lower.lim, seq(L), L+res$upper.lim, col=2)
#' abline(v=res$n.sig+0.5, lty=2, col=4)
#' mtext(paste("Non-mixed PCs =", res$n.sig), side=3, line=0.5, col=4)
#'
#' # prcomp + northTest 
#' E <- prcomp(Xp, center=FALSE, scale=FALSE)
#' L <- E$sdev^2 # Lambdas
#' res <- northTest(Xp, L)
#' plot(L, ylim=range(c(L+res$upper.lim, L-res$lower.lim)), log="y")
#' segments(seq(L), L-res$lower.lim, seq(L), L+res$upper.lim, col=2)
#' abline(v=res$n.sig+0.5, lty=2, col=4)
#' mtext(paste("Non-mixed PCs =", res$n.sig), side=3, line=0.5, col=4)
#'
#' 
#'   
#' @export
#' 
northTest <- function(x, Lambda){
  Lambda.err <- sqrt(2 / dim(x)[2]) * Lambda
  upper.lim <- Lambda + Lambda.err
  lower.lim <- Lambda - Lambda.err
  sig <- rep(1, length(Lambda)) # innocent (1) until proven guilty (0)
  for(i in seq(sig)){
    # if upper limit is bigger than preceeding lower limit, then not significant
    if((i-1) %in% seq(sig)){
      if(upper.lim[i] > lower.lim[i-1]) sig[i] <- 0 
    }
    # if lower limit is smaller than following upper limit, then not significant
    if((i+1) %in% seq(sig)){
      if(lower.lim[i] < upper.lim[i+1]) sig[i] <- 0 
    }
  }
  result <- list(
    Lambda=Lambda, Lambda.err=Lambda.err, 
    upper.lim=upper.lim, lower.lim=lower.lim,
    sig=sig, n.sig=min(which(sig == 0)) - 1
  )
  result
}
