#' Geometric Mean
#' 
#' Calculates the geometric mean of a vector
#' 
#' @param x numeric vector of positive numbers.
#' @param na.rm Remove NAs befor calculation (as in \code{\link[base]{mean}})
#' @param zero.propagate Logical. Should zeros be considered (resulting in output of zero)
#' 
#' @references
#' From \href{http://stackoverflow.com/a/25555105/1199289}{stackoverflow answer}
#' posted by \href{https://stackoverflow.com/users/935950/paul-mcmurdie}{Paul McMurdie}
#' 
#' @examples
#' ### simple usage
#' gm_mean(c(1:4))
#' gm_mean(c(-1:4)) # negative values not allowed
#' gm_mean(c(0:4)) # zeros do not propagate
#' gm_mean(c(0:4), zero.propagate=TRUE) #zeros allowed to propagate
#' gm_mean(c(1,2,3,4, NaN)) # na.rm=TRUE
#' gm_mean(c(1,2,3,4, NaN), na.rm=FALSE) # na.rm=FALSE
#' 
#' ### example of proportional change
#' df <- data.frame(index1 = 5, index2 = 25) # two indices of differing magnitude
#' mult <- c(1.25, 1.5) # multiplier
#' df <- rbind(df, df*mult) # indices change by differing proportions
#' df # view dataframe
#' gm_mean(mult) # mean proportional increase
#' gm_mean(df[2,]) / gm_mean(df[1,]) # equal
#' gm_mean(df[2,] / df[1,]) # equal
#' 
#' @export
#' 
gm_mean = function(x, na.rm=TRUE, zero.propagate = FALSE){
  if(any(x < 0, na.rm = TRUE)){
    return(NaN)
  }
  if(zero.propagate){
    if(any(x == 0, na.rm = TRUE)){
      return(0)
    }
    exp(mean(log(x), na.rm = na.rm))
  } else {
    exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
  }
}