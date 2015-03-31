#' @title Covariance matrix calculation for gappy data
#' @description This function calculates a covoriance matrix for data that contain 
#' missing values ('gappy data'). 
#' @param F1 A data field. 
#' @param F2 An optional 2nd data field.
#' @return A matrix with covariances between columns of \code{F1}. 
#' If both \code{F1} and \code{F2} are provided, then the covariances 
#' between columns of \code{F1} and the columns of \code{F2} are returned.
#' @details This function gives comparable results to \code{cov(F1, y=F2, use="pairwise.complete.obs")}
#' whereby each covariance value is divided by n number of shared values (as opposed 
#' to n-1 in the case of \code{cov()}. Futhermore, the function will return a 0 (zero) in 
#' cases where no shared values exist between columns; the advantage being that a 
#' covariance matrix will still be calculated in cases of very gappy data, or when 
#' spatial locations have accidentally been included without observations (i.e. land 
#' in fields of aquatic-related parameters).
#' 
#' @keywords covariance EOF PCA gappy
#' @export
#' @examples
#' # Create synthetic data 
#' set.seed(1)
#' mat <- matrix(rnorm(500, sd=10), nrow=50, ncol=10)
#' matg <- mat
#' matg[sample(length(mat), 0.5*length(mat))] <- NaN # Makes 50% missing values
#' matg # gappy matrix
#' 
#' # Calculate covariance matrix and compare to 'cov' function output
#' c1 <- cov4gappy(matg)
#' c2 <- cov(matg, use="pairwise.complete.obs")
#' plot(c1,c2, main="covariance comparison", xlab="cov4gappy", ylab="cov")
#' abline(0,1,col=8)
#' 
cov4gappy <- function(F1, F2=NULL){
    if(is.null(F2)){
        F1 <- as.matrix(F1)
        F1_val<-replace(F1, which(!is.na(F1)), 1)
        F1_val<-replace(F1_val, which(is.na(F1_val)), 0) 
        n_pairs=(t(F1_val) %*% F1_val)

        F1<-replace(F1, which(is.na(F1)), 0)
        cov_mat <- (t(F1) %*% F1) / n_pairs
        cov_mat <- replace(cov_mat, which(is.na(cov_mat)), 0)
    }

    if(!is.null(F2)){
        if(dim(F1)[1] == dim(F2)[1]){
            F1 <- as.matrix(F1)
            F2 <- as.matrix(F2)

            F1_val <- replace(F1, which(!is.na(F1)), 1)
            F1_val <- replace(F1_val, which(is.na(F1_val)), 0) 
            F2_val <- replace(F2, which(!is.na(F2)), 1)
            F2_val <- replace(F2_val, which(is.na(F2_val)), 0) 
            n_pairs <- (t(F1_val) %*% F2_val)

            F1<-replace(F1, which(is.na(F1)), 0)
            F2<-replace(F2, which(is.na(F2)), 0)
            cov_mat <- (t(F1) %*% F2) / n_pairs
            cov_mat <- replace(cov_mat, which(is.na(cov_mat)), 0)
	
        } else {
            print("ERROR; matrices columns not of the same lengths")
        }
    }

    cov_mat
}
