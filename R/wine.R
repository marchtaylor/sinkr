#' @name wine 
#' @title wine attributes dataset
#' @description The \code{wine} data are the results of a chemical analysis of wines 
#' grown in the same region in Italy but derived from three different cultivars. 
#' The analysis determined the quantities of 13 constituents found in each of the 
#' three types of wine: Barolo, Grignolino, Barbera.
#' 
#' \itemize{
#'   \item 1) Type (Levels: Barolo, Grignolino, Barbera)
#'   \item 2) Alcohol
#'   \item 3) Malic acid
#'   \item 4) Ash
#'   \item 5) Alcalinity of ash
#'   \item 6) Magnesium
#'   \item 7) Total phenols
#'   \item 8) Flavanoids
#'   \item 9) Nonflavanoid phenols
#'   \item 10) Proanthocyanins
#'   \item 11) Color intensity
#'   \item 12) Hue
#'   \item 13) OD280/OD315 of diluted wines
#'   \item 14) Proline
#' }
#' 
#' @docType data
#' @format This data frame contains 178 rows, each corresponding to a different cultivar of 
#' wine produced in Piedmont (Italy), and 14 columns. The first column is the type of wine 
#' (Type), a factor variable with the following levels: Barolo, Grignolino, Barbera. 
#' The variables measured on the three types of wines are the following: 
#' Alcohol, Malic acid, Ash, Alcalinity, Magnesium, Phenols, Flavanoids, 
#' Nonflavanoids, Proanthocyanins, Color intensity, Hue, OD280/OD315 Dilution, Proline. 
#' All variables but the label class are continuous.A data.frame consisting of wine type 
#' and 13 variables their attributes 
#' @source Lichman, M. (2013). UCI Machine Learning Repository.
#'   \url{http://archive.ics.uci.edu/ml} Irvine, CA: University of California, 
#'   School of Information and Computer Science.
#' @references 
#' Forina M., Lanteri S. Armanino C., Casolino C., Casale M., Oliveri, P. (2008). 
#' V-PARVUS. An Extendible Pachage of programs for esplorative data analysis, 
#' classification and regression analysis. Dip. Chimica e Tecnologie Farmaceutiche ed 
#' Alimentari, Universita' di Genova.
#' @usage data(wine)
#' @keywords datasets multivariate
#' @examples
#' 
#' data(wine)
#' wine2 <- wine[,-1] 
#' head(wine2)
#' 
#' library(vegan)
#' pca <- rda(wine2, scale = TRUE)
#' COL <- 2:4
#' biplot(pca, col=c(rgb(0,0,0,0),rgb(0,0,0,1)), t="text")
#' abline(h=0, v=0, lty=3, col=8)
#' points(pca, col=COL[wine$Type])
#' for(i in seq(levels(wine$Type))){
#'   ordiellipse(pca, wine$Type, show.groups=levels(wine$Type)[i], conf=0.95, draw="lines",
#'    col=COL[i]) #[levels(wine$Type)])
#' }
#' 
#' 
NULL