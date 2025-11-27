## ----setup, include=FALSE, echo=FALSE, results='hide'-------------------------
knitr::opts_chunk$set(
	fig.align = "center",
	fig.height = 4.5,
	fig.path = "tex/man-",
	fig.width = 6,
	message = FALSE,
	warning = FALSE,
	cache = FALSE,
	cache.path = "cache/",
	comment = NA
)

## -----------------------------------------------------------------------------
library(sinkr)
library(pals)
library(maps)
library(ggplot2)

## -----------------------------------------------------------------------------
# iris morphology
iris2 <- iris[,1:4]
iris2sc <- scale(iris2) # centered and scaled

# wine characteristics
data(wine)
wine2 <- wine[,names(wine)!="Type"]
wine2sc <- scale(wine2) # centered and scaled

# Synthetic data field
m <- 50
n <- 100
nsRatio <- 0.1 # the noise to signal ratio
x <- (seq(m)*2*pi)/m
t <- (seq(n)*2*pi)/n
Xt <- t(
  outer(sin(x), sin(t)) +
  outer(sin(2.1*x), sin(2.1*t)) +
  outer(sin(3.1*x), sin(3.1*t)) +
  outer(tanh(x), cos(t)) +
  outer(tanh(2*x), cos(2.1*t)) +
  outer(tanh(4*x), cos(0.1*t)) +
  outer(tanh(2.4*x), cos(1.1*t)) +
  tanh(outer(x, t, FUN="+")) +
  tanh(outer(x, 2*t, FUN="+")))

# noise standard deviation at 10% noise-to-signal ratio
noise_sd <- 0.1 * sd(as.vector(Xt))

# Add Gaussian noise
set.seed(123)  # For reproducibility
Xn <- Xt + rnorm(length(Xt), mean = 0, sd = noise_sd)
Xn <- array(Xn, dim = dim(Xt))

par(mfrow = c(1,3), mar = c(3,3,2,1), mgp = c(2,0.5,0))
image(t(as.matrix(iris2sc)), col = viridis(100))
mtext("iris2 (scaled)", line = 0.5)
image(t(as.matrix(wine2sc)), col = viridis(100))
mtext("wine2 (scaled)", line = 0.5)
image(x, t, t(Xn), col = viridis(100))
mtext("Synthetic", line = 0.5)

## -----------------------------------------------------------------------------
p_iris2sc <- prcomp(iris2sc)
p_wine2sc <- prcomp(wine2sc)
p_Xt <- prcomp(Xt)
p_Xn <- prcomp(Xn)

par(mfcol = c(1,3), mar = c(3,3,2,1), mgp = c(2,0.5,0))
plot(p_iris2sc$sdev^2, t = "b", log = "y", ylab = "Variances (log-scaled)")
mtext("iris2 (scaled)", line = 0.5)
plot(p_wine2sc$sdev^2, t = "b", log = "y", ylab = "Variances (log-scaled)")
mtext("wine2 (scaled)", line = 0.5)
plot(p_Xt$sdev[1:20]^2, t = "b", log = "y", ylab = "Variances (log-scaled)")
mtext("Synthetic", line = 0.5)
lines(p_Xn$sdev[1:20]^2, t = "b", col = 2, pch = 2)
legend("bottomleft", legend = c("Xt (True)", "Xn (w/ noise)"), pch = c(1,2), col = 1:2, bty = "n")



## -----------------------------------------------------------------------------
res_loocv <- pca_loocv(wine2sc)
res_loocv <- lapply(res_loocv, colSums)$pseudoinverse
loocv_nsig <- which(res_loocv==min(res_loocv))

set.seed(1)
res_kcv <- pca_kcv(wine2sc, ks = 5, verbose = FALSE)
res_kcv <- lapply(res_kcv, colSums)$pseudoinverse
kcv_nsig <- which(res_kcv==min(res_kcv))

par(mar = c(4,4,1,1))
plot(res_loocv, log = "y", t = "b", pch = 21, col = 3, bg = c(NA,3)[(seq(res_loocv)==loocv_nsig)+1])
lines(res_kcv, t = "b", pch = 21, col = 4, bg = c(NA,4)[(seq(res_kcv)==kcv_nsig)+1])


## -----------------------------------------------------------------------------
set.seed(2)
ks <- kfold(length(wine2sc), k = 20)
dineof_nsig <- lapply(ks, FUN = function(x){
  dineof(wine2sc, ref.pos = x, delta.rms = 1e-2, verbose = FALSE)$n.eof
})
unlist(dineof_nsig)
median(unlist(dineof_nsig))

## -----------------------------------------------------------------------------

df <- expand.grid(
  method = c("pca_kcv", "dineof"), 
  dataset = c("iris2", "iris2sc", "wine2sc", "Xt", "Xn"), 
  stringsAsFactors = F)
df$nsig <- NaN
for(i in seq(nrow(df))){
  # print(i)
  dat <- as.matrix(get(df$dataset[i]))
  meth <- df$method[i]
  
  if(meth == "pca_kcv"){
    set.seed(3)
    res <- pca_kcv(dat, ks = 5, verbose = FALSE)
    res <- lapply(res, colSums)$pseudoinverse
    res <- which(res == min(res))
    df$nsig[i] <- res
  }
  
  if(meth == "dineof"){
    set.seed(3)
    ks <- kfold(length(as.matrix(dat)), k = 20)
    res <- lapply(ks, FUN = function(x){
      dineof(dat, ref.pos = x, delta.rms = 1e-2, verbose = FALSE)$n.eof
    })
    res <- median(unlist(res))
    df$nsig[i] <- res
  }

}

df


## -----------------------------------------------------------------------------
data(sst)

# prcomp(sst$field) # this would fail due to empty columns

# remove empty columns
incl <- which(colSums(is.na(sst$field)) == 0)
dat <- sst$field[,incl]
P <- prcomp(dat)

# or use eof()
E <- eof(sst$field)

# results are nearly identical
par(mar = c(4,4,1,1))
plot(P$sdev[1:50]^2, E$Lambda[1:50], log = "xy")
abline(0,1)


## ----fig.height=6-------------------------------------------------------------
eof.num <- 2 # EOF number to plot
par(no.readonly=TRUE, mgp = c(2,0.5,0))
layout(matrix(c(1,3,2,3),nrow=2, ncol=2), widths=c(5,1), heights=c(3,3), respect = TRUE)
par(cex=1, mar=c(4,4,1,1))
PAL <- colorPalette(c("blue", "cyan", "grey90", "yellow", "red"), c(10,1,1,10))
ZLIM <- c(-1,1)*max(abs(E$u[,eof.num]))
COL <- val2col(E$u[,eof.num], col=PAL(100), zlim=ZLIM)
plot(lat ~ lon, data=sst$grid, pch=22, bg=COL, col=COL, cex=2)
mtext(paste("EOF", eof.num))
map("world", add=TRUE)
par(mar=c(4,0,1,4))
imageScale(E$u[,eof.num], col=PAL(100), zlim=ZLIM, axis.pos=4)
par(mar=c(4,4,1,4))
plot(sst$date, E$A[,eof.num], t="l", xlab = "", ylab = "")
lines(loess.smooth(sst$date, E$A[,eof.num], span=1/3), col=rgb(0.5,0.5,1), lwd=2) # smoothed signal
abline(h=0, v=seq(as.Date("1000-01-01"), as.Date("2100-01-01"), by="10 years"), col=8, lty=3)
mtext(paste("PC", eof.num))

## -----------------------------------------------------------------------------
datsc <- scale(dat, scale = FALSE)
set.seed(3)
ks <- kfold(length(as.matrix(dat)), k = 10)
D <- dineof(datsc, delta.rms = 1e-2, ref.pos = ks[[1]], verbose = F)

par(mar = c(4,4,2,1))
plot(E$Lambda[1:30], log = "y", ylab = "Variance")
mtext("Screeplot of sst EOF Lambda values,\nplus dineof-derived significance threshold")
abline(v = D$n.eof, lty = 2)


## -----------------------------------------------------------------------------

dat <- wine2sc #Xn

attr(dat, "scaled:center") <- NULL
attr(dat, "scaled:scale") <- NULL

set.seed(4)

# 20% gaps
frac_gaps <- 0.2
gaps <- sample(length(dat), length(dat)*frac_gaps)
dat_g <- dat
dat_g[gaps] <- NaN

image(t(dat_g), col = viridis(100))
mtext("wine2 (scaled) with 20% gaps", line = 0.5)

# 10% of non-gaps as reference
ref.pos <- sample(seq(dat_g)[-gaps], sum(!is.na(dat_g))*0.1)


# dineof to interpolate, then prcomp or other 
D <- dineof(dat_g, ref.pos = ref.pos, delta.rms = 1e-2, verbose = F, method = "svds")
P <- prcomp(D$Xa, center = F, scale. = F)

# direct rseof  
E <- eof(dat_g, recursive = TRUE, centered = F, scaled = F)

## ----gappy_recon--------------------------------------------------------------
res <- expand.grid(n = seq(E$Lambda), method = c("dineof+prcomp", "rseof"),
  data = c("gaps", "non-gaps", "all"), stringsAsFactors = FALSE)
res$rmse <- NaN

for(i in seq(E$Lambda)){
  R1 <- prcompRecon(P, pcs = seq(i))
  R2 <- eofRecon(E, pcs = seq(i), uncenter = F, unscale = F)
  
  # gaps
  idx <- which(res$n == i & res$method == "dineof+prcomp" & res$data == "gaps")
  res$rmse[idx] <- sqrt(mean((dat[gaps] - R1[gaps])^2))
  idx <- which(res$n == i & res$method == "rseof" & res$data == "gaps")
  res$rmse[idx] <- sqrt(mean((dat[gaps] - R2[gaps])^2))
  
  # non-gaps
  idx <- which(res$n == i & res$method == "dineof+prcomp" & res$data == "non-gaps")
  res$rmse[idx] <- sqrt(mean((dat[-gaps] - R1[-gaps])^2))
  idx <- which(res$n == i & res$method == "rseof" & res$data == "non-gaps")
  res$rmse[idx] <- sqrt(mean((dat[-gaps] - R2[-gaps])^2))
  
  # all
  idx <- which(res$n == i & res$method == "dineof+prcomp" & res$data == "all")
  res$rmse[idx] <- sqrt(mean((dat[] - R1[])^2))
  idx <- which(res$n == i & res$method == "rseof" & res$data == "all")
  res$rmse[idx] <- sqrt(mean((dat[] - R2[])^2))
  
}

ggplot(res) + aes(x = n, y = rmse, group = method, color = method) +
  facet_grid(~data) +
  geom_vline(xintercept = D$n.eof, lty = 2) +
  geom_line() #+ scale_y_log10(limits = c(0.1, NA)) 


