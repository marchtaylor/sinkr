# Synthetic data field ---------------

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

save(Xt, file = "data/Xt.rda")

# noise standard deviation at 10% noise-to-signal ratio
noise_sd <- 0.1 * sd(as.vector(Xt))

# Add Gaussian noise
set.seed(123)  # For reproducibility
Xn <- Xt + rnorm(length(Xt), mean = 0, sd = noise_sd)
Xn <- array(Xn, dim = dim(Xt))
