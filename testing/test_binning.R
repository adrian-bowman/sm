#     Tests of the binning function

library(sm)
if (reinstall) devtools::install("sm")

n  <-1000

x   <- rnorm(n)
xb  <- binning(x, nbins = 6)
xb

x1  <- matrix(x, ncol = 1)
xb1 <- binning(x1, nbins = 6)
all.equal(xb, xb1)

x  <- cbind(rnorm(n), rnorm(n))
binning(x, nbins = 6)

x  <- cbind(rnorm(n), rnorm(n), rnorm(n))
binning(x, nbins = 6)
