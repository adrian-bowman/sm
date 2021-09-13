#     Test code for smooth pca

Y    <- log(as.matrix(aircraft[ , -(1:2)]))
year <- aircraft$Yr

spca  <- sm.pca(year, Y, df = 4, display = "none")

spca <- sm.pca(spca, display = "eigenvalues")

sm.pca(spca, display = "eigenvectors")
spca <- sm.pca(spca, display = "eigenvalues")
spca <- sm.pca(spca, display = "eigenvectors")

with(spca, {
   ylim <- range(evals[ , 1], band)
   plot(xgrid, evals[ , 1], type = "n", ylab = "Variance", ylim = ylim)
   polygon(c(xgrid, rev(xgrid)), c(band[ , 1], rev(band[ , 2])),
           col = "lightgreen", border = NA)
   lines(xgrid, evals[ , 1], col = "blue")
})

with(spca, {
   pc <- 1
   plot(range(xgrid.plot), range(evecs.plot), type = "n",
        xlab = "x", ylab = "PC loadings")
   for (i in 1:ncol(Y))
      segments(xgrid.plot[-length(xgrid.plot)],
               evecs.plot[-nrow(evecs.plot), i],
               xgrid.plot[-1], evecs.plot[-1, i],
               col = col.plot[ , i], lty = i)
})

par(mfrow = c(2, 1))
Y <- cbind(rnorm(100), rnorm(100), rnorm(100))
x <- runif(100)
spca <- sm.pca(x, Y, df = 4)
par(mfrow = c(1, 1))
