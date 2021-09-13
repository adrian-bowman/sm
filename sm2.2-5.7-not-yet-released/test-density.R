# Test examples for the sm.density function

detach(package:sm)
unloadNamespace("sm")
install.packages("~/ownCloud/sm2.2-5.7-not-yet-released/sm", repos = NULL, type = "source")
library(sm)

n  <- 50
y1 <- rnorm(n)

# Weights are no longer required to be integers
sm.density(y1)
sm.density(y1, weights = rep(1, n))
sm.density(y1, weights = runif(n))

sm.density(y1, panel = TRUE)

sm.density(cbind(rnorm(50), rnorm(50), rnorm(50)))
