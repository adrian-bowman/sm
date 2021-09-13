#     Test code for sm.ancova

library(sm)
if (reinstall) devtools::install("sm")

# Two covariates
X <- cbind(rnorm(100), rnorm(100))
y <- rnorm(100)
g <- rbinom(100, 1, 0.5)
m <- sm.ancova(X, y, g, model = "equal")
