# Test examples for the sm.density function

library(sm)
if (reinstall) devtools::install("sm")

n  <- 50
y1 <- rnorm(n)

test_label("Weights are no longer required to be integers", test.prompt)
sm.density(y1)
sm.density(y1, weights = rep(1, n))
sm.density(y1, weights = runif(n))

sm.density(y1, panel = TRUE)

sm.density(cbind(rnorm(50), rnorm(50), rnorm(50)))

with(aircraft,
   sm.density(cbind(log(Span), log(Speed)), display = "slice"))

# Positive data
y <- rnorm(50, mean = 50)
y1 <- sm.density(y, positive = TRUE)
y2 <- sm.density(y, positive = TRUE, display = "none")
all.equal(y1$estimate, y2$estimate)

while (rgl::rgl.cur() > 0) rgl::rgl.close()
