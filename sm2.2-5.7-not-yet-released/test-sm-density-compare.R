#     Test code for sm.density.compare

setwd("~/research/sm2.2-5.7-not-yet-released/")

detach(package:sm)
unloadNamespace("sm")
system("R CMD build sm")
install.packages("sm_2.2-5.7.tar.gz", repos = NULL, type = "source")
library(sm)

y <- rnorm(100)
g <- rep(1:2, rep(50,2))

sm.density.compare(y, g)
# Check simple graphical controls.
sm.density.compare(y, g, xlim = c(-5, 5), yht = 1, lwd = 2)
# This should plot nothing as the y-scale is out of range.
sm.density.compare(y, g, xlim = c(-5, 5), ylim = c(0.5, 1))
sm.density.compare(y, g, model = "equal")
# Check simple graphical controls.
sm.density.compare(y, g, model = "equal", xlim = c(-5, 5), ylim = c(0.25, 1))

model <- sm.density.compare(y, g, model = "equal")
# Reproduce this plot by low-level instructions
ev.pts <- model$eval.points
plot(range(ev.pts), c(0, max(model$upper)), type = "n", xlab = "x", ylab = "y")
polygon(c(ev.pts, rev(ev.pts)), c(model$upper, rev(model$lower)), col = "lightblue",
        border = NA)
sm.density(y[g==1], eval.points = xgrid, add = TRUE, col = "red")
sm.density(y[g==2], eval.points = xgrid, add = TRUE, col = "green")
