#     Test code for sm.density.compare

library(sm)
if (reinstall) devtools::install("sm")

y <- rnorm(100)
g <- rep(1:2, rep(50,2))

test_label("Different forms of input for group", test.prompt)
par(mfrow = c(2, 2))
sm.density.compare(y, g)
sm.density.compare(y, as.character(g))
sm.density.compare(y, factor(g))
par(mfrow = c(1, 1))

test_label("Check simple graphical controls", test.prompt)
par(mfrow = c(1, 2))
sm.density.compare(y, g, xlim = c(-5, 5), yht = 1, lwd = 2)
sm.density.compare(y, g, model = "equal", xlim = c(-5, 5), ylim = c(0.25, 1))
par(mfrow = c(1, 1))

# This should plot nothing as the y-scale is out of range.
sm.density.compare(y, g, xlim = c(-5, 5), ylim = c(0.5, 1))

test_label("Tests", test.prompt)
par(mfrow = c(2, 2))
sm.density.compare(y, g, model = "equal")
sm.density.compare(y, g, model = "equal", col.band = "yellow")
model <- sm.density.compare(y, g, model = "equal")
# Reproduce this plot by low-level instructions
ev.pts <- model$eval.points
plot(range(ev.pts), c(0, max(model$upper)), type = "n", xlab = "x", ylab = "y")
polygon(c(ev.pts, rev(ev.pts)), c(model$upper, rev(model$lower)), col = "lightblue",
        border = NA)
sm.density(y[g==1], eval.points = ev.pts, add = TRUE, col = "red")
sm.density(y[g==2], eval.points = ev.pts, add = TRUE, col = "green")
par(mfrow = c(1, 1))

par(mfrow = c(2, 2))
sm.density.compare(y, g)
sm.density.compare(y, g, model = "equal", test = FALSE)
sm.density.compare(y, g, method = "cv")
sm.density.compare(y, g, method = "sj")
par(mfrow = c(1, 1))

# Check smoothing parameter selection
h.select(y, group = g)
h.select(y, group = g, method = "cv")
h.select(y, group = g, method = "sj")

test_label("Output", test.prompt)
par(mfrow = c(1, 2))
comp <- sm.density.compare(y, g)
legend("topleft", comp$levels, col = comp$col, lty = comp$lty, lwd = comp$lwd)
comp <- sm.density.compare(y, g, col = c("blue", "green"))
legend("topleft", comp$levels, col = comp$col, lty = comp$lty, lwd = comp$lwd)
par(mfrow = c(1, 1))
