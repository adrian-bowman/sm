setwd("~/ownCloud/madrid")

library(lattice)
library(splines)
library(ggplot2)
library(rpanel)
library(sm)
sm.options(describe = FALSE)

source("sm.r")
source("sm-pam-utilities.r")
source("sm-fake-package.r")

source('~/ownCloud/rpanel_1.1-4-not-yet-released/rpanel/R/rp-plot4d.r')

n <- 300
x1 <- runif(n, 0 , 2 * pi)
x2 <- runif(n, 0 , 2 * pi)
x3 <- runif(n, 0 , 2 * pi)
y <- sin(x1) + rnorm(n, sd = 0.01)
y <- sin(x1) + cos(x2) + rnorm(n, sd = 0.01)
plot(y ~ x1)
model1 <- sm(y ~ s(x1, df = 6))
model2 <- sm(y ~ s(x1, df = 6) * s(x2, df = 6))
model3 <- sm(y ~ s(x1, df = 6) * s(x2, df = 6) * s(x3, df = 6))
summary(model2)
summary(model3)

source("sm.r")
plot(model1, 1)
plot(model1, 1, derivative = "x1", derivative.order = 1)
plot(model1, 1, derivative = "x1", derivative.order = 2)

plot(model2, 3, display = "persp", derivative = "x1", derivative.order = 1, include.lower.terms = TRUE)
plot(model2, 3, display = "persp", include.lower.terms = TRUE)
plot(model2, 3, derivative = "x1", derivative.order = 1, display = "persp")
plot(model2, 3, derivative = "x1", derivative.order = 1, display = "persp", include.lower.terms = TRUE)

plot(model3, 4, display = "persp", derivative = "x1", derivative.order = 1, include.lower.terms = TRUE)
plot(model3, 7, display = "persp", include.lower.terms = TRUE)
plot(model3, 7, derivative = "x1", derivative.order = 1, display = "persp")
plot(model3, 7, derivative = "x1", derivative.order = 1, display = "persp", include.lower.terms = TRUE)





xgrid <- seq(min(x), max(x), length = 100)
pred0 <- predict(model, xgrid)
pred1 <- predict(model, xgrid, deriv = 1)
pred2 <- predict(model, xgrid, deriv = 2)
plot(x, y)
lines(xgrid, pred0$fit)
lines(xgrid, pred1$fit, col = "green")
lines(xgrid, -pred2$fit, col = "red")
lines(xgrid, cos(xgrid), col = "red", lty = 2)

plot(model)
lines(xgrid, pred$fit, col = "blue")
plot(pred$fit ~ xgrid, type = "l", col = "blue")

plot(model$alpha[-1])
dalpha <- diff(model$alpha[-1])
plot(dalpha)
