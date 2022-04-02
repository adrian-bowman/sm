#     Test examples for the sm function

library(sm)
if (reinstall) devtools::install("sm")

library(lattice)
library(splines)
library(rpanel)

sm.options(describe = FALSE)

x <- seq(0, 1, length = 50)
y <- x + rnorm(50, sd = 0.15)
plot(x, y)

sm(y)

sm(x, y)
model <- sm(y ~ s(x))
plot(model)

model <- sm(y ~ s(x, xrange = c(-2, 2)))
model$xrange
model <- sm(y ~ s(x, df = 4))
plot(model)

#     One covariate

sm(Score1 ~ s(Longitude), data = trawl)
sm(Score1 ~ s(Longitude, df = 4), data = trawl)
sm(Score1 ~ s(Longitude, df = 10), data = trawl)
sm(Score1 ~ s(Longitude), se = TRUE, panel = TRUE, data = trawl)
sm(Score1 ~ s(Longitude), lambda = 16.6, data = trawl)

model <- sm(Score1 ~ s(Longitude, lambda = 10000000), data = trawl)
model$df.model
model <- sm(Score1 ~ s(Longitude, df = 20), data = trawl)
model$lambda

sm(Longitude, Score1, data = trawl)
with(trawl, sm.regression(Longitude, Score1))

with(trawl, sm.regression(Longitude, Score1, panel = TRUE, se = TRUE, col = "blue"))

#     Illustration of basis weights

n <- 100
x <- seq(0, 1, length = n)
y <- sin(2 * pi *x) + rnorm(n, sd = 0.3)
y <- y - mean(y)
plot(x, y)
# png("figures/basis.png")
model <- sm(y ~ s(x, df = 4, xrange = c(0, 1), nseg = 8))
# model <- sm(y ~ s(x, df = 4, xrange = c(0, 1), nseg = 8))
matplot(x, model$B[ , -1], type = "l", ylab = "")
# dev.off()

# png("figures/basis6.png")
adf <- 6
model <- sm(y ~ s(x, df = adf, xrange = c(0, 1)), nseg = 20)
plot(model, lwd = 3)
matplot(x, t(t(model$B) * model$alpha), type = "l", add = TRUE)
# dev.off()

# Two covariates
ngrid <- 100
x1 <- seq(0, 1, length = ngrid)
x2 <- seq(0, 1, length = ngrid)
X  <- as.matrix(expand.grid(x1, x2))
y  <- rnorm(nrow(X))
model <- sm(y ~ s(X, nseg = c(4, 4)))
dim(model$B)
plot(x1, x2, type = "n")
for (i in 2:ncol(model$B))
   contour(matrix(model$B[ , i], nrow = ngrid), col = i, lty = i, nlevels = 2, add = TRUE)
image(matrix(model$B[ , 26], nrow = ngrid))
persp(matrix(model$B[ , 25], nrow = ngrid), ticktype = "detailed")

#     Periodic terms

x <- 5 + seq(0, 0.8, length = 50)
y <- sin(2 * pi * x) + rnorm(50, sd = 0.3)
plot(y ~ x)

source("sm.r")
model0 <- sm(y ~ s(x))
model1 <- sm(y ~ s(x, period = 1))
abline(h = fitted(model1)[1], col = "green")
abline(v = 0, col = "green")
abline(v = 1, col = "green")
lines(x, fitted(model0), col = "black", lty = 2)

x1 <- seq(0, 1, length = 20)
x2 <- seq(0, 1, length = 20)
g  <- expand.grid(x1, x2)
x1 <- g[ , 1]
x2 <- g[ , 2]
y <- sin(2 * pi * x1) + x2 + rnorm(400, sd = 0.3)
rp.plot3d(x1, y, x2)

x <- cbind(x1, x2)
model0 <- sm(y ~ s(x), display = "persp")
source("sm.r")
model1 <- sm(y ~ s(x, period = c(1, NA)), display = "persp")
model1 <- sm(y ~ s(x, period = c(1)))


#     Derivatives

x <- runif(50, 0 , 2 * pi)
y <- sin(x) + rnorm(50, sd = 0.05)
plot(y ~ x)

model <- sm(y ~ s(x, df = 4), xrange = c(-0.2, 2 * pi + 0.2))
xgrid <- seq(0.2, 2 * pi - 0.2, length = 100)
pred0 <- predict(model, xgrid)
pred1 <- predict(model, xgrid, deriv = 1)
pred2 <- predict(model, xgrid, deriv = 2)
plot(x, y)
lines(xgrid, pred0$fit)
lines(xgrid, pred1$fit, col = "green")
lines(xgrid, -pred2$fit, col = "red")
lines(xgrid, cos(xgrid), col = "red", lty = 2)

model <- sm(y ~ s(x))
plot(model, deriv = "x", deriv.order = 1)

plot(model)
lines(xgrid, pred$fit, col = "blue")
plot(pred$fit ~ xgrid, type = "l", col = "blue")

plot(model$alpha[-1])
dalpha <- diff(model$alpha[-1])
plot(dalpha)


#     Two covariates

n  <- 300
x1 <- runif(n)
x2 <- runif(n)
x  <- cbind(x1, x2)
y  <- cos(2*pi*x[,1]) + sin(2*pi*x[,2]) + 
                rnorm(n, sd = 0.2)
y  <- cos(2*pi*x[,1]) + rnorm(n, sd = 0.2)
sm(y ~ s(x), panel = TRUE)

model <- sm(y ~ s(x), se = TRUE, reference = "no effect")
model <- sm(y ~ s(x1) * s(x2))
model <- sm(y ~ s(x1) + s(x2) + s(x1):s(x2))
summary(model)
plot(model, component = 1)
plot(model, component = 3)
plot(model, component = 3, include.lower.terms = TRUE)
plot(model, component = 3, include.lower.terms = TRUE, deriv = "x1", deriv.order = 1)
plt <- plot(model, component = 3, se = TRUE, reference = "no effect")
est <- plt[[1]]$est
se  <- plt[[1]]$st.error

x <- runif(n)
y <- rnorm(n)
source("sm.r")
model <- sm(y ~ s(x, lambda = 100))
source("ps-normal.r")
model0 <- ps.normal(x, y, lambda = 100)
points(x, model$fitted, col = "red")
cbind(model$fitted, model0$muhat)

n <- 49
x <- as.matrix(expand.grid(1:7, 1:7))
y <- rnorm(n)
source("sm.r")
model <- sm(y ~ s(x, df = 12),
           display = "persp", theta = 30, phi = 15)
source("ps-normal.r")
model0 <- ps.normal(x, y, df = 12)

source("sm.r")
model <- sm(y ~ s(x, lambda = 100), mask.method = "none",
           display = "persp", theta = 30, phi = 15)
source("ps-normal.r")
model0 <- ps.normal(x, y, lambda = rep(100, 2))

temp <- plot(model, display = "persp", theta = 30, phi = 15)
cbind(temp[1,], model0$estimate[1,])

all(abs(model$fitted - model0$muhat) < 1e-8)
model$fitted[1] == model0$muhat[1]
print(c(model$fitted[1], model0$muhat[1]))
which(model$fitted != model0$muhat)
cbind(model$fitted, model0$muhat)
model0 <- ps.normal(x, y, lambda = rep(100, 2))
cbind(model$alpha[-1], model0$beta - mean(model0$beta))
all(model$B == model0$B)
all(t(model$B) %*% model$B == t(model0$B) %*% model0$B)
all(model$btb == model0$btb)

model <- sm(y ~ s(x, lambda = 80),
           display = "persp", theta = 30, phi = 15)
persp(matrix(model$alpha, ncol = 20),
        ticktype = "detailed", zlim = c(-0.5, 0.5))
persp(matrix(model0$beta, ncol = 20),
        ticktype = "detailed", zlim = c(-0.5, 0.5))


cbind(model$fitted, model0$muhat)

model <- sm(y ~ s(x), panel = TRUE,
           display = "persp", theta = 30, phi = 15)

persp(matrix(model$alpha[-1], ncol = 20),
        ticktype = "detailed")
persp(matrix(model0$beta, ncol = 20),
        ticktype = "detailed")

Position <- cbind(Latitude, Longitude)
source("sm.r")
sm(Score1 ~ s(Position, df = 20), display = "persp", panel = TRUE)
source("sm.r")
model <- sm(Score1 ~ s(Position, lambda = 1000000))
model <- sm(Score1 ~ s(Position, df = 20))
model$df.model

source("ps_normal2.r")
ps.normal2(Position, Score1, lambda = c(1, 1))
shell.colours <- function(n)
   topo.colors(round(n * 1.6))[-(1:round(n*0.6))]
source("sm.r")
plot(model, display = "image", col.palette = shell.colours)

x <- cbind(runif(50), runif(50))
y <- rnorm(50)
sm.regression

#     Three covariates
     
n <- 300
x <- cbind(runif(n), runif(n), runif(n))
x <- cbind(x = runif(n), yyy = runif(n), z = runif(n))
y <- cos(2*pi*x[,1]) + sin(2*pi*x[,2]) - x[,3] + 
                rnorm(n, sd = 0.002)
y <- cos(2*pi*x[,1]) +
                rnorm(n, sd = 0.002)

source("sm.r")
model1 <- sm(y ~ s(x, df = 100))
source("sm.r")
plot(model1)
plot(model1, panel.plot = FALSE)
model1 <- sm(y ~ s(x, df = 100), display = "none")
model1$df.model
model2 <- sm(y ~ s(x, xrange = rbind(c(0,1), c(0,1), c(0,1))))
source("sm.r")
plot(model1, display = "rgl", nlevels = 3)
plot(model2)
plot(model, col.palette = shell.colours)

x12 <- x[ , 1:2]
x3  <- x[ , 3]
model <- sm(y ~ s(x12) * s(x3), verbose = 2)
summary(model)
plot(model, 3, se = TRUE, reference = "no effect")

x1   <- seq(0, 1, length = 10)
x2   <- seq(0, 1, length = 10)
x3   <- seq(0, 1, length = 10)
xnew <- as.matrix(expand.grid(x1, x2, x3))
source("sm.r")
pred <- predict(model1, xnew)
pred <- predict(model2, xnew)

#     Additive models

n  <- 200
x1 <- runif(n)
x2 <- runif(n)
x3 <- runif(n)
x4 <- runif(n)
y  <- x2 + sin(2 * pi * x3) + x4^2 + rnorm(n, sd = 0.2)

source("sm.r")
model <- sm(y ~ s(x2) * s(x3) * s(x4))
source("sm.r")
summary(model)
anova(model)

model <- sm(y ~ s(x1) + s(x2) + s(x3) + s(x4))
plot(model)

x12 <- cbind(x1, x2)
model <- sm(y ~ s(x1) + s(x2) + s(x3) * s(x4))
plot(model, 1:4)
plot(model, 5)
anova(model)

source("sm.r")
newdata <- data.frame(x1 = runif(10), x2 = runif(10), 
                      x3 = runif(10), x4 = runif(10))
                      
source("sm.r")
xx <- cbind(runif(100), runif(100))
y <- rnorm(100)
source("sm.r")
model <- sm(y ~ s(xx))
x1 <- xx[,1]
x2 <- xx[,2]
source("sm.r")
model <- sm(y ~ s(x1)*s(x2))
source("sm.r")
model <- sm(y ~ s(xx, xrange = rbind(c(0,1), c(-1, 2))))
source("sm.r")
plot(model, mask.method = "hull", display = "image")
source("sm.r")
model <- sm(y ~ s(x1))
xxnew <- expand.grid(seq(0, 1.2, length = 10), seq(0, 1.2, length = 10))
xxnew <- as.matrix(expand.grid(seq(0, 1.2, length = 10),
                               seq(0, 1.2, length = 10)))
source("sm.r")
pred <- predict(model, list(xx = xxnew))
pred

source("sm.r")
xx <- runif(100)
y <- rnorm(100)
source("sm.r")

source("sm.r")
model <- sm(y ~ s(xx, df = 1.1, xrange = c(-0.5, 1.5)))
xxnew <- seq(-0.5, 1.5, length = 100)
pred <- predict(model, data.frame(xx = xxnew), se.fit = TRUE)
plot(y ~ xx, xlim = c(-0.5, 1.5))
points(xxnew[pred$inrange], pred$fit, col = "red")
points(xxnew[pred$inrange], pred$fit - 2 * pred$se.fit, col = "green")
points(xxnew[pred$inrange], pred$fit + 2 * pred$se.fit, col = "green")


ord <- order(xxnew)
lines(xxnew[ord], pred$fit[ord] + 2 * pred$se.fit[ord], lty = 2, col = "green")
lines(xxnew[ord], pred$fit[ord] - 2 * pred$se.fit[ord], lty = 2, col = "green")


xxnew <- cbind(runif(10), runif(10))
source("sm.r")
pred <- predict(model, xxnew)
newdata <- data.frame(x1, x2, x3, x4)
all(model$fitted == predict(model, newdata))

anova(model)
plot(model)
summary(model)

x4 <- cbind(x2, x3)
model <- sm(y ~ s(x1, lambda = 100) + s(x4, lambda = c(100, 100)))
plot(model, 1)
plot(model, 2, display = "persp")

model <- sm(y ~ s(x1, lambda = 100) * s(x4, lambda = c(100, 100)))
summary(model)
plot(model, 1)
plot(model, 2)
plot(model, 3)

y     <- 5 + rnorm(50)
x1    <- rnorm(50)
x2    <- rnorm(50)
x3    <- rnorm(50)
x4    <- rnorm(50)
x34   <- cbind(x3, x4)
x234  <- cbind(x2, x3, x4)
model <- sm(y ~ s(x1) + s(x2) + s(x3) + s(x4))
plot(model, se = TRUE)
model <- sm(y ~ s(x1) + s(x2) + s(x3, x4))
plot(model, 1:2, se = TRUE)
model <- sm(y ~ s(x1) + s(x2) + s(x34))
plot(model)
model <- sm(y ~ s(x1) + s(x2) + s(x3) * s(x4))
plot(model, 5)
plot(model, 5, include.lower.terms = TRUE)
model <- sm(y ~ s(x1) + s(x2, x34))
plot(model)
model <- sm(y ~ s(x1) + s(x234))
plot(model)
model <- sm(y ~ s(x1) + s(x34, x2))
plot(model)
model <- sm(y ~ s(x1, x2) + s(x3, x4))
plot(model, 1)
plot(model, 2)
model <- sm(y ~ s(x1) * s(x2) + s(x3) * s(x4))
plot(model, 5, include.lower.terms = TRUE)
plot(model, 6, include.lower.terms = TRUE)


#     EA data

Location.year <- cbind(Easting, Northing, decyear)
model <- ps.additive(y ~ s(Location.year, lambda = c(1, 1, 1)))
plot(model, 1)
model <- ps.additive(y ~ s(Easting, lambda = 1) + 
                         s(Northing, lambda = 1) + 
                         s(decyear, lambda = 1) +
                         s(doy, lambda = 1) +
                         s(Easting, Northing, lambda = c(1, 1)))
plot(model, 1:4)
plot(model, 5)

x <- cbind(Easting, Northing, doy)
model <- ps.additive(list(x[,1], x[,2], x[,3], 
                          x[,1:2], x[,2:3], x[,c(1,3)], x),
                     y, 
                     list(1, 1, 1, 
                          rep(0.1, 2), rep(0.1, 2), rep(0.1, 2), 
                          rep(0.01, 3)),
                     nseg = 8)
plot(model, 3, display = "persp")

#     Fixed point

data(trawl)
attach(trawl)

source("sm.r")
model <- sm(Score1 ~ s(Longitude, df = 6))
fixed <- matrix(c(143.4, 1.5), 
               ncol = 2, byrow = TRUE)
fixed <- matrix(c(143.4, 1.5, 143.8, 1.5), 
               ncol = 2, byrow = TRUE)
source("sm.r")
model <- sm(Score1 ~ s(Longitude, df = 6, fixed = fixed))
plot(model)
plot(Longitude, Score1)
points(Longitude, model$fitted, col = "green")
points(fixed, col = "red")

Position  <- cbind(Longitude - 143, Latitude)
model <- sm(Score1 ~ s(Position))
plot(model, display = "rgl")
fxd   <- c(-0.1, -11.8, 1.8)
fxd   <- rbind(c(-0.1, -11.8, 1.8), c(-0.1, -11.2, 1.8))
model <- sm(Score1 ~ s(Position, fixed = fxd))
plot(model, display = "rgl")

source("ps-normal.r")
model <- ps.normal(Longitude, Score1, lambda = 5000)
model <- ps.normal(Longitude, Score1, lambda = 5000, fixed = fixed)


x <- seq(0, 1, length = 100)
B <- bbase(x, xl = 0, xr = 1, nseg = 15, deg = 3)
matplot(x, B[ , 4:(ncol(B) - 3)], type = "l", ylab = "")

x <- runif(50)
y <- sin(2 * pi * x) + rnorm(50)
plot(x, y)
abline(h = 0, lty = 2)
B     <- bbase(x, xl = 0, xr = 1, nseg = 15, deg = 3)
B     <- B[ , 4:(ncol(B) - 3)]
alpha <- solve(t(B) %*% B) %*% t(B) %*% y
xg    <- seq(0, 1, length = 100)
Bg    <- bbase(xg, xl = 0, xr = 1, nseg = 15, deg = 3)
Bg    <- Bg[ , 4:(ncol(Bg) - 3)]
fv    <- Bg %*% alpha
lines(xg, fv, lwd = 2, col = "blue")

#     Monotonic

source("ps-normal.r")
ps.normal(Longitude, Score1, lambda = 5000)
ps.normal(Longitude, Score1, lambda = 5000, decreasing = TRUE)
ps.normal(Longitude, Score1, lambda = 5000, increasing = TRUE)



#     See how large the sample sizes can be for one and two 
#     covariates
     
n  <- 2000
x1 <- cbind(runif(n))
x2 <- cbind(runif(n), runif(n))
y  <- rnorm(n)
model <- sm(y ~ s(x2, lambda = c(1, 1)), nseg = 10)


#     Toluene data from Shell

setwd("/Volumes/adrian/research/daniel")

d <- read.table("Shell data/Toluene.txt", header = TRUE)
attach(d)

library(MASS)
eqscplot(XCoord, YCoord)

shell.colours <- function(n)
   topo.colors(round(n * 1.6))[-(1:round(n * 0.6))]

x <- cbind(XCoord, YCoord, SampleDateNumeric)

clr <- shell.colours(20)[cut(log(Result), 20, labels = FALSE)]
lvls <- c(min(log(Result)), log(c(5, 10, 25, 50, 75, 100, 200, 400, 800, 
                         1500, 3000, 5000)), max(log(Result)))
clr <- shell.colours(13)[cut(log(Result), lvls, labels = FALSE)]
rp.plot3d(XCoord, YCoord, SampleDateNumeric, col = clr)

#     Daniels' function Date.92X in FUnctions.r converts dates 
#     from numeric to character

source("/Volumes/adrian/research/madrid/sm.r")
model <- sm(log(Result) ~ s(x, df = 80), display = "none")
plot(model, partial.residuals = TRUE, col.palette = shell.colours,
                  hscale = 1.5, vscale = 1.5)

n   <- 400
xx  <- runif(n)
yy  <- runif(n)
zz  <- runif(n)
cc1 <- -1 + zz * 2
cc2 <- -1 + zz * 2
rr <- exp(-0.5*((xx - zz)/0.2)^2 - 0.5*((yy - zz)/0.2)^2) + rnorm(n, sd = 0.001)
clr <- topo.colors(20)[cut(rr, 20, labels = FALSE)]
rp.plot3d(xx, yy, zz, col = clr)

xyz <- cbind(xx, yy, zz)
model <- sm(rr ~ s(xyz), df = 200)

