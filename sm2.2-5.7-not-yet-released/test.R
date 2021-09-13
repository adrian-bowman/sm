#       Testing of sm version 2.1

# detach(package:sm)

# See test-3d.r for rgl tests

library(sm)
sm.options(describe = FALSE, show.script = FALSE)

x <- matrix(rnorm(100), ncol = 2)
y <- x[,1]^2 + x[,2]^2 + rnorm(50)
sm.regression(x, y)
sm.regression(x, y, col = "blue")

sm.density(x)
sm.density(x, col = "red")


#----------------------------------------------------------
#       Density estimation
#----------------------------------------------------------

y <- rnorm(1000)
sm.density(y, h = hnorm(y))

x <- matrix(rnorm(100), ncol = 2)
sm.density(x[,1])
sm.density(x)
sm.density(x, theta = 0, phi = 0)
sm.density(x, theta = -30, phi = 40)
sm.density(x, display = "image")
sm.density(x, display = "slice", add = TRUE)
sm.density(exp(x), display = "slice", positive = TRUE)
sm.density(exp(x), display = "image", positive = TRUE)
sm.density(exp(x), positive = TRUE, theta = -30, phi = 40)

# x <- matrix(rnorm(150), ncol = 3)
# sm.density(x)

x <- rnorm(200)
g <- rep(1:2, rep(100,2))
sm.density(x, group = g)
sm.density.compare(x, g)
sm.density.compare(x, g, model = "equal", verbose = 2) 
sm.density.compare(x, g, model = "equal") 
sm.density.compare(x, g, model = "equal", test = FALSE) 

y <- exp(rnorm(50))


sm.density(y, positive = TRUE)


#----------------------------------------------------------
#       Ancova
#----------------------------------------------------------

air <- read.table("air.dat")
names(air) <- c("ozone", "temperature", "wind", "group", "radiation")
attach(air)
x         <- cbind(wind, temperature)
y         <- ozone
group     <- radiation < 200
h         <- c(3,5)

sm.options(list(xlab = "Wind", ylab = "Ozone"))
sm.ancova(x[,1], y, as.numeric(group), h[1], model = "equal")
sm.ancova(x[,1], y, as.numeric(group), h[1], model = "parallel")

sm.options(list(xlab = "Wind", ylab = "Temperature", zlab = "Ozone",
                zlim =  c(1.8, 5.2), theta = 45, phi = 30, ngrid = 20))
sm.regression(x[!group,], y[!group], h, model = "no effect")
sm.regression(x[ group,], y[ group], h, model = "no effect")
sm.regression(x[!group,], y[!group], h, model = "linear")
sm.regression(x[ group,], y[ group], h, model = "linear")
sm.ancova(x, y, group, h, model = "equal")
sm.ancova(x, y, group, h, model = "parallel")

#   1-covariate data



n <- 200
x <- sort(rnorm(n))
y <- x^2 + rnorm(n)
h.select(x, y, nbins = 0)
h.select(x, y)
h.select(x, y, method = "cv")
h.df <- h.select(x, y)
h.cv <- h.select(x, y, method = "cv")
sm.regression(x, y)
sm.regression(x, y, h = h.df)
sm.regression(x, y, h = h.cv, add = TRUE, lty = 2)
sm.regression(x, y, method = "cv", add = TRUE)

g <- rbinom(n, 1, 0.5)
h.select(x[g==0], y[g==0])
h.select(x[g==1], y[g==1])
h.select(x, y, group = g)

n <- 100
x <- sort(rnorm(n))
y <- x^2 + rnorm(n)
g <- rbinom(n, 1, 0.5)
h.select(x[g==0], y[g==0], method = "df", df = 4)
h.select(x[g==1], y[g==1], method = "df", df = 4)
h.select(x, y, group = g, method = "df", df = 4)
 


n <- 100
x <- sort(runif(n))
y <- sin(2 * pi * x) + rnorm(n, sd = 0.2)
g <- rbinom(n, 1, 0.5)
sm.ancova(x, y, g, model="equal")
sm.ancova(x, y, g, df = 6, model="equal")
sm.regression(x, y, group = g, model="equal")
sm.regression(x, y, group = g, df = 6, model="equal")

par(mfrow = c(1, 2))
sm.ancova(x, y, g, df = 6, model="equal")
sm.ancova(x, y, g, h = h.select(x, y, group = g, method = "aicc"),
            model="equal")
par(mfrow = c(1, 1))


h.select(x, y)

n <- 499
n <- 501
x <- rnorm(n)
y <- rnorm(n)
h.select(x, y)
h.select(x, y, nbins = 0)
h.select(x, y, nbins = 50)

#   2-covariate data



n  <- 200
g  <- rbinom(n, 1, 0.5)
x  <- cbind(rnorm(n), rnorm(n))
y  <- x[,1]^2 + x[,2]^2 + 4 * rnorm(n)
# y  <- x[,1]^2 + x[,2]^2 + (g==2)*3*x[,1] + 4*rnorm(n)
y[g==2] <- y[g==2] + 1
sm.ancova(x, y, g, h= c(0.5,0.5), model = "equal")
sm.ancova(x, y, g, model = "equal")


h.select(x[g == 0, ], y[g == 0])
h.select(x[g == 0, ], y[g == 0], df = 8)
h.select(x[g == 0, ], y[g == 0], method = "cv")
h.select(x[g == 0, ], y[g == 0], method = "cv", structure.2d = "common")
h.select(x[g == 0, ], y[g == 0], method = "aicc")
h.select(x[g == 0, ], method = "normal")
h.select(x[g == 0, ], method = "cv", structure.2d = "common")

sm.density(x[g == 0, ], method = "normal")
sm.density(x[g == 0, ], method = "cv")
h.select(x[g == 0, ], y[g == 0])
h.select(x[g == 1, ], y[g == 1])
h.select(x, y, group = g, structure.2d = "common")
sm.regression(x[g == 0, ], y[g == 0])
sm.regression(x[g == 0, ], y[g == 0], method = "cv")

n  <- 200
x  <- cbind(rnorm(n), rnorm(n))
y  <- x[,1]^2 + x[,2]^2 + 4 * rnorm(n)
sm.regression(x, y, h = c(0.5, 0.5))
sm.regression(x, y)


par(mfrow = c(2,2))
sm.density.compare(x[,1], g)
sm.density.compare(x[,1], g, model = "equal", test = FALSE)
sm.density.compare(x[,1], g, method = "cv")
sm.density.compare(x[,1], g, method = "sj")
par(mfrow = c(2,2))
h.select(x[,1], group = g)
h.select(x[,1], group = g, method = "cv")
h.select(x[,1], group = g, method = "sj")
