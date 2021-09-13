pkgname <- "sm"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "sm-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('sm')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("binning")
### * binning

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: binning
### Title: Construct frequency table from raw data in 1, 2 or 3 dimensions.
### Aliases: binning
### Keywords: nonparametric

### ** Examples

# example of 1-d use
x  <- rnorm(1000)
xb <- binning(x)
xb <- binning(x, breaks=seq(-4,4,by=0.5))
# example of 2-d use
x <- rnorm(1000)
y <- 2*x + 0.5*rnorm(1000)
x <- cbind(x, y)
xb<- binning(x, nbins=12)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("binning", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("h.select")
### * h.select

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: h.select
### Title: Selection of the smoothing parameter
### Aliases: h.select
### Keywords: nonparametric regression smooth

### ** Examples

x <- rnorm(50)
h.select(x)
h.select(x, method = "sj")

x <- matrix(rnorm(100), ncol = 2)
h.select(x)
sm.density(x, method = "cv")

x <- rnorm(50)
y <- x^2 + rnorm(50)
h.select(x, y)
sm.regression(x, y, method = "aicc")

x <- matrix(rnorm(100), ncol = 2)
y <- x[,1]^2 + x[,2]^2 + rnorm(50)
h.select(x, y, method = "cv", structure.2d = "common")
sm.regression(x, y, df = 8)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("h.select", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("hcv")
### * hcv

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: hcv
### Title: Cross-validatory choice of smoothing parameter
### Aliases: hcv
### Keywords: nonparametric smooth

### ** Examples

#  Density estimation

x <- rnorm(50)
par(mfrow=c(1,2))
h.cv <- hcv(x, display="lines", ngrid=32)
sm.density(x, h=hcv(x))
par(mfrow=c(1,1))

#  Nonparametric regression

x <- seq(0, 1, length = 50)
y <- rnorm(50, sin(2 * pi * x), 0.2)
par(mfrow=c(1,2))
h.cv <- hcv(x, y, display="lines", ngrid=32)
sm.regression(x, y, h=hcv(x, y))
par(mfrow=c(1,1))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("hcv", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("hnorm")
### * hnorm

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: hnorm
### Title: Normal optimal choice of smoothing parameter in density
###   estimation
### Aliases: hnorm
### Keywords: nonparametric smooth

### ** Examples

x <- rnorm(50)
hnorm(x)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("hnorm", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("hsj")
### * hsj

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: hsj
### Title: Sheather-Jones choice of smoothing parameter for density
###   estimation
### Aliases: hsj
### Keywords: nonparametric smooth

### ** Examples

x <- rnorm(50)
hsj(x)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("hsj", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("mosses")
### * mosses

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: mosses
### Title: Heavy metals in mosses in Galicia.
### Aliases: mosses
### Keywords: smooth regression

### ** Examples

## Not run: 
##D # Comparison of Co in March and September
##D    
##D with(mosses, {
##D 	
##D    nbins <- 12
##D    vgm.m <- sm.variogram(loc.m, Co.m, nbins = nbins, original.scale = TRUE,
##D                         ylim = c(0, 1.5))
##D    vgm.s <- sm.variogram(loc.s, Co.s, nbins = nbins, original.scale = TRUE,
##D                         add = TRUE, col.points = "blue")
##D                         
##D    trns <- function(x) (x / 0.977741)^4
##D    del <- 1000
##D    plot(vgm.m$distance.mean, trns(vgm.m$sqrtdiff.mean), type = "b",
##D          ylim = c(0, 1.5), xlab = "Distance", ylab = "Semi-variogram")
##D    points(vgm.s$distance.mean - del, trns(vgm.s$sqrtdiff.mean), type = "b",
##D          col = "blue", pch = 2, lty = 2)
##D 
##D    plot(vgm.m$distance.mean, trns(vgm.m$sqrtdiff.mean), type = "b",
##D          ylim = c(0, 1.5), xlab = "Distance", ylab = "Semi-variogram")
##D    points(vgm.s$distance.mean - del, trns(vgm.s$sqrtdiff.mean), type = "b",
##D          col = "blue", pch = 2, lty = 2)
##D    segments(vgm.m$distance.mean, trns(vgm.m$sqrtdiff.mean - 2 * vgm.m$se),
##D          vgm.m$distance.mean, trns(vgm.m$sqrtdiff.mean + 2 * vgm.m$se))
##D    segments(vgm.s$distance.mean - del, trns(vgm.s$sqrtdiff.mean - 2 * vgm.s$se),
##D          vgm.s$distance.mean - del, trns(vgm.s$sqrtdiff.mean + 2 * vgm.s$se),
##D          col = "blue", lty = 2)
##D 
##D    mn <- (vgm.m$sqrtdiff.mean + vgm.s$sqrtdiff.mean) / 2
##D    se <- sqrt(vgm.m$se^2 + vgm.s$se^2)
##D    plot(vgm.m$distance.mean, trns(vgm.m$sqrtdiff.mean), type = "n",
##D         ylim = c(0, 1.5), xlab = "Distance", ylab = "Semi-variogram")
##D    polygon(c(vgm.m$distance.mean, rev(vgm.m$distance.mean)),
##D         c(trns(mn - se), rev(trns(mn + se))),
##D         border = NA, col = "lightblue")  
##D    points(vgm.m$distance.mean, trns(vgm.m$sqrtdiff.mean))
##D    points(vgm.s$distance.mean, trns(vgm.s$sqrtdiff.mean), col = "blue", pch = 2)
##D 
##D    vgm1 <- sm.variogram(loc.m, Co.m, nbins = nbins, varmat = TRUE, 
##D                         display = "none")
##D    vgm2 <- sm.variogram(loc.s, Co.s, nbins = nbins, varmat = TRUE,
##D                         display = "none")
##D 
##D    nbin  <- length(vgm1$distance.mean)
##D    vdiff <- vgm1$sqrtdiff.mean - vgm2$sqrtdiff.mean
##D    tstat <- c(vdiff %*% solve(vgm1$V + vgm2$V) %*% vdiff)
##D    pval  <- 1 - pchisq(tstat, nbin)
##D    print(pval)
##D })
##D 
##D # Assessing isotropy for Hg in March
##D 
##D with(mosses, {
##D    sm.variogram(loc.m, Hg.m, model = "isotropic")
##D })
##D 
##D # Assessing stationarity for Hg in September
##D 
##D with(mosses, {
##D    vgm.sty <- sm.variogram(loc.s, Hg.s, model = "stationary")
##D    i <- 1
##D    image(vgm.sty$eval.points[[1]], vgm.sty$eval.points[[2]], vgm.sty$estimate[ , , i],
##D          col = topo.colors(20))
##D    contour(vgm.sty$eval.points[[1]], vgm.sty$eval.points[[2]], vgm.sty$sdiff[ , , i],
##D          col = "red", add = TRUE)
##D })
##D 
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("mosses", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("nise")
### * nise

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: nise
### Title: Integrated squared error between a density estimate and a Normal
###   density
### Aliases: nise
### Keywords: nonparametric smooth

### ** Examples

x <- rnorm(100)
nise(x)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("nise", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("nmise")
### * nmise

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: nmise
### Title: mean integrated squared error for density estimation with normal
###   data
### Aliases: nmise
### Keywords: nonparametric smooth

### ** Examples

x  <- rnorm(50)
sd <- sqrt(var(x))
n  <- length(x)
h  <- seq(0.1, 2, length=32)
plot(h, nmise(sd, n, h), type = "l")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("nmise", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("nnbr")
### * nnbr

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: nnbr
### Title: nearest neighbour distances from data in one or two dimensions
### Aliases: nnbr
### Keywords: nonparametric smooth

### ** Examples

x  <- rnorm(50)
hw <- nnbr(x, 10)
hw <- hw/exp(mean(log(hw)))
sm.density(x, h.weights=hw)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("nnbr", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("provide.data")
### * provide.data

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: provide.data
### Title: Making data available as data.frame
### Aliases: provide.data
### Keywords: utilities

### ** Examples

provide.data(birth)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("provide.data", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("sig.trace")
### * sig.trace

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: sig.trace
### Title: A significance trace for a hypothesis test
### Aliases: sig.trace
### Keywords: nonparametric smooth

### ** Examples

x <- runif(50, 0, 1)
y <- 5*x^2 + rnorm(50)
sig.trace(sm.regression(x, y, model = "linear", display="none"), 
        hvec = seq(0.05, 0.3, length = 10))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("sig.trace", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("sm.ancova")
### * sm.ancova

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: sm.ancova
### Title: Nonparametric analysis of covariance
### Aliases: sm.ancova
### Keywords: nonparametric smooth

### ** Examples

x <- runif(50, 0, 1)
y <- 4*sin(6*x) + rnorm(50)
g <- rbinom(50, 1, 0.5)
sm.ancova(x, y, g, h = 0.15, model = "equal")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("sm.ancova", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("sm.autoregression")
### * sm.autoregression

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: sm.autoregression
### Title: Nonparametric estimation of the autoregression function
### Aliases: sm.autoregression
### Keywords: nonparametric smooth ts

### ** Examples

sm.autoregression(log(lynx), maxlag=3, se=TRUE)
sm.autoregression(log(lynx), lags=cbind(2:3,4:5))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("sm.autoregression", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("sm.binomial")
### * sm.binomial

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: sm.binomial
### Title: Nonparametric logistic regression
### Aliases: sm.binomial
### Keywords: nonparametric smooth models

### ** Examples
## Not run: 
##D # the next example assumes that all binomial denominators are 1's
##D sm.binomial(dose, failure, h=0.5)
##D # in the next example, (some of) the dose levels are replicated 
##D sm.binomial(dose, failure, n.trials, h=0.5)
## End(Not run)

with(birth, {
   sm.binomial(Lwt[Smoke=="S"], Low[Smoke=="S"], h=20,
           xlab='mother weight[Smoke=="S"]')
   x<- seq(0,1,length=30)
   y<- rbinom(30,10,prob=2*sin(x)/(1+x))
   sm.binomial(x,y,N=rep(10,30), h=0.25)
})



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("sm.binomial", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("sm.binomial.bootstrap")
### * sm.binomial.bootstrap

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: sm.binomial.bootstrap
### Title: Bootstrap goodness-of-fit test for a logistic regression model.
### Aliases: sm.binomial.bootstrap
### Keywords: nonparametric smooth htest models

### ** Examples

## Not run: sm.binomial.bootstrap(concentration, dead, N, 0.5, nboot=50)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("sm.binomial.bootstrap", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("sm.density")
### * sm.density

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: sm.density
### Title: Nonparametric density estimation in one, two or three
###   dimensions.
### Aliases: sm.density
### Keywords: nonparametric smooth

### ** Examples

#  A one-dimensional example
y <- rnorm(50)
sm.density(y, model = "Normal")
# sm.density(y, panel = TRUE)

#  A two-dimensional example
y <- cbind(rnorm(50), rnorm(50))
sm.density(y, display = "image")
# sm.density(y, panel = TRUE)


#  A three-dimensional example
# y <- cbind(rnorm(50), rnorm(50), rnorm(50))
# sm.density(y)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("sm.density", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("sm.density.compare")
### * sm.density.compare

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: sm.density.compare
### Title: Comparison of univariate density estimates
### Aliases: sm.density.compare
### Keywords: nonparametric smooth

### ** Examples

y <- rnorm(100)
g <- rep(1:2, rep(50,2))
sm.density.compare(y, g)

comp <- sm.density.compare(y, g, model = "equal")
legend("topleft", comp$levels, col = comp$col, lty = comp$lty, lwd = comp$lwd)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("sm.density.compare", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("sm.discontinuity")
### * sm.discontinuity

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: sm.discontinuity
### Title: The detection of discontinuities in a regression curve or
###   surface.
### Aliases: sm.discontinuity
### Keywords: smooth regression

### ** Examples

par(mfrow = c(3, 2))

with(nile, {
   sm.discontinuity(Year, Volume, hd = 0)
   sm.discontinuity(Year, Volume)

   ind <- (Year > 1898)
   plot(Year, Volume)
   h <- h.select(Year, Volume)
   sm.regression(Year[!ind], Volume[!ind], h, add = TRUE)
   sm.regression(Year[ ind], Volume[ ind], h, add = TRUE)

   hvec <- 1:15
   p <- numeric(0)
   for (h in hvec) {
      result <- sm.discontinuity(Year, Volume, h,
                          display = "none", verbose = 0)
      p <- c(p, result$p)
   }
   plot(hvec, p, type = "l", ylim = c(0, max(p)), xlab = "h")
   lines(range(hvec), c(0.05, 0.05), lty = 2)
})

with(trawl, {
   Position  <- cbind(Longitude, Latitude)
   ind <- (Longitude < 143.8)
   # Remove a repeated point which causes difficulty with sm.discontinuity
   ind[54] <- FALSE
   sm.regression(Position[ind,], Score1[ind], theta = 35, phi = 30)
   sm.discontinuity(Position[ind,], Score1[ind], col = "blue")
})
par(mfrow = c(1, 1))
	
#  The following example takes longer to run.
#  Alternative values for nside are 32 and 64.
#  Alternative values of yjump are 1 and 0.5.
# nside  <- 16
# yjump  <- 2
# x1     <- seq(0, 1, length = nside)
# x2     <- seq(0, 1, length = nside)
# x      <- expand.grid(x1, x2)
# x      <- cbind(x1 = x[, 1], x2 = x[, 2])
# y      <- rnorm(nside * nside)
# ind    <- (sqrt((x[, 1] - 0.5)^2 + (x[, 2] - 0.5)^2) <= 0.25)
# y[ind] <- y[ind] + yjump
# image(x1, x2, matrix(y, ncol = nside))
# sm.discontinuity(x, y, df = 20, add = TRUE)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("sm.discontinuity", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("sm.monotonicity")
### * sm.monotonicity

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: sm.monotonicity
### Title: A test of monotonicity in a regression curve.
### Aliases: sm.monotonicity
### Keywords: smooth regression

### ** Examples

	## Not run: 
##D #     Radiocarbon dating data
##D 
##D with(radioc, {
##D    ind     <- (Cal.age>5000 & Cal.age<6000)
##D    cal.age <- Cal.age[ind]
##D    rc.age  <- Rc.age[ind]
##D    sm.monotonicity(cal.age, rc.age, method = "aicc", nboot = 200)
##D })
##D 
##D #     Hosmer & Lemeshow birth data
##D 
##D with(birth, {
##D    sm.monotonicity(Lwt[Smoke == "N"], Low[Smoke == "N"],
##D           type = "binomial")
##D })
## End(Not run)


base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("sm.monotonicity", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("sm.options")
### * sm.options

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: sm.options
### Title: Set or return options of sm library
### Aliases: sm.options
### Keywords: nonparametric smooth

### ** Examples

## Not run: 
##D sm.options(poly.index = 0)
##D # subsequent regression estimations will be performed using local means
##D # instead of local regression
##D #
##D sm.options(describe = FALSE)  
##D # turns off typing documentation files of data loaded by `sm.script'
##D # (works from command-line)
##D # 
## End(Not run)


base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("sm.options", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("sm.pca")
### * sm.pca

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: sm.pca
### Title: Smooth principal components analysis
### Aliases: sm.pca
### Keywords: nonparametric smooth

### ** Examples

## Not run: 
##D Y    <- log(as.matrix(aircraft[ , -(1:2)]))
##D year <- aircraft$Yr
##D h    <- h.select(year, Y[ , 1], method = "df", df = 4)
##D spca <- sm.pca(year, Y, h, display = "none")
##D sm.pca(year, Y, h, display = "eigenvalues")
##D sm.pca(year, Y, h, display = "eigenvectors", ylim = c(-1, 1))
##D 
##D # The following code shows how the plots can be redrawn from the returned object
##D 
##D spca <- sm.pca(year, Y, h, display = "eigenvalues")
##D spca <- sm.pca(year, Y, h, display = "eigenvectors", ylim = c(-1, 1))
##D 
##D with(spca, {
##D    ylim <- range(evals[ , 1], band)
##D    plot(xgrid, evals[ , 1], type = "n", ylab = "Variance", ylim = ylim)
##D    polygon(c(xgrid, rev(xgrid)), c(band[ , 1], rev(band[ , 2])),
##D            col = "lightgreen", border = NA)
##D    lines(xgrid, evals[ , 1], col = "red")
##D })
##D 
##D with(spca, {
##D    pc <- 1
##D    plot(range(xgrid.plot), range(evecs.plot), type = "n",
##D         xlab = "x", ylab = "PC loadings")
##D    for (i in 1:ncol(Y))
##D       segments(xgrid.plot[-length(xgrid.plot)],
##D                evecs.plot[-nrow(evecs.plot), i],
##D                xgrid.plot[-1], evecs.plot[-1, i],
##D                col = col.plot[ , i], lty = i)
##D })
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("sm.pca", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("sm.poisson")
### * sm.poisson

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: sm.poisson
### Title: Nonparametric Poisson regression
### Aliases: sm.poisson
### Keywords: nonparametric smooth

### ** Examples

with(muscle, {
   TypeI <- TypeI.R+ TypeI.P+TypeI.B
   sm.poisson(x=log(TypeI), y=TypeII, h=0.25,display="se")
   sm.poisson(x=log(TypeI), y=TypeII, h=0.75, col=2, add=TRUE)
})



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("sm.poisson", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("sm.poisson.bootstrap")
### * sm.poisson.bootstrap

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: sm.poisson.bootstrap
### Title: Bootstrap goodness-of-fit test for a Poisson regression model
### Aliases: sm.poisson.bootstrap
### Keywords: nonparametric smooth htest models

### ** Examples

## takes a while: extend sm.script(muscle)
with(muscle, {
   TypeI <- TypeI.P + TypeI.R + TypeI.B
   sm.poisson.bootstrap(log(TypeI), TypeII, h = 0.5)
})



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("sm.poisson.bootstrap", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("sm.regression")
### * sm.regression

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: sm.regression
### Title: Nonparametric regression with one or two covariates.
### Aliases: sm.regression
### Keywords: nonparametric regression smooth

### ** Examples

with(trawl, {
   Zone92   <- (Year == 0 & Zone == 1)
   Position <- cbind(Longitude - 143, Latitude)
   dimnames(Position)[[2]][1] <- "Longitude - 143"

   par(mfrow = c(2, 2))
   sm.regression(Longitude, Score1, method = "aicc", col = "red",
       model = "linear")
   sm.regression(Position[Zone92, ], Score1[Zone92], display = "image", 
       theta = 120)
   sm.regression(Position[Zone92, ], Score1[Zone92], df = 12, col = "se",
       theta = 120)
   sm.regression(Position[Zone92, ], Score1[Zone92], df = 12, col = "se", 
       model = "linear", theta = 120)
   par(mfrow = c(1, 1))
})

# sm.regression(Position[Zone92, 2:1], Score1[Zone92], display = "rgl", df = 12)
# sm.regression(Position[Zone92, 2:1], Score1[Zone92], display = "rgl", df = 12,
#       alpha = c(0.9, 1), col = "se", model = "linear")

# sm.regression(Position[Zone92, 1], Score1[Zone92], panel = TRUE)
# sm.regression(Position[Zone92,  ], Score1[Zone92], panel = TRUE)
# sm.regression(Position[Zone92,  ], Score1[Zone92], panel = TRUE, display = "rgl")




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("sm.regression", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("sm.rm")
### * sm.rm

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: sm.rm
### Title: Nonparametric analysis of repeated measurements data
### Aliases: sm.rm
### Keywords: nonparametric smooth

### ** Examples

sm.rm(y=as.matrix(citrate), display.rice=TRUE)
#
with(dogs, {
   Time <- seq(1,13,by=2)
   gr1  <- as.matrix(dogs[dogs$Group==1,2:8])
   plot(c(1,13), c(3,6),xlab="time", ylab="potassium", type="n") 
   sm1  <- sm.rm(Time, gr1, display="se", add=TRUE)
})



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("sm.rm", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("sm.script")
### * sm.script

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: sm.script
### Title: Running a script associated to the sm library
### Aliases: sm.script
### Keywords: utilities

### ** Examples

sm.script()
sm.script(speed)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("sm.script", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("sm.sigma")
### * sm.sigma

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: sm.sigma
### Title: Estimation of the error standard deviation in nonparametric
###   regression.
### Aliases: sm.sigma
### Keywords: nonparametric smooth

### ** Examples

## Not run: 
##D with(airquality, {
##D    x     <- cbind(Wind, Temp)
##D    y     <- Ozone^(1/3)
##D    group <- (Solar.R < 200)
##D    sig1 <- sm.sigma(x[ group, ], y[ group], ci = TRUE)
##D    sig2 <- sm.sigma(x[!group, ], y[!group], ci = TRUE)
##D    print(c(sig1$estimate, sig1$ci))
##D    print(c(sig2$estimate, sig2$ci))
##D    print(sm.sigma(x[ group, ], y[ group], model = "constant", h = c(3, 5))$p)
##D    print(sm.sigma(x[!group, ], y[!group], model = "constant", h = c(3, 5))$p)
##D    print(sm.sigma2.compare(x[group, ], y[group], x[!group, ], y[!group]))
##D })
## End(Not run)


base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("sm.sigma", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("sm.sigma2.compare")
### * sm.sigma2.compare

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: sm.sigma2.compare
### Title: Comparison across two groups of the error standard deviation in
###   nonparametric regression with two covariates.
### Aliases: sm.sigma2.compare
### Keywords: nonparametric smooth

### ** Examples

## Not run: 
##D with(airquality, {
##D    x     <- cbind(Wind, Temp)
##D    y     <- Ozone^(1/3)
##D    group <- (Solar.R < 200)
##D    sig1 <- sm.sigma(x[ group, ], y[ group], ci = TRUE)
##D    sig2 <- sm.sigma(x[!group, ], y[!group], ci = TRUE)
##D    print(c(sig1$estimate, sig1$ci))
##D    print(c(sig2$estimate, sig2$ci))
##D    print(sm.sigma(x[ group, ], y[ group], model = "constant", h = c(3, 5))$p)
##D    print(sm.sigma(x[!group, ], y[!group], model = "constant", h = c(3, 5))$p)
##D    print(sm.sigma2.compare(x[group, ], y[group], x[!group, ], y[!group]))
##D })
## End(Not run)


base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("sm.sigma2.compare", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("sm.sphere")
### * sm.sphere

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: sm.sphere
### Title: Nonparametric density estimation for spherical data.
### Aliases: sm.sphere
### Keywords: nonparametric smooth

### ** Examples

lat  <- rnorm(50, 10, 15)
long <- c(rnorm(25, 300, 15), rnorm(25, 240, 15))
par(mfrow=c(1,2))
sm.sphere(lat, long)
sm.sphere(lat, long, sphim=TRUE, kappa=15)
par(mfrow=c(1,1))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("sm.sphere", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("sm.surface3d")
### * sm.surface3d

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: sm.surface3d
### Title: Adding a regression surface to an rgl plot.
### Aliases: sm.surface3d
### Keywords: nonparametric regression smooth

### ** Examples

with(trawl, {
   Zone93    <- (Year == 1 & Zone == 1)
   Position  <- cbind(Longitude - 143, Latitude)
   model1 <- sm.regression(Position[Zone93,], Score1[Zone93],
        h= c(0.1, 0.1), display = "rgl", xlab="Longitude - 143")
   model2 <- sm.regression(Position[Zone93,], Score1[Zone93],
        h= c(0.2, 0.2), display = "none")
   sm.surface3d(model2$eval.points, model2$est, model1$scaling, col = "red")
})



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("sm.surface3d", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("sm.survival")
### * sm.survival

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: sm.survival
### Title: Nonparametric regression with survival data.
### Aliases: sm.survival
### Keywords: nonparametric smooth survival

### ** Examples

x <- runif(50, 0, 10)
y <- rexp(50, 2)
z <- rexp(50, 1)
status <- rep(1, 50)
status[z<y] <- 0
y <- pmin(z, y)
sm.survival(x, y, status, h=2)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("sm.survival", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("sm.ts.pdf")
### * sm.ts.pdf

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: sm.ts.pdf
### Title: Nonparametric density estimation of stationary time series data
### Aliases: sm.ts.pdf
### Keywords: nonparametric smooth ts

### ** Examples

with(geyser, {
   sm.ts.pdf(geyser$duration, lags=1:2)
})



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("sm.ts.pdf", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("sm.variogram")
### * sm.variogram

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: sm.variogram
### Title: Confidence intervals and tests based on smoothing an empirical
###   variogram.
### Aliases: sm.variogram
### Keywords: smooth regression spatial

### ** Examples


## Not run: 
##D with(coalash, {
##D    Position <- cbind(East, North)
##D    sm.options(list(df = 6, se = TRUE))
##D 
##D    par(mfrow=c(2,2))
##D    sm.variogram(Position, Percent, original.scale = FALSE, se = FALSE)
##D    sm.variogram(Position, Percent, original.scale = FALSE)
##D    sm.variogram(Position, Percent, original.scale = FALSE, model = "independent")
##D    sm.variogram(East,     Percent, original.scale = FALSE, model = "independent")
##D    par(mfrow=c(1,1))
##D })
##D 
##D # Comparison of Co in March and September
##D    
##D with(mosses, {
##D 	
##D    nbins <- 12
##D    vgm.m <- sm.variogram(loc.m, Co.m, nbins = nbins, original.scale = TRUE,
##D                         ylim = c(0, 1.5))
##D    vgm.s <- sm.variogram(loc.s, Co.s, nbins = nbins, original.scale = TRUE,
##D                         add = TRUE, col.points = "blue")
##D                         
##D    trns <- function(x) (x / 0.977741)^4
##D    del <- 1000
##D    plot(vgm.m$distance.mean, trns(vgm.m$sqrtdiff.mean), type = "b",
##D          ylim = c(0, 1.5), xlab = "Distance", ylab = "Semi-variogram")
##D    points(vgm.s$distance.mean - del, trns(vgm.s$sqrtdiff.mean), type = "b",
##D          col = "blue", pch = 2, lty = 2)
##D 
##D    plot(vgm.m$distance.mean, trns(vgm.m$sqrtdiff.mean), type = "b",
##D          ylim = c(0, 1.5), xlab = "Distance", ylab = "Semi-variogram")
##D    points(vgm.s$distance.mean - del, trns(vgm.s$sqrtdiff.mean), type = "b",
##D          col = "blue", pch = 2, lty = 2)
##D    segments(vgm.m$distance.mean, trns(vgm.m$sqrtdiff.mean - 2 * vgm.m$se),
##D          vgm.m$distance.mean, trns(vgm.m$sqrtdiff.mean + 2 * vgm.m$se))
##D    segments(vgm.s$distance.mean - del, trns(vgm.s$sqrtdiff.mean - 2 * vgm.s$se),
##D          vgm.s$distance.mean - del, trns(vgm.s$sqrtdiff.mean + 2 * vgm.s$se),
##D          col = "blue", lty = 2)
##D 
##D    mn <- (vgm.m$sqrtdiff.mean + vgm.s$sqrtdiff.mean) / 2
##D    se <- sqrt(vgm.m$se^2 + vgm.s$se^2)
##D    plot(vgm.m$distance.mean, trns(vgm.m$sqrtdiff.mean), type = "n",
##D         ylim = c(0, 1.5), xlab = "Distance", ylab = "Semi-variogram")
##D    polygon(c(vgm.m$distance.mean, rev(vgm.m$distance.mean)),
##D         c(trns(mn - se), rev(trns(mn + se))),
##D         border = NA, col = "lightblue")  
##D    points(vgm.m$distance.mean, trns(vgm.m$sqrtdiff.mean))
##D    points(vgm.s$distance.mean, trns(vgm.s$sqrtdiff.mean), col = "blue", pch = 2)
##D 
##D    vgm1 <- sm.variogram(loc.m, Co.m, nbins = nbins, varmat = TRUE, 
##D                         display = "none")
##D    vgm2 <- sm.variogram(loc.s, Co.s, nbins = nbins, varmat = TRUE,
##D                         display = "none")
##D 
##D    nbin  <- length(vgm1$distance.mean)
##D    vdiff <- vgm1$sqrtdiff.mean - vgm2$sqrtdiff.mean
##D    tstat <- c(vdiff %*% solve(vgm1$V + vgm2$V) %*% vdiff)
##D    pval  <- 1 - pchisq(tstat, nbin)
##D    print(pval)
##D })
##D 
##D # Assessing isotropy for Hg in March
##D 
##D with(mosses, {
##D    sm.variogram(loc.m, Hg.m, model = "isotropic")
##D })
##D 
##D # Assessing stationarity for Hg in September
##D 
##D with(mosses, {
##D    vgm.sty <- sm.variogram(loc.s, Hg.s, model = "stationary")
##D    i <- 1
##D    image(vgm.sty$eval.points[[1]], vgm.sty$eval.points[[2]], vgm.sty$estimate[ , , i],
##D          col = topo.colors(20))
##D    contour(vgm.sty$eval.points[[1]], vgm.sty$eval.points[[2]], vgm.sty$sdiff[ , , i],
##D          col = "red", add = TRUE)
##D })
##D 
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("sm.variogram", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
