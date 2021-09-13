#     Checks on the sm.variogram function

library(sm)
if (reinstall) devtools::install("sm")

test_label("Measurements at identical locations", test.prompt)
with(coalash, {
   Position <- cbind(East, North)
   position <- rbind(Position[1, ], Position)
   percent  <- c(rnorm(1, mean = 10, sd = 0.1), Percent)
   sm.options(list(df = 6, se = TRUE))
   par(mfrow = c(1, 2))
   sm.variogram(position, percent)
   title("Identical location added")
   sm.variogram(position, percent, n.zero.dist = 6)
   title("Zero bin amalgamated")
   par(mfrow = c(1, 1))
})

test_label("Errors for isotropy or stationarity test with 1-d data", test.prompt)
try(with(coalash, sm.variogram(East, Percent, model = "isotropic")))
try(with(coalash, sm.variogram(East, Percent, model = "stationary")))

test_label("Errors for inappropriate arguments", test.prompt)
with(coalash, sm.variogram(East, Percent, model = "something"))

test_label("Test of constant variance", test.prompt)
catsM   <- boot::catsM
model   <- lm(Hwt ~ Bwt, data = catsM)
res     <- residuals(model)
X       <- cbind(1, catsM$Bwt)
H       <- diag(length(catsM$Bwt)) - X %*% solve(t(X) %*% X) %*% t(X)
sdres   <- sqrt(diag(H)) * summary(model)$sigma
tres    <- sqrt(abs(res))
tres.mn <- 2^{0.25} * gamma(0.75) * sqrt(sdres) / sqrt(pi)
tres.sd <- 2^{0.25} * sqrt(sqrt(pi) - gamma(0.75)^2) * sqrt(sdres) / sqrt(pi)
h       <- diff(range(catsM$Bwt)) / 8
sm.regression(catsM$Bwt, (tres - tres.mn) / tres.sd, model = "no effect", h = h, poly.index = 0)

test_label("Coalash data", test.prompt)
with(coalash, {
   Position <- cbind(East, North)
   sm.options(list(df = 6, se = TRUE))

   par(mfrow=c(2,2))
   sm.variogram(Position, Percent, original.scale = FALSE, se = FALSE)
   sm.variogram(Position, Percent, original.scale = FALSE)
   sm.variogram(Position, Percent, original.scale = FALSE, model = "independent")
   sm.variogram(East,     Percent, original.scale = FALSE, model = "independent")
   par(mfrow=c(1,1))
})

test_label("Comparison of Co in March and September", test.prompt)
with(mosses, {
	
   nbins <- 12
   vgm.m <- sm.variogram(loc.m, Co.m, nbins = nbins, original.scale = TRUE,
                        ylim = c(0, 1.5))
   vgm.s <- sm.variogram(loc.s, Co.s, nbins = nbins, original.scale = TRUE,
                        add = TRUE, col.points = "blue")
                        
   trns <- function(x) (x / 0.977741)^4
   del <- 1000
   plot(vgm.m$distance.mean, trns(vgm.m$sqrtdiff.mean), type = "b",
         ylim = c(0, 1.5), xlab = "Distance", ylab = "Semi-variogram")
   points(vgm.s$distance.mean - del, trns(vgm.s$sqrtdiff.mean), type = "b",
         col = "blue", pch = 2, lty = 2)

   plot(vgm.m$distance.mean, trns(vgm.m$sqrtdiff.mean), type = "b",
         ylim = c(0, 1.5), xlab = "Distance", ylab = "Semi-variogram")
   points(vgm.s$distance.mean - del, trns(vgm.s$sqrtdiff.mean), type = "b",
         col = "blue", pch = 2, lty = 2)
   segments(vgm.m$distance.mean, trns(vgm.m$sqrtdiff.mean - 2 * vgm.m$se),
         vgm.m$distance.mean, trns(vgm.m$sqrtdiff.mean + 2 * vgm.m$se))
   segments(vgm.s$distance.mean - del, trns(vgm.s$sqrtdiff.mean - 2 * vgm.s$se),
         vgm.s$distance.mean - del, trns(vgm.s$sqrtdiff.mean + 2 * vgm.s$se),
         col = "blue", lty = 2)

   mn <- (vgm.m$sqrtdiff.mean + vgm.s$sqrtdiff.mean) / 2
   se <- sqrt(vgm.m$se^2 + vgm.s$se^2)
   plot(vgm.m$distance.mean, trns(vgm.m$sqrtdiff.mean), type = "n",
        ylim = c(0, 1.5), xlab = "Distance", ylab = "Semi-variogram")
   polygon(c(vgm.m$distance.mean, rev(vgm.m$distance.mean)),
        c(trns(mn - se), rev(trns(mn + se))),
        border = NA, col = "lightblue")  
   points(vgm.m$distance.mean, trns(vgm.m$sqrtdiff.mean))
   points(vgm.s$distance.mean, trns(vgm.s$sqrtdiff.mean), col = "blue", pch = 2)

   vgm1 <- sm.variogram(loc.m, Co.m, nbins = nbins, varmat = TRUE, 
                        display = "none")
   vgm2 <- sm.variogram(loc.s, Co.s, nbins = nbins, varmat = TRUE,
                        display = "none")

   nbin  <- length(vgm1$distance.mean)
   vdiff <- vgm1$sqrtdiff.mean - vgm2$sqrtdiff.mean
   tstat <- c(vdiff %*% solve(vgm1$V + vgm2$V) %*% vdiff)
   pval  <- 1 - pchisq(tstat, nbin)
   pval
})

test_label("Isotropy", test.prompt)
with(mosses, {
   sm.variogram(loc.m, Hg.m, model = "isotropic")
   title("Hg in March")
   sm.variogram(rbind(loc.m[1, ], loc.m), c(4, Hg.m), model = "isotropic")
   title("Repeat observation")
})

test_label("Stationarity", test.prompt)
with(mosses, {
   vgm.sty   <- sm.variogram(loc.s, Hg.s, model = "stationary")
   vgm.sty.r <- sm.variogram(rbind(loc.s[1, ], loc.s), c(4.5, Hg.s), model = "stationary")
   i <- 1
   par(mfrow = c(1, 2))
   image(vgm.sty$eval.points[[1]], vgm.sty$eval.points[[2]], vgm.sty$estimate[ , , i],
         col = topo.colors(20))
   contour(vgm.sty$eval.points[[1]], vgm.sty$eval.points[[2]], vgm.sty$sdiff[ , , i],
           col = "red", add = TRUE)
   title("Hg in September: p =", round(vgm.sty$p, 2))
   image(vgm.sty.r$eval.points[[1]], vgm.sty.r$eval.points[[2]], vgm.sty.r$estimate[ , , i],
         col = topo.colors(20))
   contour(vgm.sty.r$eval.points[[1]], vgm.sty.r$eval.points[[2]], vgm.sty.r$sdiff[ , , i],
           col = "red", add = TRUE)
   title("Repeat observation: p =", round(vgm.sty.r$p, 2))
   par(mfrow = c(1, 1))
})
