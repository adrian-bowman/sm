library(sm)

?sm.variogram

catdat <- boot::catsM
plot(catdat$Bwt, y = catdat$Hwt)

install.packages("~/OneDrive - University of Glasgow/research/sm2.2-5.7-not-yet-released/sm",
                 repos = NULL, type = "source")
library(sm)
sm.variogram(x = catdat$Bwt, y = catdat$Hwt, model = "stationary")



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

## Not run: 
# Comparison of Co in March and September
   
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

# Assessing isotropy for Hg in March

with(mosses, {
   sm.variogram(loc.m, Hg.m, model = "isotropic")
})

# Assessing stationarity for Hg in September

with(mosses, {
   vgm.sty <- sm.variogram(loc.s, Hg.s, model = "stationary")
   i <- 1
   image(vgm.sty$eval.points[[1]], vgm.sty$eval.points[[2]], vgm.sty$estimate[ , , i],
         col = topo.colors(20))
   contour(vgm.sty$eval.points[[1]], vgm.sty$eval.points[[2]], vgm.sty$sdiff[ , , i],
         col = "red", add = TRUE)
})
