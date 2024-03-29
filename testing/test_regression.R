#     Test examples for the sm.regression function

library(sm)
if (reinstall) devtools::install("sm")

# Weights can now be non-integer
with(trawl, {
  n   <- length(Score1)
  # wts <- rep(1, n)
  wts <- runif(n)
  sm.regression(Longitude, Score1, weights = wts, nbins = 0)
})

Zone92   <- (trawl$Year == 0 & trawl$Zone == 1)
Position <- cbind(trawl$Longitude - 143, trawl$Latitude)
dimnames(Position)[[2]] <- c("Longitude - 143", "Latitude")

par(mfrow = c(2, 2))
sm.regression(trawl$Longitude, trawl$Score1, method = "aicc", col = "red", model = "linear")
sm.regression(Position[Zone92, ], trawl$Score1[Zone92], display = "image", theta = 120)
sm.regression(Position[Zone92, ], trawl$Score1[Zone92], df = 12, col = "se", theta = 120)
sm.regression(Position[Zone92, ], trawl$Score1[Zone92], df = 12, col = "se", model = "linear", theta = 120)
par(mfrow = c(1, 1))

sm.regression(Position[Zone92, 2:1], trawl$Score1[Zone92], display = "rgl", df = 12)
sm.regression(Position[Zone92, 2:1], trawl$Score1[Zone92], display = "rgl", df = 12,
      alpha = c(0.9, 1), col = "se", model = "linear")

sm.regression(Position[Zone92, 1], trawl$Score1[Zone92], panel = TRUE)
sm.regression(Position[Zone92,  ], trawl$Score1[Zone92], panel = TRUE)
sm.regression(Position[Zone92,  ], trawl$Score1[Zone92], panel = TRUE, display = "rgl")

while (rgl::rgl.cur() > 0) rgl::rgl.close()
