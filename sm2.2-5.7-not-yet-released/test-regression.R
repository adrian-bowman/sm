#     Test examples for the sm.regression function

detach(package:sm)
unloadNamespace("sm")
install.packages("~/ownCloud/sm2.2-5.7-not-yet-released/sm", repos = NULL, type = "source")
library(sm)

# Weights can now be non-integer
with(trawl, {
  n   <- length(Score1)
  wts <- rep(1, n)
  wts <- runif(n)
  sm.regression(Longitude, Score1, weights = wts, nbins = 0)
})

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


attach(trawl)
Zone92   <- (Year == 0 & Zone == 1)
Position <- cbind(Longitude - 143, Latitude)
dimnames(Position)[[2]][1] <- "Longitude - 143"

sm.regression(Position[Zone92, 2:1], Score1[Zone92], display = "rgl", df = 12)
sm.regression(Position[Zone92, 2:1], Score1[Zone92], display = "rgl", df = 12,
      alpha = c(0.9, 1), col = "se", model = "linear")

sm.regression(Position[Zone92, 1], Score1[Zone92], panel = TRUE)
sm.regression(Position[Zone92,  ], Score1[Zone92], panel = TRUE)
sm.regression(Position[Zone92,  ], Score1[Zone92], panel = TRUE, display = "rgl")
