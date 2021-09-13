
sm.density(rnorm(50), panel = TRUE, hscale = 1.5, vscale = 1.25)

attach(trawl)
Zone93    <- (Year == 0 & Zone == 1)
Position  <- cbind(Longitude - 143, Latitude)

sm.regression(Longitude, Score1, panel = TRUE,
     hscale = 1.5, vscale = 1.25)

sm.regression(Position, Score1, panel = TRUE)
sm.regression(Position, Score1, panel = TRUE, display = "rgl")

y <- cbind(rnorm(50), rnorm(50))
y <- cbind(rnorm(50), rnorm(50), rnorm(50))

sm.density(y, panel = TRUE)

sm.regression(Longitude, Score1, panel = TRUE)

data(airquality)
attach(airquality)
group <- (Solar.R <= 200)
x     <- cbind(Temp, Wind)
Ozone <- Ozone^(1/3)

h     <- c(7.1, 2.7)
h     <- h.select(x, Ozone)

model <- sm.regression(x[!group,], Ozone[!group], display = "rgl")
sm.regression(x[group,], Ozone[group], display = "persp", theta = -90)

sm.regression(x[group, 1], Ozone[group], 
   se = TRUE,
   col.points = "green", col = "blue")

y <- cbind(rnorm(50), rnorm(50))

sm.density(y, panel = TRUE)
sm.density(y, panel = TRUE, display = "persp")
