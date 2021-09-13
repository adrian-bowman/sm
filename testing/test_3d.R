
with(trawl, {
Zone93    <- (Year == 0 & Zone == 1)
Position  <- cbind(Longitude - 143, Latitude)

sm.ancova(Position[,1], Score1, Zone, model = "parallel", band = FALSE)
temp <- sm.regression(Position[,1], Score1, model = "no effect", panel = TRUE)
Position[1,1] <- NA
Score1[1] <- NA
sm.ancova(Position[,1], Score1, Zone, model = "parallel", band = FALSE)
h.select(Position[,1], Score1)

sm.regression(Position[,1], Score1, panel = TRUE)
sm.regression(Position, Score1, display = "rgl", panel = TRUE)
sm.regression(Position, Score1, model = "no effect", panel = TRUE, display = "image")

model1 <- sm.regression(Position[Zone93, ], Score1[Zone93], 
     # col = "se",
     band = TRUE,
     model = "linear", 
     test = FALSE,
     # se.breaks = c(-2, 2),
     h = c(0.1, 0.1), xlab = "Longitude - 143", theta = 120)
     
model1 <- sm.regression(Position[Zone93,], Score1[Zone93], 
     display = "rgl", 
     col = "se",
     col.points = "blue",
     # model = "linear",
     # se.breaks = c(-2, 2),
     # ylim = c(-1, 3),
     # col.palette = topo.colors(12),
     # col.mesh = "height",
     alpha = 0.2, 
     # alpha.mesh = 1, 
     # lit = TRUE,
     # panel = TRUE,
     h = c(0.1, 0.1), xlab = "Longitude - 143")
     
model2 <- sm.regression(Position[Zone93,], Score1[Zone93],
     h= c(0.2, 0.2), display = "none")
sm.surface3d(model2$eval.points, model2$est, model1$scaling, col = "red")

Zone92 <- (Year == 0 & Zone == 1)
model <- sm.regression(Position[Zone93, 2:1], Score1[Zone93], 
     h= c(0.1, 0.1), display = "rgl", zlab="Longitude - 143", ylim = c(-1, 2),
     col = c("green", "black"), col.points = "red", ngrid = 50, alpha = c(1, 0))
sm.regression(Position[Zone92, 2:1], Score1[Zone92], scaling = model$scaling,
     display = "rgl", zlab="Longitude - 143", add = TRUE,
     h= c(0.1, 0.1), col = c("red", "black"), ngrid = 50, alpha = c(1, 0))

n <- 1000
x <- cbind(rnorm(n), rnorm(n))
y <- rnorm(n)
sm.regression(x, y, display = "none")$sigma
sm.sigma(x, y)$estimate

})

data(airquality)
with(airquality, {
   
group <- (Solar.R <= 200)
x     <- cbind(Temp, Wind)
Ozone <- Ozone^(1/3)

h     <- c(7.1, 2.7)
h <- h.select(x, Ozone)

model <- sm.regression(x[!group,], Ozone[!group], display = "rgl")
sm.regression(x[group,], Ozone[group], display = "persp", theta = -90)

sm.regression(x[group, 1], Ozone[group], 
   se = TRUE,
   col.points = "green", col = "blue")

})

y <- cbind(rnorm(50), rnorm(50), rnorm(50))
y <- cbind(rnorm(50), rnorm(50))
y <- rnorm(50)

sm.density(y, panel = TRUE)

model <- sm.density(geys3d[, 3:1], col.points = "red", ngrid = 50)

n <- 200
y <- cbind(rnorm(n), rnorm(n))
y <- cbind(rnorm(n), rnorm(n), rnorm(n))

sm.density(y, display = "rgl", panel = TRUE)

while (rgl::rgl.cur() > 0) rgl::rgl.close()
