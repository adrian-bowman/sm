par(mfrow = c(3, 2))

attach(nile)

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
   result <- sm.discontinuity(Year, Volume, h, display = "none", verbose = 0)
   p <- c(p, result$p)
   }
plot(hvec, p, type = "l", ylim = c(0, max(p)), xlab = "h")
lines(range(hvec), c(0.05, 0.05), lty = 2)

attach(trawl)
Position  <- cbind(Longitude, Latitude)
ind <- (Longitude < 143.8)
ind[54] <- FALSE
sm.regression(Position[ind,], Score1[ind], theta = 35, phi = 30)
sm.discontinuity(Position[ind,], Score1[ind], col = "blue")

par(mfrow = c(1, 1))

#  The following example takes longer to run.
#  Alternative values for nside are 32 and 64.
#  Alternative values of yjump are 1 and 0.5.
nside  <- 16
yjump  <- 2
x1     <- seq(0, 1, length = nside)
x2     <- seq(0, 1, length = nside)
x      <- expand.grid(x1, x2)
x      <- cbind(x1 = x[, 1], x2 = x[, 2])
y      <- rnorm(nside * nside)
ind    <- (sqrt((x[, 1] - 0.5)^2 + (x[, 2] - 0.5)^2) <= 0.25)
y[ind] <- y[ind] + yjump
image(x1, x2, matrix(y, ncol = nside))
a <- sm.discontinuity(x, y, df = 20, add = TRUE)

plot(unique(x[,1]), unique(x[,2]), type = "n", xlab = "x1", ylab = "x2")
contour(a$eval.points[,1], a$eval.points[,2], a$st.diff,
        xlab = "x1", ylab = "x2", add = T, lty = 2, col = 2, labcex = 1)
# lines(dis[,1], dis[,2], lty = 2, lwd = 3, col=4)
lines(a$ridge.curve[, 1], a$ridge.curve[, 2])
