with(radioc, {
   ind     <- (Cal.age>5000 & Cal.age<6000)
   cal.age <- Cal.age[ind]
   rc.age  <- Rc.age[ind]
   h <- h.select(cal.age, rc.age, method = "aicc")
   h <- 38
   sm.regression(cal.age, rc.age, h)

   sm.monotonicity(cal.age, rc.age, h = 38)
   sm.monotonicity(cal.age, rc.age)
})

#     Hosmer & Lemeshow birth data

with(birth, {
   Low1 <- Low[Smoke == "S"]
   Lwt1 <- Lwt[Smoke == "S"]
   sm.binomial(Lwt1, Low1, h = 20)
   sm.monotonicity(Lwt1, Low1, h = 20, type = "binomial")
})

n <- 30
m <- 500
x <- seq(-0.3, 0.7, length = n)
y <- rbinom(n, m, 0.2 + x^2)
N <- rep(m, n)
plot(y / N ~ x, ylim = c(0, 1))

sm.monotonicity(x, y, N, h = 0.1, type = "binomial", ylim = c(0, 1))

sm.binomial(x, y, N, 0.35)

a <- 0.3
n <- 30
x <- seq(0.1, 0.9, length = n)
g <- x - a * exp(-0.5 * (x - 0.5)^2 / 0.1^2)
plot(x, g)

m <- 50
N <- rep(m, n)
y <- rbinom(n, m, g)
plot(x, y / N)
sm.monotonicity(x, y, N, h = 0.1, type = "binomial", ylim = c(0, 1))

sm.binomial(x, y, N, 0.086)
