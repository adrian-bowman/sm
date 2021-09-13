#     Checks on the h.select function

library(sm)
if (reinstall) devtools::install("sm")
cat("\n")

cat("** Density estimation (1d) ...")
if (test.prompt) readline(prompt = "  Press [enter] to continue") else cat("\n\n")
x <- rnorm(50)
cat("default", h.select(x), "\n")
cat("sj", h.select(x, method = "sj"), "\n")
cat("cv", h.select(x, method = "cv"), "\n")
cat("\n")

cat("** Density estimation (2d) ...")
if (test.prompt) readline(prompt = "  Press [enter] to continue") else cat("\n\n")
x <- matrix(rnorm(100), ncol = 2)
for (structure in c("scaled", "separate", "common")) {
   for (mthd in c("normal", "cv")) {
      h <- h.select(x, structure.2d = structure, method = mthd)
      cat(structure, mthd, h, "\n")
   }
}
sm.density(x, method = "cv")
cat("\n")

cat("** Flexible regression (1d) ...")
if (test.prompt) readline(prompt = "  Press [enter] to continue") else cat("\n\n")
x <- rnorm(50)
y <- x^2 + rnorm(50)
for (mthd in c("df", "cv", "aicc")) {
      h <- h.select(x, y, structure.2d = structure, method = mthd)
      cat(mthd, h, "\n")
}
for (edf in c(3, 6, 12)) 
   cat("df", edf, h.select(x, y, df = edf), "\n")
sm.regression(x, y, method = "aicc")
cat("\n")

cat("** Flexible regression (2d) ...")
if (test.prompt) readline(prompt = "  Press [enter] to continue") else cat("\n\n")
x <- matrix(rnorm(100), ncol = 2)
y <- x[,1]^2 + x[,2]^2 + rnorm(50)
for (structure in c("scaled", "separate", "common")) {
   for (mthd in c("df", "cv", "aicc")) {
      h <- h.select(x, y, structure.2d = structure, method = mthd)
      cat(structure, mthd, h, "\n")
   }
}
sm.regression(x, y, df = 8)
cat("\n")
