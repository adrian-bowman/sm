setwd("/Volumes/adrian/research/sm/sm2.2-5-not-yet-released/sm")

files <- list.files("inst/smdata", ".dat")
for (fnm in files) {
  d <- read.table(paste("inst/smdata", fnm, sep = "/"), header = TRUE)
  fn <- substr(fnm, 1, nchar(fnm) - 4)
  assign(fn, d)
  save(list = fn, file = paste("data/", fn, ".rda", sep = ""))
  print(fn)
}
