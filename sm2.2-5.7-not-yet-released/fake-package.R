wd <- getwd()
setwd("~/Desktop/sm/sm2.2-5.4-not-yet-released/sm/R")
temp <- system("ls", intern = TRUE)
for (i in temp) {
	nc <- nchar(i)
	ss <- substr(i, nc-1, nc)
	if ((ss == ".r") | (ss == ".R")) source(i)
	}
setwd(wd)

fake.First.lib <- function() {
    # library.dynam("sm", pkg, library)
    # assign(".sm.home", file.path(library, pkg),
    #       pos=match("package:sm", search()))  
  ".sm.Options" <- assign(".sm.Options",
    list(hmult = 1, h.weights = NA, 
         add = FALSE, band = TRUE, props = c(75,50,25), nbins = NA,
         positive = FALSE, delta = NA, display = NA, 
         xlab = NA, ylab = NA, zlab = NA, 
         xlim = NA, ylim = NA, zlim = NA, yht = NA,
         model = "none", reference = "none",
         panel = FALSE, ngrid = NA, eval.points = NA, rugplot = TRUE, 
         col.palette = topo.colors(12), col.palette.fn = topo.colors, 
         superimpose = NA,
         col = 1, col.points = "black", lty = 1, pch = 1,
         theta = -30, phi = 40, size = 3, scaling = NULL, alpha = 0.5,
         poly.index = 1, diff.ord = 2, test = TRUE, hull = TRUE, verbose = 1, 
         df = 4, method = NA, structure.2d = "scaled", nboot = 100, 
         describe = TRUE, show.script = TRUE, eval.grid = TRUE, mask.method = "hull",
         partial.residuals = TRUE, nlevels = 20,
        pos=match(".GlobalEnv", search()) ))
   cat("faking library 'sm', version 2.2-5 ...\n")
   invisible()
   }

fake.First.lib()
