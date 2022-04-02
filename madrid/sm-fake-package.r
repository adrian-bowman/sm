source("~/research/madrid/sm.R")
source("~/research/madrid/sm-pam-utilities.R")


fake.First.lib <- function() {
    # library.dynam("sm", pkg, library)
    # assign(".sm.home", file.path(library, pkg),
    #       pos=match("package:sm", search()))  
  ".sm.Options" <- assign(".sm.Options",
    list(hmult = 1, h.weights = NA, 
         add = FALSE, band = TRUE, props = c(75,50,25), nbins = NA,
         positive = FALSE, delta = NA, display = NA, 
         xlab = NA, ylab = NA, zlab = NA, 
         xlim = NA, ylim = NA, zlim = NA, x1lim = NA, x2lim = NA,
         yht = NA, asp = NA,
         cex = NA, cex.axis = NA, cex.lab = NA, labcex = NA, key = TRUE,
         panel = FALSE, panel.plot = NA, ngrid = NA, eval.points = NA,
         rugplot = TRUE,  col.palette = NA, 
         col.palette.fn = topo.colors, nlevels = 40,
         col = 1, col.points = "black", lty = 1, pch = 1, se = NA,
         theta = -30, phi = 40, size = 3, scaling = NULL, alpha = 0.5,
         poly.index = 1, diff.ord = 2, test = TRUE, hull = TRUE, verbose = 1, 
         df = 4, df.max = NA, method = NA, structure.2d = "scaled", nboot = 100, 
         describe = TRUE, show.script = TRUE, eval.grid = TRUE, reference = NA,
         mask.method = "hull", partial.residuals = TRUE, include.mean = NA,
         include.lower.terms = FALSE, include.lower.reference = FALSE, lwd = 1,
         transform.response = I, hscale = 1, vscale = 1, eqscplot = FALSE, order = 1:3,
    		deriv = NA, deriv.order = NA,
         pos=match(".GlobalEnv", search()) ))
  # cat("faking 'sm-pam', ...\n")
   invisible()
   }

fake.First.lib()
# sm.options(describe = FALSE)
