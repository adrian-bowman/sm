"sm.options" <- function (...) {
    if (nargs() == 0) return(.sm.Options)
    current <- .sm.Options
    if (is.character(...))
        temp <- eval(parse(text = paste(c("list(", ..., ")"))))
    else temp <- list(...)
    if (length(temp) == 1 && is.null(names(temp))) {
        arg <- temp[[1]]
        switch(mode(arg),
               list = temp <- arg,
               character = return(.sm.Options[arg]),
               stop("invalid argument: ", sQuote(arg)))
    }
    if (length(temp) == 0) return(current)
    n <- names(temp)
    if (is.null(n)) stop("options must be given by name")
    changed <- current[n]
    current[n] <- temp
    if (sys.parent() == 0) env <- asNamespace("sm") else env <- parent.frame()
    assign(".sm.Options", current, envir = env)
    invisible(current)
}

"replace.na" <- function (List, comp, value) {
    arg <- paste(substitute(List), "$", substitute(comp), sep = "")
    arg.value <- eval(parse(text = arg), parent.frame(1))
    if (any(is.na(arg.value))) {
        change <- paste(arg, "<-", deparse(substitute(value)))
        a <- eval(parse(text = change), parent.frame(1))
        }
    invisible()
    }

"sm.check.data" <- function (x, y = NA, weights = NA, group = NA, ...) {
   opt <- sm.options(list(...))

   density <- all(is.na(y))
   if (density) X <- x
      else  X <- cbind(x, y)

   if(all(is.na(weights)) | all(weights == 1))
      X <- cbind(X, 1) 
   else{
      if(!is.na(opt$nbins) & opt$nbins!=0) 
         stop("if weights are set, nbins must be either 0 or NA")
      if(any(weights<0 | is.na(weights))) 
         stop("negative or NA weights are meaningless")
      if(any(weights!=round(weights))) {
         weights <- round(weights/min(weights[weights>0]))
         if(opt$verbose>0) 
            cat("Warning: weights have been rescaled to integer values\n")
         }
      X <- cbind(X, weights)
      }

   ndim <- ncol(X) - 1 - (!density)            # dimensionality of x
   if (!all(is.na(group))) {
      X <- cbind(X, group)
      group.col <- ncol(X)
      }
   if (!all(is.na(opt$h.weights))) {
      X <- cbind(X,opt$h.weights)
      hw.col <- ncol(X)
      } 
   if (any(is.na(X)) & opt$verbose > 0) cat("missing data are removed\n")
   X <- na.omit(data.matrix(X))
   if (ndim > 2 + density) stop("x has too many columns")
   weights <- as.vector(X[, ndim + (!density) + 1])
   if (!density) y <- as.vector(X[, ndim + 1])
   x <- if (ndim == 1) as.vector(X[, 1]) else X[, 1:ndim]
   if (!all(is.na(group))) group <- as.vector(X[, group.col])
   if (!all(is.na(opt$h.weights))) opt$h.weights <- X[, hw.col]
   list(x = x, y = y, weights = weights, group = group, ndim = ndim, 
        nobs = nrow(X), density = density, options = opt)
   }

".sm.Options" <-
    list(hmult = 1, h.weights = NA, period = NA,
         add = FALSE, band = NA, props = c(75, 50, 25), nbins = NA,
         positive = FALSE, delta = NA, display = NA, 
         xlab = NA, ylab = NA, zlab = NA, 
         xlim = NA, ylim = NA, zlim = NA, x1lim = NA, x2lim = NA,
         yht = NA, asp = NA,
         panel = FALSE, panel.plot = NA,
         ngrid = NA, eval.points = NA, rugplot = TRUE, 
         col = NA, col.band = "cyan", col.mesh = "black", 
         col.palette = NA, col.palette.fn = topo.colors, nlevels = 40,
         col.points = "black", include.mean = NA,
         se = NA, se.breaks = c(-3, -2, 2, 3), lty = 1, pch = 1,
         cex = NA, cex.axis = NA, cex.lab = NA, labcex = NA, key = TRUE,
         theta = -30, phi = 40, size = 2, scaling = NULL, 
         alpha = 0.7, alpha.mesh = 1, lit = FALSE,
         poly.index = 1, diff.ord = 2, test = NA, hull = TRUE, verbose = 1, 
         df = NA, df.max = NA, method = NA, structure.2d = "scaled", nboot = 100, 
         describe = TRUE, show.script = TRUE, eval.grid = TRUE, reference = NA,
         mask.method = "hull", partial.residuals = TRUE, hscale = 1, vscale = 1,
         include.lower.terms = FALSE, include.lower.reference = FALSE, lwd = 1,
         transform.response = I, eqscplot = FALSE, order = 1:3,
    		 deriv = NA, deriv.order = NA)
