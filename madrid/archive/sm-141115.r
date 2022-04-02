sm <- function(x, y, weights, bdeg = 3, pord = 2, h, model, ...
                    # increasing = FALSE, decreasing = FALSE, kappa = lambda * 100,
                           ) {

   weights.missing <- missing(weights)

   if (!missing(y)) {
      x.name <- deparse(substitute(x))
      y.name <- deparse(substitute(y))
      if (weights.missing) weights <- NA
      if (missing(model)) model <- "none"
      return(sm.regression(x, y, h, model = model, weights = weights, ...))
   }
   else if (class(x) != "formula") {
      x.name <- deparse(substitute(x))
      if (weights.missing) weights <- NA
      if (missing(model)) model <- "none"
      return(sm.density(x, h, model = model, weights = weights, xlab = x.name, ...))
   }
      
   opt <- sm.options(list(...))
   
   replace.na(opt, display,   "lines")
   replace.na(opt, reference, "none")
   replace.na(opt, panel,     FALSE)
   pam.formula <- x

   terms.obj        <- terms(pam.formula, specials = "s")
   vars.inf         <- eval.parent(attr(terms.obj, "variables"))
   term.labels      <- attr(terms.obj, "term.labels")
   s.ind            <- attr(terms.obj, "specials")$s
   response.ind     <- attr(terms.obj, "response")
   involved         <- attr(terms.obj, "factors")
   terms.linear     <- matrix(c(involved[s.ind, ]), ncol = length(term.labels))
   terms.linear     <- which(apply(terms.linear, 2, sum) == 0)
   nterms           <- length(term.labels)
   terms.smooth     <- which(!(1:nterms %in% terms.linear))
   rhs.linear       <- paste(term.labels[terms.linear], collapse = " + ")
   rhs.linear       <- if (nchar(rhs.linear) == 0) "1" else rhs.linear
   formula.linear   <- paste(rownames(involved)[response.ind], "~", rhs.linear)
   formula.linear   <- as.formula(formula.linear)
   names(vars.inf)  <- rownames(involved)
   bricks.type      <- sapply(vars.inf[-response.ind], mode)
   ind              <- (bricks.type == "numeric") & 
                       sapply(vars.inf[-response.ind], is.factor)
   bricks.type[ind] <- "factor"
   Xlinear          <- vars.inf[-response.ind][bricks.type != "list"]
   names(Xlinear)   <- names(bricks.type)[bricks.type != "list"]

   ylab             <- attr(terms.obj, "variables")
   ylab             <- strsplit(deparse(ylab), ",")[[1]][1]
   ylab             <- substr(ylab, 6, nchar(ylab))
   y                <- unlist(vars.inf[[response.ind]])
   X                <- list()
   xlabels          <- list()
   xlab             <- list()
   ndims            <- list()
   df               <- list()
   nseg             <- list()
   lambda           <- list()
   period           <- list()
   xrange           <- list()
   fixed            <- list()
   fac              <- list()
   xmissing         <- FALSE
   
   if (any(apply(involved, 2, sum) > 3))
      stop("four-way interactions not yet implemented.")

   for (i in 1:length(terms.smooth)) {
   	
      inv     <- which(involved[ , terms.smooth[i]] == 1)
      ilinear <- which(bricks.type[names(inv)] == "numeric")
      ifactor <- which(bricks.type[names(inv)] == "factor")
      if (length(ilinear) > 0)
         stop("interactions with linear terms are not yet implemented.")
      if (length(ifactor) > 1)
         stop("interactions with more than one factor are not yet implemented.")
      else if (length(ifactor) == 1) {
         fact     <- names(bricks.type)[ifactor]
         inv      <- inv[-match(fact, names(inv))]
         fac[[i]] <- Xlinear[[fact]]
      }
      else
         fac[[i]] <- NA
      
      nvars        <- length(inv)
      X[[i]]       <- matrix(nrow = length(y), ncol = 0)
      xlabels[[i]] <- vector("character")
      xlab[[i]]    <- vector("character")
      ndims[[i]]   <- numeric()
      df[[i]]      <- numeric()
      lambda[[i]]  <- numeric()
      period[[i]]  <- numeric()
      nseg[[i]]    <- numeric()
      xrange[[i]]  <- matrix( , nrow = 0, ncol = 2)
      fixed[[i]]   <- matrix( , nrow = 0, ncol = 2)
      for (j in inv) {
         lambda[[i]]  <- c(lambda[[i]], vars.inf[[j]]$lambda)
         nseg[[i]]    <- c(nseg[[i]],   vars.inf[[j]]$nseg)
         xlabels[[i]] <- c(xlabels[[i]], vars.inf[[j]]$variables)
      	 newvar       <- eval.parent(parse(text = vars.inf[[j]]$variables))
         if (is.matrix(newvar)) {
            nms <- colnames(newvar)
            if (any(is.null(colnames(newvar)))) 
                            nms <- paste(vars.inf[[j]]$variables, 
                                         "[", 1:ncol(newvar), "]", sep = "")
         }
         else 
            nms <- vars.inf[[j]]$variables
         xlab[[i]]    <- c(xlab[[i]], nms)
         newvar       <- as.matrix(newvar)
         ndims.new    <- ncol(newvar)
         ndims[[i]]   <- c(ndims[[i]], ndims.new)
         prd          <- vars.inf[[j]]$period
         if (length(prd) == 1 && is.na(prd)) prd <- rep(NA, ndims.new)
         if (length(prd) != ndims.new)
            stop("period does not match the columns of x.")
         period[[i]]  <- c(period[[i]], prd)
         if (any(!is.na(prd))) {         
            for (k in 1:ndims.new)
      	       if (!is.na(prd[k])) newvar[ , k] <- newvar[ , k] %% prd[k]
         }
         xrng <- vars.inf[[j]]$xrange
         if ((ndims.new == 1) & (length(xrng) == 2))
            xrng <- matrix(xrng, nrow = 1)
         if (!is.matrix(xrng))
            xrng <- matrix(NA, nrow = ndims.new, ncol = 2)
         if (nrow(xrng) != ndims.new)
            stop("xrange does not match columns of x.")
         for (k in 1:ndims.new) {
            if (any(is.na(xrng[k, ]))) {
               if (!is.na(prd[k]))
                  xrng[k, ] <- c(0, prd[k])
               else
                  xrng[k, ] <- c(min(newvar[ , k]), max(newvar[ , k]))
       # xrange <- t(apply(xrange, 1, function(x) c(x[1] - 0.05 * diff(x), x[2] + 0.05 * diff(x))))
            }
         }
         xrange[[i]]  <- rbind(xrange[[i]], xrng)
         fixed[[i]]   <- rbind(fixed[[i]], vars.inf[[j]]$fixed)
         X[[i]]       <- cbind(X[[i]], newvar)
         df.new       <- vars.inf[[j]]$df
         if (is.na(df.new)) df.new <- switch(ndims.new, 6, 12, 18)
         df[[i]]      <- c(df[[i]], df.new)
      }
#      if (any(is.na(nseg[[i]])) | prod(nseg[[i]]) > 400)
      if (any(is.na(nseg[[i]])))
         nseg[[i]] <- rep(switch(sum(ndims[[i]]), 100, 17, 7), sum(ndims[[i]]))
      if (any(is.na(X[[i]]))) xmissing  <- TRUE
   }
   
   # Remove observations which have missing data.
   ind.missing <- lapply(X, function(x) apply(x, 1, function(z) any(is.na(z))))
   ind.missing <- cbind(is.na(y), matrix(unlist(ind.missing), ncol = length(X)))
   ind.missing <- apply(ind.missing, 1, any)
   if (any(ind.missing)) {
      y <- y[!ind.missing]
      for (i in 1:length(X)) X[[i]] <- as.matrix(X[[i]][!ind.missing, ])
      cat("warning: missing data removed.\n")
   }
   
   if (opt$verbose > 1) tim <- proc.time()
   	
   P <- list(length = length(terms.smooth))
   B <- model.matrix(formula.linear, parent.frame())
   m <- ncol(B)
   
   for (i in 1:length(terms.smooth)) {
      mat    <- ps.matrices(X[[i]], xrange[[i]], ndims = ndims[[i]], 
                     nseg = nseg[[i]], period = period[[i]])
   	  if (all(is.na(fac[[i]]))) {
         B      <- cbind(B, mat$B)
         m      <- c(m, ncol(mat$B))
         P[[i]] <- mat$P
   	  }
      else {
         Btemp <- matrix(nrow = length(y), ncol = 0)
         for (j in levels(fac[[i]]))
            Btemp <- cbind(Btemp, mat$B * as.numeric(fac[[i]] == j))
         B      <- cbind(B, Btemp)
         m      <- c(m, ncol(Btemp))
         nlevs  <- length(levels(fac[[i]]))
         pdim   <- nlevs * ncol(mat$P)
         P[[i]] <- matrix(0, nrow = pdim, ncol = pdim)
         for (j in 1:nlevs) {
         	ind <- (j - 1) * ncol(mat$P) + (1:ncol(mat$P))
            P[[i]][ind, ind] <- mat$P
         }
         P[[i]] <- P[[i]] + matrix(1, ncol = nlevs, nrow = nlevs) %x% diag(ncol(mat$B))
      }
      xrange[[i]] <- mat$xrange
   }
      
   if (opt$verbose > 1) {
      cat("Timings:\nconstructing matrices", (proc.time() - tim)[1], "seconds\n")
      tim <- proc.time()
   }

   b.ind <- list(length = length(m))
   for (i in 1:length(terms.smooth))
      b.ind[[i]] <- (cumsum(m)[i] + 1):cumsum(m)[i + 1]
   
   if (weights.missing) {
      # btb   <- t(B)  %*% B
      btb   <- crossprod(B)
      # Does crossprod also work with a vector, below?
      bty   <- t(B)  %*% y
   }
   else if (is.vector(weights)) {
      btb   <- t(B * weights)  %*% B
      bty   <- t(B * weights)  %*% y
   }
   else if (is.matrix(weights)) {
      btb   <- t(B) %*% weights %*% B
      bty   <- t(B) %*% weights %*% y
   }
   else
      stop("the weights argument is inappropriate.")

   if (opt$verbose > 1) {
      cat("matrix products", (proc.time() - tim)[1], "seconds\n")
      tim <- proc.time()
   }

   # Select the smoothing parameters, if required
   lambda.df <- function(lambda, btb, P) {
      B1   <- solve(btb + lambda * P)
      sum(diag(btb %*% B1))
   }
   
   for (i in 1:length(terms.smooth)) {
      if (any(is.na(df[[i]]))) df[[i]] <- switch(sum(ndims[[i]]), 6, 12, 18)
      # code doesn't currently handle more than one df for terms with more than one variable.
      df[[i]] <- sum(df[[i]])
      if (df[[i]] > prod(nseg[[i]] + 3))
         stop(paste("df is too large for the value of nseg in term", i))
      if (any(is.na(lambda[[i]])))
         lambda[[i]] <- lambda.select(btb[b.ind[[i]], b.ind[[i]]], bty[b.ind[[i]]], P[[i]], df[[i]])
   }
   
   if (opt$verbose > 1) {
      cat("selecting smoothing parameters", (proc.time() - tim)[1], "seconds\n")
      tim <- proc.time()
   }

   # Fit
   Pall  <- matrix(0, nrow = ncol(B), ncol = ncol(B))
   for (i in 1:length(terms.smooth))
      Pall[b.ind[[i]], b.ind[[i]]] <- lambda[[i]] * P[[i]]
   B1    <- solve(btb + Pall) 
   # btb   <- t(B[,-1])  %*% diag(rep(1, length(y))) %*% B[,-1]
   # return(list(btb = btb))
   # B1    <- solve(btb[-1, -1] + Pall[-1, -1] - lambda[[i]] * mat$cmat)
   alpha <- as.vector(B1 %*% bty)
   # alpha <- as.vector(B1 %*% bty[-1])
   # mu    <- c(B[, -1] %*% alpha)
   # return(list(alpha = alpha, B = B[ , -1], btb = btb[-1, -1]))
   
   if (opt$verbose > 1) {
      cat("fitting", (proc.time() - tim)[1], "seconds\n")
      tim <- proc.time()
   }
   
   # Force the estimate to pass through fixed points (1 covariate only)
   if (length(terms.smooth) == 1 & ndims[[1]] == 1 & all(!is.na(fixed[[1]]))) {
      fxd <- fixed[[1]]
      if (any(fxd[,1] < xrange[[1]][1]) | 
          any(fxd[,1] > xrange[[1]][2]))
         stop("fixed points must be inside the range of the data.")
      A     <- cbind(1, ps.matrices(as.matrix(fxd[ , 1]), xrange[[1]], ndims[[1]],
                            nseg[[1]])$B)
      alpha <- alpha +  B1 %*% t(A) %*% solve(A %*% B1 %*% t(A)) %*% 
                             (fxd[ , 2] - A %*% alpha)
   }   

   mu         <- c(B %*% alpha)
   df.model   <- sum(btb * t(B1))
   df.error   <- length(y) - sum(btb * (2 * B1 - B1 %*% btb %*% B1))
   sigma      <- sqrt(sum((y - mu)^2) / df.error)
   cov.alpha  <- B1 %*% btb %*% t(B1) * sigma^2
   rss        <- sum((y - mu)^2)
   tss        <- sum((y - mean(y))^2)
   R.squared  <- 100 * (tss - rss) / tss
   
   if (opt$verbose > 1) {
      cat("summaries", (proc.time() - tim)[1], "seconds\n")
      tim <- proc.time()
   }

   # If there is only one term, include the mean
   # if (nterms == 1) b.ind[[1]] <- c(1, b.ind[[1]])

   result <- list(fitted = mu, alpha = alpha, m = m, B = B, 
                  bty = bty, btb = btb, B1 = B1, Pall = Pall, xlabels = xlabels,
                  linear.matrix = model.matrix(formula.linear, parent.frame()),
                  terms.linear = terms.linear, terms.smooth = terms.smooth,
                  xlab = xlab, ylab = ylab, term.labels = term.labels,
                  lambda = lambda, ndims = ndims, 
                  y = y, X = X, fac = fac, Xlinear = Xlinear,
                  bricks.type = bricks.type,
                  sigma = sigma, cov.alpha = cov.alpha, b.ind = b.ind,
                  df = df, df.model = df.model, df.error = df.error, 
                  rss = rss, R.squared = R.squared, xrange = xrange,
                  nseg = nseg, bdeg = bdeg, period = period, pam.formula = pam.formula,
                  involved = involved, nterms = nterms)
   if (!weights.missing) result$weights <- weights
   class(result) <- "pam"
   
   if (nterms == 1 & ndims[[1]] <= 2) {
      if (opt$panel) {
         replace.na(opt, df.max, switch(ndims[[1]], 20, 50, 100))
      	 df.min  <- switch(ndims[[1]], 2, 4, 8) + 0.1
         df.max  <- if (!opt$panel) df[[1]] else min(length(y) - 5, opt$df.max)
         df.min  <- if (!opt$panel) df[[1]] else df.min
         Pall    <- rbind(0, cbind(0, P[[1]]))
         llambda    <- 0
         llambda.df <- lambda.df(exp(max(llambda)), btb, Pall)
         while (min(llambda.df) >= df.min) {
            llambda    <- c(llambda, max(llambda) + log(10))
            llambda.df <- c(llambda.df, lambda.df(exp(max(llambda)), btb, Pall))
         }
         while (max(llambda.df) <= df.max) {
            llambda    <- c(llambda, min(llambda) - log(10))
            llambda.df <- c(llambda.df, lambda.df(exp(min(llambda)), btb, Pall))
         }
         df.fun <- approxfun(llambda.df, llambda)
   
         sm.pam.draw <- function(pam.panel) {
         	plot(pam.panel$model, options = pam.panel$opt)
            title(pam.panel$df)
            pam.panel
         }
         sm.pam.redraw <- function(pam.panel) {
            # pam.panel$model$lambda <- lambda.select(pam.panel$model$btb, pam.panel$model$bty,
            #                                         Pall, pam.panel$df)
            pam.panel$model$lambda <- exp(pam.panel$df.fun(pam.panel$df))
            B1 <- solve(pam.panel$model$btb + pam.panel$model$lambda * pam.panel$Pall)
            pam.panel$model$alpha  <- as.vector(B1 %*% pam.panel$model$bty)
            pam.panel$model$fitted <- c(pam.panel$model$B %*% pam.panel$model$alpha)
            pam.panel$opt$se       <- pam.panel$se
            pam.panel$opt$theta    <- pam.panel$theta
            pam.panel$opt$phi      <- pam.panel$phi
            rp.tkrreplot(pam.panel, plot)
            pam.panel
         }
         opt1 <- opt
         opt1$panel <- FALSE
         pam.panel <- rp.control(model = result, opt = opt1, Pall = rbind(0, cbind(0, P[[1]])),
                                 df = opt$df, df.fun = df.fun, theta = opt$theta, phi = opt$phi)
         rp.tkrplot(pam.panel, plot, sm.pam.draw, hscale = opt$hscale, vscale = opt$vscale, pos = "right")
         rp.slider(pam.panel, df, df.min, df.max, sm.pam.redraw, showvalue = TRUE)
         rp.checkbox(pam.panel, se, sm.pam.redraw, title = "Standard errors")
         if (ndims[[1]] == 2) {
            rp.slider(pam.panel, theta, -180, 180, sm.pam.redraw, "persp angle 1")
            rp.slider(pam.panel, phi,      0,  90, sm.pam.redraw, "persp angle 2")
         }
      }
      else if (opt$display != "none")
         plot(result, ...)
   }
   
   invisible(result)
}

#----------------------------------------------------------------------------

lambda.select <- function(btb, bty, P, df, method = "df") {
   #     This currently uses the same lambda in all dimensions
       lambda.df <- function(lambda, btb, P) {
          B1   <- solve(btb + lambda * P)
          # print(c(lambda, sum(diag(btb %*% B1))))
          sum(diag(btb %*% B1))
       }
       if (method == "df") {
          lambda <- 1
          while (lambda.df(lambda, btb, P) <= df) lambda <- lambda / 10
          lower  <- lambda
          lambda <- 1
          while (lambda.df(lambda, btb, P) >= df) lambda <- lambda * 10
          upper  <- lambda
          lambda.crit <- function(lambda, btb, P, df)
             lambda.df(lambda, btb, P) - df
          result <- uniroot(lambda.crit, interval = c(lower, upper), btb, P, df)
          # cat("result$root", result$root, "\n")
          lambda <- result$root
       }
    lambda
    }

#----------------------------------------------------------------------------

predict.pam <- function(model, newdata, se.fit = FALSE, verbose = 1, deriv = 0) {

   if (!is.list(newdata)) {
      newdata <- list(newdata)
      # names(newdata) <- vars.inf[[2]]$variables
      names(newdata) <- model$xlabels[[1]]
   }
   if (!all(unlist(model$xlabels) %in% names(newdata)))
      stop("some required variables are not present in the new data.")

   nnew <- if (is.matrix(newdata[[1]])) nrow(newdata[[1]])
         else length(newdata[[1]])
   X             <- list()

   for (i in 1:model$nterms) {
      inv          <- which(model$involved[ , i] == 1) - 1
      nvars        <- length(inv)
      X[[i]]       <- matrix(nrow = nnew, ncol = 0)
      for (j in inv) {
      	 newvar       <- eval(parse(text = model$xlabels[[i]][j]), newdata)
         X[[i]]       <- cbind(X[[i]], newvar)
      }
   }

   inrange <- rep(TRUE, nnew)
   for (i in 1:model$nterms)
      for (j in 1:ncol(X[[i]]))
   	     inrange <- inrange & X[[i]][ , j] >= model$xrange[[i]][j, 1] & 
   	                          X[[i]][ , j] <= model$xrange[[i]][j, 2] 
   outrange <- which(!inrange)
   if (length(outrange) > 0 & verbose > 0)
      warning("some evaluation points are out of range and have been omitted.")
   nnew <- length(which(inrange))
   B <- rep(1, nnew)
   for (i in 1:model$nterms) {
      mat    <- ps.matrices(as.matrix(X[[i]][inrange, ]), xrange = model$xrange[[i]], 
                     ndims = model$ndims[[i]], nseg = model$nseg[[i]], period = model$period[[i]])
      B      <- cbind(B, mat$B)
   }
   
   fv          <- rep(NA, nnew)
   fv[inrange] <- c(B %*% model$alpha)

   if (model$nterms == 1 & model$ndims[[1]] == 1 & deriv > 0) {
      mat    <- ps.matrices(as.matrix(X[[1]][inrange, ]), xrange = model$xrange[[1]], 
                     ndims = model$ndims[[1]], nseg = model$nseg[[1]], bdeg = model$bdeg - deriv, 
                     period = model$period[[1]])
   	  alpha1 <- diff(model$alpha[-1], differences = deriv)
   	  h      <- model$xrange[[1]][,2] - model$xrange[[1]][,1]
      fv[inrange] <- c(mat$B %*% alpha1) / (h / mat$nseg)^deriv
   }

   results     <- list(fit = fv, inrange = inrange, B = B)
   
   if (se.fit)
      results$se.fit = sqrt(diag(B %*% model$cov.alpha %*% t(B)))
   
   return(invisible(results))
}

#----------------------------------------------------------------------------

anova.pam <- function(model, terms = 1:length(model$b.ind), method = "QF", verbose = 1) {

   terms.obj <- terms(model$pam.formula, specials = "s")
   involved  <- attr(terms.obj, "factors")
   ord       <- attr(terms.obj, "order")
   ind       <- vector("logical", length = 0)
   for (i in terms) {
      inv    <- which(involved[ , i] > 0)
      ind <- c(ind, any(apply(involved, 2, function(x) all(x[inv] > 0)) & (ord > ord[i])))
   }
   terms <- terms[!ind]

   n         <- length(model$y)
   btb       <- model$btb
   B1        <- model$B1
   p         <- vector(length = 0)
   
   for (i in terms) {

      ind  <- model$b.ind[[i]]
      ind  <- ind[ind != 1]
      # weighted by covariance matrix
      eig  <- eigen(model$cov.alpha[ind, ind] / model$sigma^2)
      pos  <- which(abs(eig$values) > .Machine$double.eps)
      n0   <- length(eig$values) - length(pos)
      if (n0 > 0 & verbose > 1)
         cat(n0, "eigenvalues omitted out of", length(eig$values), "\n")
      inv  <- eig$vectors[, pos]  %*% diag(1/eig$values[pos]) %*% t(eig$vectors[, pos])
      # A1   <- B1[ , ind] %*% inv %*% B1[ind, ]
      # simple unweighted form
      A1   <- B1[ , ind] %*% B1[ind, ]
      Fobs <- c(model$y %*% model$B %*% A1 %*% t(model$B) %*% model$y) / model$rss
      # Fobs <- sum(model$alpha[ind]^2) / model$rss
      
      if (method == "QF") {
         A2   <- 2 * B1 - B1 %*% btb %*% B1
         A3   <- A1 + Fobs * A2
         A4   <- A3 %*% btb %*% A3 - 2 * Fobs * A3
         A5   <- A4 %*% btb %*% A3 + Fobs^2 * A3 - Fobs * A4
         k1   <-     sum(btb * A3) - n * Fobs
         k2   <- 2 * sum(btb * A4) + n * Fobs^2
         k3   <- 8 * sum(btb * A5) - n * Fobs^3
         aa   <- abs(k3 / (4 * k2))
         bb   <- (8 * k2^3) / k3^2
         cc   <- k1 - aa * bb
         p    <- c(p, 1 - pchisq(-cc / aa, bb))
      }

      else if (method == "F") {
         # P1   <- diag(nrow(P1)) - P1
         # P2   <- diag(nrow(P2)) - P2
         # P1   <- t(P1) %*% P1
         # P2   <- t(P2) %*% P2
         # df1  <- n - sum(diag(P1))
         # df2  <- n - sum(diag(P2))
         # rss1 <- c(y %*% P1 %*% y)
         # rss2 <- c(y %*% P1 %*% y)
         # Fobs <- (rss1 - rss2) * (n - df2) / (rss2 * (df2 - df1))
      	 df.m <- sum(btb * A1)
      	 # print(c(model$df[[i]], df.m))
      	 Fobs <- Fobs * model$df.error / df.m
         p    <- c(p, 1 - pf(Fobs, model$df.m, model$df.error))
      	 print(c(Fobs, p))
      	 # print(c(Fobs, model$df.error, model$df.model, df.m))
         # return(invisible(list(p = pval, Fobs = Fobs, df1 = df1, df2 = df2)))
      }
      else 
         stop("method not recognised.")
         
   }
   
   names(p) <- model$term.labels[terms]
   pmat <- matrix(round(p, 3), ncol = 1, dimnames = list(names(p), "p-value"))
   if (verbose > 0) print(pmat)
   return(invisible(list(p = p)))

}

	
#----------------------------------------------------------------------------

summary.pam <- function(model, verbose = 1) {
	
   lcoefs   <- model$alpha[1:model$m[1]]
   se       <- sqrt(model$cov.alpha[cbind(1:model$m[1], 1:model$m[1])])
   d.linear <- data.frame(Estimate = lcoefs, se = se,
                     row.names = paste("   ", colnames(model$linear.matrix)))
   if (verbose > 0) {
      cat("Linear terms:\n")
      print(d.linear)
   }
   
   nterms <- length(model$xlabels)
   adf    <- rep(0,  nterms)
   nvars  <- rep(1,  nterms)
   xl     <- rep("", nterms)
   low    <- rep(NA, nterms)
   high   <- rep(NA, nterms)
   for (i in 1:nterms) {
      l            <- paste("s(", model$xlabels[[i]], ")", sep = "")
      if (length(l) == 2) l <- paste(l[1], l[2], sep = ":")
      if (length(l) == 3) l <- paste(l[1], l[2], l[3], sep = ":")
      xl[i]        <- l
      ind          <- model$b.ind[[i]]
      btb.i        <- model$btb * 0
      btb.i[, ind] <- model$btb[ , ind]
      adf[i]       <- round(sum(btb.i * t(model$B1)), 1)
      nvars[i]     <- length(model$xlabels[[i]])
      mu.i         <- model$B[ , ind] %*% model$alpha[ind]
      low[i]       <- signif(min(mu.i))
      high[i]      <- signif(max(mu.i))
   }
   adf             <- c(adf,   round(model$df.model, 1))
   nvars           <- c(nvars, length(unique(unlist(model$xlabels))))
   low             <- c(low,   min(model$fitted))
   high            <- c(high,  max(model$fitted))
   d.smooth        <- data.frame(adf, nvars, low, high, high - low,
                       row.names = paste("   ", c(xl, "model")))
   names(d.smooth) <- c("df", "nvars", "     lowest", "highest", "range")
   if (verbose > 0) {
      cat("\nSmooth terms:\n")
      print(d.smooth)
      cat("\nError s.d.: ",  signif(model$sigma))
      cat("\nR-squared :  ", round(model$R.squared, 1), "%", sep = "")
   }
   
   invisible(list(linear = d.linear, smooth = d.smooth))
}
   
#----------------------------------------------------------------------------

plot.pam <- function(model, components = 1:length(model$xlabels), plotinfo, 
                   options = list(), ...) {

   if (length(options) > 0) opt <- sm.options(options)
   else                     opt <- sm.options(list(...))

   ngrid <- opt$ngrid
   replace.na(opt, reference,  "none")
   replace.na(opt, se,         FALSE)
   if (opt$reference != "none") opt$se <- TRUE
   replace.na(opt, nlevels,    20)
   replace.na(opt, panel,      TRUE)
   replace.na(opt, panel.plot, TRUE)
   replace.na(opt, display,    TRUE)
   display       <- opt$display
   se            <- opt$se
   ylim          <- opt$ylim
   partial.residuals <- opt$partial.residuals
   ngrid.missing <- is.na(ngrid)
   
   if (missing(plotinfo)) {
      plotinfo <- list()
      for (i in components) {
   	
         x <- model$X[[i]]
         if (is.vector(x)) x <- matrix(x, ncol = 1)
         ndim <- ncol(x)
      
         # Compute surface on a grid
         if (ngrid.missing) ngrid <- switch(ndim, 100, 20, 12)
         u  <- list(length = ndim)
         for (j in 1:ndim)
            u[[j]] <- seq(model$xrange[[i]][j, 1], model$xrange[[i]][j, 2], length = ngrid)
         U  <- as.matrix(expand.grid(u))
   #      b  <- list(length = ndim)
   #      mm <- vector(length = ndim)
   #      for (j in 1:ndim) {
   #         b[[j]] <- bbase(U[,j], xl = model$xrange[[i]][j, 1], xr = model$xrange[[i]][j, 2], 
   #                     nseg = model$nseg[[i]][j], deg = model$bdeg)
   #         mm[j]  <- dim(b[[j]])[2]
   #      }

   #      B <- b[[1]]
   #      if (ndim > 1)
   #         B <- t(apply(cbind(b[[1]], b[[2]]), 1, 
   #                              function(x) c(x[1:mm[1]] %x% x[-(1:mm[1])])))
   #      if (ndim == 3)
   #         B <- t(apply(cbind(B,  b[[3]]), 1, 
   #                  function(x) c(x[1:(mm[1]*mm[2])] %x% x[-(1:(mm[1]*mm[2]))])))

         B <- ps.matrices(U, model$xrange[[i]], model$ndims[[i]], nseg = model$nseg[[i]],
                            period = model$period[[i]])$B
         if (!all(is.na(model$fac[[i]]))) {
            nlevs <- length(levels(model$fac[[i]]))
            Btemp <- B
            if (nlevs > 1)
               for (j in 2:nlevs) B <- cbind(B, Btemp)
         }
       
         ind <- model$b.ind[[i]]
         est <- c(B %*% model$alpha[ind])
         if (ndim > 1) est <- array(est, dim = rep(ngrid, ndim))
      
         mui <- c(model$B[ , ind] %*% model$alpha[ind])
         model.fitted <- model$fitted
      
         result <- list(u = u, est = est, ndim = ndim, component = i, mui = mui, 
                        x = x, xlab = model$xlab[[i]], ylab = model$ylab,
                        model.fitted = model.fitted)
         if (se)
            result$st.error <- sqrt(diag(B %*% model$cov.alpha[ind, ind] %*% t(B)))
         plotinfo[[length(plotinfo) + 1]] <- result
      }
   }
   else {
      mui <- plotinfo[[1]]$mui
      est <- plotinfo[[1]]$est
      ngrid <- length(plotinfo[[1]]$u[[1]])
   }
   
   if (display == "none") return(plotinfo)
   
   # One smooth term and one factor: sm.ancova
   if (length(which(model$bricks.type == "list"))   == 1 &&
       length(which(model$bricks.type == "factor")) == 1 &&
       plotinfo[[1]]$ndim == 1) {
         Xl    <- model$Xlinear[[1]]
         lvls  <- levels(Xl)
         nlvls <- length(lvls)
         cfs   <- model$alpha[1:model$m[1]]
         est   <- matrix(plotinfo[[1]]$est, nrow = length(est), ncol = nlvls)
         for (i in 1:nlvls)
            est[ , i] <- est[ , i] + 
                 cfs %*% model$linear.matrix[min(which(Xl == lvls[i])), ]
         if (length(model$term.labels) == 3) {
            for (i in 1:nlvls) {
               cols      <- ncol(B) / nlvls
               cols      <- (i - 1) * cols + 1:cols
               ind       <- model$b.ind[[2]][cols]
               print(ind)
               est[ , i] <- est[ , i] + c(B[ , cols] %*% model$alpha[ind])
            }
         }
         if (any(is.na(ylim)))
         	ylim <- range(model$y, est)
         replace.na(opt, col, 1 + 1:nlvls)
   	     matplot(plotinfo[[1]]$u[[1]], est, type = "l", lty = 1, ylim = ylim,
   	                     xlab = model$xlabels[[1]], ylab = "component")
   	     points(x, model$y, col = opt$col[as.numeric(Xl)])
         # for (i in 1:nlvls)
         #    lines(plotinfo[[1]]$u[[1]], est + shift[i],
         #              col = opt$col[i], lty = opt$lty, lwd = opt$lwd)
         title(paste(c(model$term.labels[model$terms.linear], ":", lvls), collapse = " "))
         title("put the levels in different colours?", line = 1, cex.main = 0.7)
   	  return(invisible(plotinfo))
   }
   
#   if (length(ndim1) == 1) {
#   	  with(plotinfo[[ndim1]], {
#   	     if (any(is.na(ylim)))  ylim <- range(est)
#   	     if (partial.residuals) ylim <- range(ylim, model$y - (model.fitted - mui))
#   	     plot(est ~ u[[1]], ylab = "component", ylim = ylim, type = "n",
#   	                     xlab = model$xlabels[[components[ndim1]]])
#   	     if (length(model$b.ind) == 1)
#   	        points(x, model$y)
#         else if (partial.residuals)
#            points(x, model$y - (model.fitted - mui))
#         replace.na(opt, col, "blue")
#         lines(u[[1]], est, col = opt$col, lty = opt$lty, lwd = opt$lwd)
#         if (se) {
#            lines(u[[1]], est + 2 * st.error, col = opt$col, lty = 2)
#            lines(u[[1]], est - 2 * st.error, col = opt$col, lty = 2)
#         }
#   	  })
#   }
   
   ndim1 <- vector(length = 0)
   for (i in 1:length(plotinfo))
      if (plotinfo[[i]]$ndim == 1) ndim1 <- c(ndim1, i)

   if (length(ndim1) > 0) {
      est.all  <- numeric()
      u.all    <- numeric()
      x.all    <- numeric()
      pres.all <- numeric()
      se1.all  <- numeric()
      se2.all  <- numeric()
      lbl.x    <- character()
      lbl.u    <- character()
      xrng     <- vector("list", length(ndim1))
      for (i in 1:length(ndim1)) {
         est.all  <- c(est.all,  plotinfo[[ndim1[i]]]$est)
         u.all    <- c(u.all,    plotinfo[[ndim1[i]]]$u[[1]])
         x.all    <- c(x.all,    plotinfo[[ndim1[i]]]$x)
         pres.all <- c(pres.all, model$y - model.fitted + plotinfo[[ndim1[i]]]$mui)
         se1.all  <- c(se1.all,  plotinfo[[ndim1[i]]]$est + 
                                     2 * plotinfo[[ndim1[i]]]$st.error)
         se2.all  <- c(se2.all,  plotinfo[[ndim1[i]]]$est - 
                                     2 * plotinfo[[ndim1[i]]]$st.error)
         lbl.u    <- c(lbl.u,  rep(model$xlabels[[components[ndim1][i]]], 
                                      length(plotinfo[[ndim1[i]]]$u[[1]])))
         lbl.x    <- c(lbl.x,  rep(model$xlabels[[components[ndim1][i]]], 
                                      nrow(plotinfo[[ndim1[i]]]$x)))
         xrng[[i]] <- model$xrange[[ndim1[i]]]
      }
   	  if (any(is.na(ylim)))  ylim <- range(est.all)
   	  if (se)                ylim <- range(ylim, se1.all, se2.all)
   	  if (partial.residuals) {
   	     ylim <- range(ylim, pres.all)
   	     if (!require(lattice)) stop("the lattice package is not available.")
         print(xyplot(pres.all ~ x.all | lbl.x,
             scales = list(x = list(relation = "free")), ylim = ylim,
             # xlim = xrng,
             xlab = "", ylab = "component",
             panel = function(x, y, subscripts) {
             	       lpoints(x, y, col = "black", pch = ".")
             	       lb <- unique(lbl.x[subscripts])
             	       llines(u.all[lbl.u == lb], est.all[lbl.u == lb], col = "blue")
             	       # print(lb)
             	       # print(data.frame(u.all, lbl.u))
             	       # print(u.all[lbl.u == lb])
             	       # print(est.all[lbl.u == lb])
             	       if (se) {
             	          llines(u.all[lbl.u == lb], se1.all[lbl.u == lb], col = "blue", lty = 2)
             	          llines(u.all[lbl.u == lb], se2.all[lbl.u == lb], col = "blue", lty = 2)
             	       }
             }
         ))
   	  }
   	  else
         print(xyplot(est.all ~ u.all | lbl.u, type = "l", col = "blue",
             scales = list(x = list(relation = "free")), ylim = ylim,
             xlab = "", ylab = "component",
             panel = function(x, y, subscripts) {
             	       llines(x, y, col = "blue", pch = ".")             	          
             	       if (se) {
             	          llines(u.all[subscripts], se1.all[subscripts], col = "blue", lty = 2)
             	          llines(u.all[subscripts], se2.all[subscripts], col = "blue", lty = 2)
             	       }
             }
         ))
   }
   
   if (length(ndim1) < length(plotinfo)) {
      if (length(ndim1) > 0)
         ndim2 <- (1:length(plotinfo))[-ndim1]
      else
         ndim2 <- 1:length(plotinfo)
      for (i in ndim2) {
         with(plotinfo[[i]], {
         # del1  <- u[[1]][2] - u[[1]][1]
         # del2  <- u[[2]][2] - u[[2]][1]
         # ugrid <- as.matrix(expand.grid(u[[1]], u[[2]]))
         if (ndim == 2) {
            mask  <- sm.mask(x, cbind(u[[1]], u[[2]]), mask.method = opt$mask.method)
    	    if (any(is.na(ylim))) ylim <- range(est * mask, na.rm = TRUE)
            if (display %in% c("persp", "lines")) 
               persp(u[[1]], u[[2]], est * mask, 
                     xlab = xlab[1], ylab = xlab[2], zlab = ylab,
                     ticktype = "detailed", col = "green", zlim = ylim,
                     d = 10, theta = opt$theta, phi = opt$phi)
#            else if (display == "image")
#               image(u[[1]], u[[2]], est * mask, zlim = ylim) 
            else {
               filled.contour(u[[1]], u[[2]], est * mask, 
                              zlim = ylim, xlab = xlab[1], ylab = xlab[2],
                              color.palette = opt$col.palette.fn, nlevels = opt$nlevels,
                              plot.axes = {
                                 axis(1)
                                 axis(2)
                                 mtext(xlab[1], side = 1, line = 3)
                                 mtext(xlab[2], side = 2, line = 3, las = 0)
                                 if (partial.residuals)
                                    points(x, col = "blue", pch = 16, cex = 0.5)
                                 if (se & opt$reference == "no effect") {
                                    surf <- est * mask / matrix(st.error, ncol = ngrid)
                                    mx   <- floor(max(surf, na.rm = TRUE))
                                    if (mx >= 2) {
                                       lvls <- pretty(2:floor(max(surf, na.rm = TRUE)))
                                       contour(u[[1]], u[[2]], surf, levels = lvls, add = TRUE)
                                    }
                                    mn   <- ceiling(min(surf, na.rm = TRUE))
                                    if (mn <= -2) {
                                       lvls <- pretty((-2):floor(min(surf, na.rm = TRUE)))
                                       contour(u[[1]], u[[2]], surf, levels = lvls, add = TRUE, lty = 2)
                                    }
                                 }
                                 if (is.function(opt$superimpose))
                                    opt$superimpose()
                                 })
#               image(u[[1]], u[[2]], est * mask, 
#                               zlim = ylim, xlab = xlab[1], ylab = xlab[2],
#                               col = opt$col.palette)

            }
         }
         else if (ndim == 3) {

            mdl  <- list(x = cbind(u[[1]], u[[2]]), z = u[[3]], y = est)
            if (opt$reference == "no effect") 
               mdl$reference <- plotinfo[[i]]$est / array(st.error, dim = dim(est))
            else if (se)
            	   mdl$reference <- array(st.error, dim = dim(est))
            rp.plot4d(x[ , 1:2], x[ , 3], mui, mdl)
            
            # pam.slice.draw <- function(panel) {
               # with(panel, {
               # # if (!is.na(files)) pdf(sub(".pdf", paste(i, ".pdf", sep = ""), files))
               # if (partial.residuals) x.unique <- unique(x)
               # if (display %in% "persp") 
                  # persp(u[[1]], u[[2]], est[ , , igrid] * mask[ , , igrid], zlim = ylim,
                        # xlab = xlab[1], ylab = xlab[2], zlab = ylab,
                        # ticktype = "detailed", col = "green", d = 10, theta = 30)
               # else {

# #                  image(u[[1]], u[[2]], t(est[ , , i] * mask), 
# #                              zlim = zlim, xlab = xlab[1], ylab = xlab[2],
# #                              col = col.palette(40),
# #                              main = paste(xlab[3], ": ", signif(u[[3]][i]), sep = ""))

               # #    ind          <- (cumsum(model.m)[2] + 1):cumsum(model.m)[2 + 1]
               # #    baseline     <- model.B[ , ind] %*% model.alpha[ind]
               # #    baseline     <- matrix(baseline, ngrid * ngrid)
               # #    image(u[[1]], u[[2]], baseline + est[ , , i] * mask, zlim = zlim)
               # #    }
               # # else
               
                  # # print(range(est[,,i]*mask, na.rm = TRUE))
                  # # print(lvls)
                  # # print(exp(est[ , , igrid]) * mask[ , , igrid])
                  # # image(est[ , , igrid] * mask[ , , igrid])
                  # # stop()

                  # lvls <- opt$levels
                  # if (all(is.null(opt$levels)))
                     # lvls <- pretty(opt$transform.response(ylim), opt$nlevels)
                  # # filled.contour(u[[1]], u[[2]], opt$transform.response(est[ , , igrid]) * mask[ , , igrid], 
                              # # zlim = opt$transform.response(ylim), xlab = xlab[1], ylab = xlab[2],
                              # # color.palette = opt$col.palette.fn, nlevels = opt$nlevels,
                              # # levels = lvls,
                              # # asp = opt$asp,
                              # # plot.axes = {
                                 # # axis(1)
                                 # # axis(2)
                                 # # mtext(xlab[1], side = 1, line = 3)
                                 # # mtext(xlab[2], side = 2, line = 3, las = 0)
                                 # # if (opt$partial.residuals)
                                    # # points(x.unique, col = "blue", pch = 19, cex = 0.5)
                                 # # if ("sdiff" %in% names(model)) {
                                    # # cts   <- contourLines(u[[1]], u[[2]], 
                                                        # # model$sdiff[, , igrid] * mask[ , , igrid])
                                    # # lvls  <- rep(0, length(cts))
                                    # # for (i in 1:length(cts)) lvls[i] <- cts[[i]]$level
                                    # # lvls  <- lvls[lvls <= -2 | lvls >= 2]
	                                # # if (length(lvls[lvls > 0]) > 0)
	                                   # # contour(u[[1]], u[[2]], 
	                                                    # # model$sdiff[, , igrid] * mask[ , , igrid], 
	                                          # # levels = lvls[lvls > 0], add = TRUE, col = "red")
	                                # # if (length(lvls[lvls < 0]) > 0)
	                                   # # contour(u[[1]], u[[2]],
	                                                    # # model$sdiff[, , igrid] * mask[ , , igrid], 
	                                          # # levels = lvls[lvls < 0], add = TRUE, col = "red", lty = 2)
                                 # # }
                                 # # if (se & reference == "no effect") {
                                    # # surf <- t(est[ , , i] * mask[ , , igrid]) / t(st.e[ , , igrid])
                                    # # lvls <- pretty(2:floor(max(surf, na.rm = TRUE)))
                                    # # contour(u[[1]], u[[2]], surf, levels = lvls, add = TRUE)
                                    # # lvls <- pretty((-2):floor(min(surf, na.rm = TRUE)))
                                    # # contour(u[[1]], u[[2]], surf, levels = lvls, add = TRUE,
                                            # # lty = 2)
                                 # # }
                                 # # if (is.function(opt$superimpose))
                                    # # opt$superimpose()
                              # # },
                              # # plot.title = title(paste(xlab[3], ": ", 
                                               # # signif(u[[3]][igrid]), sep = "")))

                 # image(u[[1]], u[[2]], t(est[ , , igrid] * mask[ , , igrid]), 
                             # zlim = ylim, xlab = xlab[1], ylab = xlab[2],
                             # col = opt$col.palette(40))
                 # if (se & reference == "no effect") {
                    # surf <- t(est[ , , igrid] * mask[ , , igrid]) / t(st.e[ , , igrid])
                    # lvls <- pretty(2:floor(max(surf, na.rm = TRUE)))
                    # contour(u[[1]], u[[2]], surf, levels = lvls, add = TRUE)
                    # lvls <- pretty((-2):floor(min(surf, na.rm = TRUE)))
                    # contour(u[[1]], u[[2]], surf, levels = lvls, add = TRUE, lty = 2)
                 # }

# #                   levelplot(t(est[ , , i] * mask) ~ u[[1]] * u[[2]],
# #                                   zlim = ylim, xlab = xlab[1], ylab = xlab[2], aspect = "iso")

               # }
               # # if (!is.na(files)) dev.off()
               # })
               # panel
            # }
            # pam.slice.redraw <- function(panel) {
               # panel$igrid <- min(panel$igrid, panel$ngrid)
               # panel$igrid <- max(panel$igrid, 1)
               # rp.tkrreplot(panel, plot)
               # panel
            # }
            # pam.slice.draw1 <- function(panel) {
               # panel$igrid <- min(panel$igrid, panel$ngrid)
               # panel$igrid <- max(panel$igrid, 1)
               # rp.do(panel, pam.slice.draw)
               # panel
            # }
            
            # st.e  <- if (se) array(st.error, dim = rep(ngrid, 3)) else NULL
            # u     <- plotinfo[[i]]$u
            # x     <- plotinfo[[i]]$x
            # mask  <- plotinfo[[i]]$est * NA
            # for (j in 1:length(u[[3]])) {
               # indj <- (abs(u[[3]][j] - x[,3]) <= diff(range(u[[3]])) / length(u[[3]]))
               # if (length(which(indj)) > 2) 
                  # mask[ , , j] <- sm.mask(x[indj, 1:2], cbind(u[[1]], u[[2]]), 
                                          # mask.method = opt$mask.method)
               # else
                  # mask[ , , j] <- NA
            # }
            # #     Temporary fix!
            # mask.temp <- apply(mask, 1:2, function(x) as.numeric(any(!is.na(x))))
            # for (ii in 1:(dim(mask)[3])) mask [ , , ii] <- mask.temp
            # mask[mask == 0] <- NA
            
   	        # if (any(is.na(ylim)))
   	           # ylim <- range(est * array(c(mask), dim = dim(est)), na.rm = TRUE)
   	           
            # if (!(display %in% c("rgl"))) {
               # if (opt$panel) {
                  # panel <- rp.control(est = plotinfo[[i]]$est, mask = mask,
                                      # st.e == st.e, igrid = 1,
                                      # xlab = plotinfo[[i]]$xlab,
                                      # u = u, ylim = ylim, model.m = model$m, 
                                      # ngrid = ngrid, display = display,
                                      # reference = opt$reference)
                  # # rp.tkrplot(panel, key,  sm.pam.colour.chart, pos = "right", hscale = 0.2, vscale = 1.1)
                  # if (opt$panel.plot) {
                     # rp.tkrplot(panel, plot, pam.slice.draw, pos = "right", 
                                 # hscale = opt$hscale, vscale = opt$vscale)
                     # action.fn <- pam.slice.redraw
                  # }
                  # else
                     # action.fn <- pam.slice.draw
                  # # rp.doublebutton(panel, i, 1, action = pam.slice.redraw)
                  # rp.slider(panel, igrid, 1, ngrid, action.fn,
                           # title = plotinfo[[i]]$xlab[3], resolution = 1)
               # }
               # else {
                  # igd <- if ("igrid" %in% names(opt)) opt$igrid else 1
                  # pam.slice.draw(list(est = plotinfo[[i]]$est, mask = mask,
                                      # st.e == st.e, igrid = igd,
                                      # xlab = plotinfo[[i]]$xlab,
                                      # u = u, ylim = ylim, model.m = model$m, 
                                      # ngrid = ngrid, display = display,
                                      # reference = opt$reference))
               # }
            # }
            # else {
               # pres <- model$y - (model.fitted - plotinfo[[i]]$mui)
               # clr  <- cut(pres, nlevels, labels = FALSE)
               # lvls <- seq(min(pres), max(pres), length = nlevels)
               # clr  <- col.palette(nlevels)[clr]
               # scale.fn <- rp.plot3d(x[,1], x[,2], x[,3], xlab = xlab[1], ylab = xlab[2], zlab = xlab[3],
                                     # col = clr)	
               # clr  <- cut(c(est), nlevels, labels = FALSE)
               # lvls <- seq(min(c(est)), max(c(est)), length = nlevels + 1)
               # lvls <- lvls[-c(1, nlevels + 1)]
               # struct <- contour3d(est, lvls, engine = "none")
               # surf.ids <- integer(0)
               # for (i in 2:(nlevels - 1)) {
             	  # if (length(lvls) > 1) strct <- struct[[i]] else strct <- struct
                  # trngs.x <- c(t(cbind(strct$v1[, 1], strct$v2[, 1],strct$v3[, 1])))
                  # trngs.y <- c(t(cbind(strct$v1[, 2], strct$v2[, 2],strct$v3[, 2])))
                  # trngs.z <- c(t(cbind(strct$v1[, 3], strct$v2[, 3],strct$v3[, 3])))
                  # a <- scale.fn(trngs.x, trngs.y, trngs.z)
                  # surf.ids <- c(surf.ids, 
                        # triangles3d(a$x, a$y, a$z, col = col.palette(nlevels)[i], alpha = 0.3))
               # }

            # }

            }
         })
      }
   }
   
   invisible(plotinfo)
}

sm.pam.colour.chart <- function(panel) {
  par(mar = c(5, 1, 4, 2) + 0.1)
  rp.colour.chart(panel$col.palette, panel$ylim)
  panel
  }

rp.colour.chart <- function(cols, zlim)  {
   ngrid <- length(cols)
   plot(0:1, zlim, type = "n", xaxs = "i", yaxs = "i", xaxt = "n", yaxt = "n", xlab = "", ylab = "")
   axis(4)
   xvec <- rep(0, ngrid)
   yvec <- seq(zlim[1], zlim[2], length = ngrid + 1)
   rect(xvec, yvec[-length(yvec)], xvec + 1, yvec[-1], col = cols, border = NA)
   box()
   }

#----------------------------------------------------------------------------
fitted.pam <- function(model) model$fitted

residuals.pam <- function(model) model$y - model$fitted

#----------------------------------------------------------------------------
sm.mask <- function(x, eval.points, mask.method = "hull") {
	
   ngrid       <- nrow(eval.points)
   grid.points <- cbind(rep(eval.points[, 1], ngrid), 
                           rep(eval.points[, 2], rep(ngrid, ngrid)))
                           
   if (mask.method == "hull") {
      hull.points <- as.matrix(x[order(x[, 1], x[, 2]), ])
      dh          <- diff(hull.points)
      hull.points <- hull.points[c(TRUE, !((dh[, 1] == 0) & (dh[, 2] == 0))), ]
      hull.points <- hull.points[chull(hull.points), ]
      nh          <- nrow(hull.points)
      gstep       <- matrix(rep(eval.points[2, ] - eval.points[1, ], nh),
                        ncol = 2, byrow = TRUE)
      hp.start    <- matrix(rep(eval.points[1, ], nh), ncol = 2, byrow = TRUE)
      hull.points <- hp.start + gstep * round((hull.points - hp.start)/gstep)
      hull.points <- hull.points[chull(hull.points), ]
      D           <- diff(rbind(hull.points, hull.points[1, ]))
      temp        <- D[, 1]
      D[, 1]      <- D[, 2]
      D[, 2]      <- (-temp)
      C           <- as.vector((hull.points * D) %*% rep(1, 2))
      C           <- matrix(rep(C, ngrid^2), nrow = ngrid^2, byrow = TRUE)
      D           <- t(D)
      wy          <- ((grid.points %*% D) >= C)
      wy          <- apply(wy, 1, all)
      wy[wy]      <- 1
      wy[!wy]     <- NA
      mask        <- matrix(wy, ncol = ngrid)
   }
   else if (mask.method == "near") {
   	  del1  <- eval.points[2, 1] - eval.points[1, 1]
   	  del2  <- eval.points[2, 2] - eval.points[1, 2]
      mask  <- apply(grid.points, 1,
                   function(z) any(((z[1] - x[,1])/del1)^2 + ((z[2] - x[,2])/del2)^2 < 4^2))
      mask  <- matrix(as.numeric(mask), ncol = ngrid)
      mask[mask == 0] <- NA
   }
   else
      mask <- matrix(1, ncol = ngrid, nrow = ngrid)
      
   return(invisible(mask))
}

#----------------------------------------------------------------------------
s <- function(..., lambda = NA, df = NA, period = NA, xrange = NA, nseg = NA,
                   fixed = c(NA, NA)) {
   vars.list <- as.list(substitute(list(...)))[-1]
   nvar <- length(vars.list)
   if (nvar > 3)
      stop("smooth terms can be constructed from only 1, 2 or 3 variables.")
   variables <- character(0)
   for (i in 1:nvar) variables <- c(variables, deparse(vars.list[[i]]))
   list(variables = variables, lambda = lambda, df = df, period = period,
        xrange = xrange, nseg = nseg, fixed = fixed)
}

#----------------------------------------------------------------------------

ps.matrices <- function(x, xrange, ndims, nseg, bdeg = 3, pord = 2, period = NA,
                            decompose =  TRUE) {

    # Compute a set of basis functions and a penalty matrix associated with x.
    # An intercept term and the main effect of any interaction terms are removed.
    
    ndimx <- ncol(x)
    if (ndimx > 3) stop("terms with more than three dimensions cannot be used.")
    n    <- nrow(x)
    
    if (missing(nseg)) nseg <- rep(switch(ndimx, 100, 17, 7), ndimx)
    
    # Compute B-spline basis
    
    b <- list(length = ndimx)
    m <- vector(length = ndimx)
    for (i in 1:ndimx) {
       b[[i]] <- bbase(x[,i], xl = xrange[i , 1], xr = xrange[i, 2], nseg = nseg[i], 
                       deg = bdeg)
       m[i]   <- ncol(b[[i]])
    }

    B <- b[[1]]
    if (ndimx > 1)
       B <- t(apply(cbind(b[[1]], b[[2]]), 1,
                            function(x) c(x[1:m[1]] %x% x[-(1:m[1])])))
    if (ndimx == 3)
       B <- t(apply(cbind(B,  b[[3]]), 1, 
                function(x) c(x[1:(m[1]*m[2])] %x% x[-(1:(m[1]*m[2]))])))
    
    # Construct smoothness penalty matrices
    P <- list()
    for (i in 1:ndimx) {
       P[[i]] <- diff(diag(m[i]), diff = pord)
       if (!is.na(period[i])) {
       	  z      <- c(1, rep(0, m[i] - 4), -1)
          P[[i]] <- rbind(P[[i]], c(z, 0, 0))
          P[[i]] <- rbind(P[[i]], c(0, z, 0))
          P[[i]] <- rbind(P[[i]], c(0, 0, z))
       }
       P[[i]] <- crossprod(P[[i]])
    }
    if (ndimx >= 2) {
       P[[1]] <- P[[1]] %x% diag(m[2])
       P[[2]] <- diag(m[2]) %x% P[[2]]
    }
    if (ndimx == 3) {
       P[[1]] <- P[[1]] %x% diag(m[3])
       P[[2]] <- P[[2]] %x% diag(m[3])
       P[[3]] <- diag(m[1]) %x% diag(m[2]) %x% P[[3]]
    }
    pmat <- matrix(0, nrow = ncol(B), ncol = ncol(B))
    for (i in 1:ndimx)
       pmat <- pmat + P[[i]]

#     Construct anova constraint penalty matrices
    if (length(ndims) == 1) {
       # Sum of coefficients constraint
       # cmat <- matrix(1, nrow = prod(m), ncol = prod(m))
       # Sum of estimated values constraint
       Bsum <- apply(B, 2, sum)
       cmat <- Bsum %o% Bsum
       # Corner point constraint (first coefficient is 0
       # cmat <- diag(c(1, rep(0, ncol(B) - 1)))
       pmat <- pmat + cmat
    }
    else if (length(ndims) == 2) {
       if (all(ndims == c(1, 1))) ind <- c(m[1], m[2])
       if (all(ndims == c(1, 2))) ind <- c(m[1], m[2] * m[3])
       if (all(ndims == c(2, 1))) ind <- c(m[1] * m[2], m[3])
       pmat <- pmat + matrix(1, nrow = ind[1], ncol = ind[1]) %x% diag(ind[2])
       pmat <- pmat + diag(ind[1]) %x% matrix(1, nrow = ind[2], ncol = ind[2])
    }
    else if (length(ndims) == 3) {
       pmat <- pmat + matrix(1, nrow = m[1], ncol = m[1]) %x% diag(m[2]) %x% diag(m[3])
       pmat <- pmat + diag(m[1]) %x% matrix(1, nrow = m[2], ncol = m[2]) %x% diag(m[3])
       pmat <- pmat + diag(m[1]) %x% diag(m[2]) %x% matrix(1, nrow = m[3], ncol = m[3])
    }
        
    result <- list(B = B, P = pmat, xrange = xrange, nseg = nseg, bdeg = bdeg, pord = pord)
    if (length(ndims) == 1) result$cmat <- cmat
    invisible(result)
}

bbase <- function(x, xl = min(x), xr = max(x), nseg = 10, deg = 3) {
# Construct B-spline basis
    dx <- (xr - xl) / nseg
    knots <- seq(xl - deg * dx, xr + deg * dx, by = dx)
    P <- outer(x, knots, tpower, deg)
    n <- dim(P)[2]
    D <- diff(diag(n), diff = deg + 1) / (gamma(deg + 1) * dx ^ deg)
    B <- (-1) ^ (deg + 1) * P %*% t(D)
    B
}

tpower <- function(x, t, p)
# Truncated p-th power function
    (x - t) ^ p * (x > t)

