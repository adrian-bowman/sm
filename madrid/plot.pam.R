#----------------------------------------------------------------------------
#                                plot
#----------------------------------------------------------------------------

plot.pam <- function(model, components = 1:length(model$xlabels), plotinfo,
                     options = list(), ...) {
   
   opt <- if (length(options) > 0) sm.options(options) else sm.options(list(...))
   
   if (length(components) == 1 && components == "linear") {
      smy   <- summary(model)
      upper <- smy$linear[-1, 1] + 2 * smy$linear[-1, 2]
      lower <- smy$linear[-1, 1] - 2 * smy$linear[-1, 2]
      trms  <- rownames(smy$linear)[-1]
      trms  <- factor(trms, levels = trms)
      xl    <- if (any(is.na(opt$ylim))) range(upper, lower) else opt$ylim
      dfrm  <- data.frame(terms = rep(trms, 2), x = c(upper, lower))
      plt   <- ggplot2::ggplot(dfrm, ggplot2::aes(x, terms, group = terms)) +
         ggplot2::geom_line(col = "blue", size = 2) +
         ggplot2::scale_x_continuous(limits = xl)
      if ("title" %in% names(opt)) plt <- plt + ggplot2::ggtitle(opt$title)
      print(plt)
      return()
   }
   
   if (any(components > model$nterms))
      stop("some components do not match model terms.")
   
   ngrid <- opt$ngrid
   replace.na(opt, reference,               "none")
   replace.na(opt, nlevels,                 20)
   replace.na(opt, panel,                   TRUE)
   replace.na(opt, panel.plot,              TRUE)
   replace.na(opt, display,                 "image")
   replace.na(opt, eqscplot,                FALSE)
   replace.na(opt, deriv.order,             if (!is.na(opt$deriv)) 1 else 0)
   replace.na(opt, deriv,                   NA)
   replace.na(opt, include.terms,           "single")
   if (model$nterms == 1) opt$include.terms <- "lower"
   inc.mn <- (opt$include.terms == "lower") & (is.na(opt$deriv) | (opt$deriv.order == 0))
   replace.na(opt, include.mean, inc.mn)
   se.replace <- switch(opt$include.terms, single = "term", lower = "lower.interactions", additive.only = "none")
   replace.na(opt, se,                      se.replace)
   replace.na(opt, size,                    1)
   replace.na(opt, lwd,                     2)
   replace.na(opt, cex,                     1)
   replace.na(opt, colour.key,              TRUE)
   replace.na(opt, z.key,                   TRUE)
   # col.pal <- if (requireNamespace("RColorBrewer", quietly = TRUE))
   # rev(colorRampPalette(RColorBrewer::brewer.pal(9, "BuGn"))(100))
   # else topo.colors(100)
   if (requireNamespace("colorspace", quietly = TRUE))
      col.pal <- if (opt$include.terms == "lower") colorspace::heat_hcl(100) else colorspace::diverge_hcl(100)
   else
      col.pal <- topo.colors(100)
   replace.na(opt, col.palette, col.pal)
   replace.na(opt, order, 1:3)
   if (!("vscale" %in% names(list(...)))) opt$vscale <- opt$hscale
   
   if (!is.na(opt$deriv)) {
      if (length(components) > 1)
         stop("only one component can be specified when a derivative is requested.")
      if (!(opt$deriv %in% model$xlabels[[components]]))
         stop("the requested derivative is not in the specified model term.")
      if (opt$deriv.order == 0) opt$deriv <- NA
   }
   
   # To handle additive terms
   if (is.character(components)) {
      comps      <- as.numeric(unlist(strsplit(components, "+", fixed = TRUE)))
      components <- comps[1]
      additive   <- TRUE
   }
   else
      additive <- FALSE
   # Code not yet followed through
   
   display           <- opt$display
   se                <- opt$se
   ylim              <- opt$ylim
   partial.residuals <- opt$partial.residuals
   ngrid.missing     <- is.na(ngrid)
   
   # Set up the plotting information
   if (missing(plotinfo)) {
      plotinfo <- list()
      
      for (i in components) {
         
         x     <- model$X[[i]]
         xdims <- model$xdims[[i]]
         
         if (is.vector(x)) x <- matrix(x, ncol = 1)
         ndim <- sum(model$ndims[[i]])
         if (ngrid.missing) ngrid <- switch(ndim, 100, 20, 15)
         u   <- list(length = ndim)
         for (j in 1:ndim)
            u[[j]] <- seq(model$xrange[[i]][j, 1], model$xrange[[i]][j, 2], length = ngrid)
         U   <- as.matrix(expand.grid(u))
         
         if (opt$include.terms == "lower") {
            ind.terms <- which(sapply(model$xlabels, function(x) all(x %in% model$xlabels[[i]])))
         }
         else
            ind.terms <- i
         if (!is.na(opt$deriv)) {
            ind.t     <- integer(0)
            for (k in ind.terms)
               if (opt$deriv %in% model$xlabels[[k]]) ind.t <- c(ind.t, k)
            ind.terms <- ind.t
         }
         
         deriv <- if (is.na(opt$deriv)) 0 else opt$deriv.order
         
         B       <- matrix(nrow = nrow(U), ncol = 0)
         B.int   <- B
         B.add   <- B
         ind     <- integer(0)
         ind.int <- integer(0)
         ind.add <- integer(0)
         
         for (j in ind.terms) {
            Uj <- matrix(nrow = nrow(U), ncol = 0)
            for (k in 1:length(model$xlabels[[j]])) {
               indk <- match(model$xlabels[[j]][k], model$xlabels[[i]])
               cs   <- cumsum(model$xdims[[i]])[indk]
               indk <- (cs - model$xdims[[i]][indk] + 1):cs
               Uj   <- cbind(Uj, U[ , indk])
            }
            # Uj  <- U[, match(model$xlabels[[j]], model$xlabels[[i]])]
            # if (!is.matrix(Uj)) Uj <- matrix(Uj, ncol = 1)
            ind.d <- which(model$xlabels[[j]] == opt$deriv)
            bdg   <- rep(model$bdeg, sum(model$ndims[[j]]))
            bdg[ind.d] <- bdg[ind.d] - deriv
            Bj <- ps.matrices(Uj, model$xrange[[j]], model$ndims[[j]], nseg = model$nseg[[j]],
                              bdeg = bdg, pord = model$pord[[i]],
                              period = model$period[[j]], penalty = FALSE)$B
            if (!is.na(opt$deriv) & deriv > 0) {
               h  <- (model$xrange[[j]][ind.d, 2] - model$xrange[[j]][ind.d, 1]) / model$nseg[[j]][ind.d]
               I  <- diag(model$nseg[[j]][1] + model$bdeg)
               D1 <- diff(I, differences = deriv)
               D  <- 1
               for (k in 1:length(bdg)) {
                  Mk <- if (k == ind.d) D1 else I
                  D  <- D %x% Mk
               }
               Bj <- Bj %*% D / (h^deriv)
            }
            B   <- cbind(B, Bj)
            ind <- c(ind, model$b.ind[[j]])
            if (j == i) {
               Bi   <- Bj
               indi <- model$b.ind[[i]]
            }
            if (length(model$xlabels[[j]]) == 1) {
               B.add   <- cbind(B.add, Bj)
               ind.add <- c(ind.add, model$b.ind[[j]])
            }
            else if (j == i |
                     (ndim == 3) & (all(model$xlabels[[j]] %in% model$xlabels[[i]][-opt$order[3]]))) {
               B.int   <- cbind(B.int, Bj)
               ind.int <- c(ind.int, model$b.ind[[j]])
            }
         }
         
         # Deal with factors
         if (!all(is.na(model$fac[[i]]))) {
            nlevs <- length(levels(model$fac[[i]]))
            Btemp <- B
            if (nlevs > 1)
               for (j in 2:nlevs) B <- cbind(B, Btemp)
         }
         
         # Add the mean if necessary
         if (opt$include.mean) {
            B   <- cbind(1, B)
            ind <- c(1, ind)
         }
         
         # Compute the estimate
         B.est   <- switch(opt$include.terms, single = Bi, lower = B, additive.only = B.add)
         ind.est <- switch(opt$include.terms, single = indi, lower = ind, additive.only = ind.add)
         est     <- c(B.est %*% model$alpha[ind.est])
         if (ndim > 1)
            est <- array(est, dim = rep(ngrid, ndim))
         
         mui <- if (is.na(opt$deriv)) c(model$B[ , ind.est] %*% model$alpha[ind.est])
         else rep(NA, length(model$y))
         model.fitted <- model$fitted
         
         result <- list(u = u, est = est, ndim = ndim, component = i, mui = mui,
                        x = x, xdims = xdims, xlab = model$xlab[[i]], ylab = model$ylab,
                        model.fitted = model.fitted)
         
         # Add information on standard errors if required
         if (se != "none") {
            if (se == "term") {
               B.se   <- Bi
               ind.se <- indi
            }
            else if (se == "lower.interactions") {
               B.se   <- B.int
               ind.se <- ind.int
            }
            else
               stop("value for argument 'se' not recognised.")
            result$st.error  <- sqrt(diag(B.se %*% model$cov.alpha[ind.se, ind.se] %*% t(B.se)))
            if (opt$reference == "no effect") {
               est.se <- c(B.se %*% model$alpha[ind.se])
               if (ndim > 1)
                  est.se <- array(est.se, dim = rep(ngrid, ndim))
               dm <- if (ndim > 1) dim(est.se) else length(est.se)
               result$reference <- est.se / array(result$st.error, dm)
            }
         }
         
         plotinfo[[length(plotinfo) + 1]] <- result
      }
   }
   else {
      mui   <- plotinfo[[1]]$mui
      est   <- plotinfo[[1]]$est
      ngrid <- length(plotinfo[[1]]$u[[1]])
   }
   
   if (display == "none") {
      if (length(plotinfo) == 1) plotinfo <- plotinfo[[1]]
      return(plotinfo)
   }
   
   #------------------------------------------------------------------------
   #                            Plotting
   #------------------------------------------------------------------------
   
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
            est[ , i] <- est[ , i] + c(B[ , cols] %*% model$alpha[ind])
         }
      }
      if (any(is.na(ylim))) ylim <- range(model$y, est)
      replace.na(opt, col, 1 + 1:nlvls)
      if (!requireNamespace("ggplot2", quietly = TRUE)) stop("the ggplot2 package is required.")
      xgrid <- plotinfo[[1]]$u[[1]]
      dfrm  <- data.frame(xgrid = rep(xgrid, nlvls), est = c(est), lvl = factor(rep(1:nlvls, each = length(xgrid))))
      dfrm1 <- data.frame(x = plotinfo[[1]]$x, y = model$y, lvl = model$Xlinear[[1]])
      plt   <- ggplot2::ggplot(dfrm, ggplot2::aes(x = xgrid, y = est, colour = lvl)) +
         ggplot2::geom_line() + ggplot2::geom_point(data = dfrm1, ggplot2::aes(x, y, colour = lvl)) +
         ggplot2::labs(colour = names(model$Xlinear)[1]) +
         ggplot2::xlab(model$xlabels[[1]]) + ggplot2::ylab(model$ylab)
      print(plt)
      # matplot(plotinfo[[1]]$u[[1]], est, type = "l", lty = 1, ylim = ylim,
      # xlab = model$xlabels[[1]], ylab = "component")
      # points(x, model$y, col = opt$col[as.numeric(Xl)])
      # # for (i in 1:nlvls)
      # #    lines(plotinfo[[1]]$u[[1]], est + shift[i],
      # #              col = opt$col[i], lty = opt$lty, lwd = opt$lwd)
      # title(paste(c(model$term.labels[model$terms.linear], ":", lvls), collapse = " "))
      # title("put the levels in different colours?", line = 1, cex.main = 0.7)
      plotinfo$plot <- plt
      return(invisible(plotinfo))
   }
   
   if (!is.na(opt$deriv)) partial.residuals <- FALSE
   
   #------------------------------------------------------------------------
   #                                 1-d terms
   #------------------------------------------------------------------------
   
   ndim1 <- vector(length = 0)
   for (i in 1:length(plotinfo))
      if (plotinfo[[i]]$ndim == 1) ndim1 <- c(ndim1, i)
   
   if (length(ndim1) > 0) {
      est.all  <- numeric()
      u.all    <- numeric()
      x.all    <- numeric()
      xx.all   <- numeric()
      pres.all <- numeric()
      y.all    <- numeric()
      se1.all  <- numeric()
      se2.all  <- numeric()
      lbl.x    <- character()
      lbl.u    <- character()
      lbl      <- character()
      xrng     <- vector("list", length(ndim1))
      for (i in 1:length(ndim1)) {
         pinf      <- plotinfo[[ndim1[i]]]
         uu        <- pinf$u[[1]]
         x.add     <- uu
         if (partial.residuals)
            x.add  <- c(c(pinf$x), uu)
         y.add     <- pinf$est
         if (partial.residuals)
            y.add <- c(model$y - model.fitted + pinf$mui, y.add)
         u.all     <- c(u.all, uu)
         est.all   <- c(est.all, pinf$est)
         lblx.add  <- rep("line", length(uu))
         if (partial.residuals)
            lblx.add  <- c(rep("data", length(pinf$x)), lblx.add)
         if (se != "none" | opt$reference != "none") {
            se1.add  <-  2 * pinf$st.error
            se2.add  <- -2 * pinf$st.error
            if (opt$reference == "none") {
               se1.add <- se1.add + pinf$est
               se2.add <- se2.add + pinf$est
            }
            se1.all  <- c(se1.all,  se1.add)
            se2.all  <- c(se2.all,  se2.add)
            x.add    <- c(x.add, uu, rev(uu))
            y.add    <- c(y.add, se1.add, rev(se2.add))
            lblx.add <- c(lblx.add, rep("se", 2 * length(uu)))
         }
         x.all     <- c(x.all, x.add)
         y.all     <- c(y.all, y.add)
         lbl.x     <- c(lbl.x, lblx.add)
         lbl       <- c(lbl,   rep(model$xlabels[[components[ndim1][i]]], length(x.add)))
         xrng[[i]] <- model$xrange[[ndim1[i]]]
      }
      lbl <- factor(lbl, levels = unlist(model$xlabels[components[ndim1]]))
      if (any(is.na(ylim))) ylim <- range(y.all)
      if (se != "none")     ylim <- range(ylim, se1.all,  se2.all)
      if (!requireNamespace("ggplot2", quietly = TRUE)) stop("the ggplot2 package is required.")
      dfrm  <- data.frame(x.all, y.all, lbl.x, lbl)
      # dfrm1 <- data.frame(u.all, est.all, se1.all, se2.all)
      plt   <- ggplot2::ggplot(dfrm, ggplot2::aes(x.all, y.all))
      if (se != "none" | opt$reference != "none")
         plt <- plt + ggplot2::geom_polygon(data = subset(dfrm, lbl.x == "se"), fill = "lightblue")
      if (partial.residuals)
         plt <- plt + ggplot2::geom_point(data = subset(dfrm, lbl.x == "data"), size = opt$size)
      if (all(!is.na(opt$xlim)))
         plt <- plt + ggplot2::xlim(opt$xlim[1], opt$xlim[2])
      plt   <- plt + ggplot2::ylim(ylim[1], ylim[2])
      plt   <- plt + ggplot2::geom_line(data = subset(dfrm, lbl.x == "line"), col = "blue", size = opt$lwd)
      nr    <- if (is.null(opt$nrow)) NULL else opt$nrow
      plt   <- plt + ggplot2::facet_wrap( ~ lbl, nrow = nr, scales = "free_x")
      plt   <- plt + ggplot2::xlab("")
      if (!is.na(opt$deriv.order))
         ylb <- paste(model$ylab, switch(opt$deriv.order, "(1st deriv.)", "(2nd deriv.)"))
      else
         ylb <- "partial residuals"
      plt   <- plt + ggplot2::ylab(ylb)
      if ("title" %in% names(opt)) plt <- plt + ggplot2::ggtitle(opt$title)
      print(plt)
   }
   
   #------------------------------------------------------------------------
   #                                 2-d terms
   #------------------------------------------------------------------------
   
   if (length(ndim1) < length(plotinfo)) {
      if (length(ndim1) > 0)
         ndim2 <- (1:length(plotinfo))[-ndim1]
      else
         ndim2 <- 1:length(plotinfo)
      for (i in ndim2) {
         env <- environment()
         with(plotinfo[[i]], {
            # del1  <- u[[1]][2] - u[[1]][1]
            # del2  <- u[[2]][2] - u[[2]][1]
            # ugrid <- as.matrix(expand.grid(u[[1]], u[[2]]))
            
            if (ndim == 2) {
               mask  <- sm.mask(x, cbind(u[[1]], u[[2]]), mask.method = opt$mask.method)
               # mask  <- sm.mask(x[ , opt$order[1:2]], cbind(u[[opt$order[1]]], u[[opt$order[2]]]),
               #                  mask.method = opt$mask.method)
               if (any(is.na(ylim))) ylim <- range(est * mask, na.rm = TRUE)
               if (!opt$include.terms == "lower") ylim <- c(-1, 1) * max(abs(ylim))
               if (length(xlab) == 1)
                  xlab <- if (!is.null(colnames(x))) colnames(x) else paste(xlab, 1:2, sep = "-")
               # xlab <- xlab[opt$order[1:2]]
               if (!is.na(opt$deriv)) {
                  if (opt$deriv.order == 1) ylab <- paste(ylab, "(1st derivative)")
                  if (opt$deriv.order == 2) ylab <- paste(ylab, "(2nd derivative)")
               }
               clr <- if (all(is.na(opt$col))) "green" else opt$col
               if (clr == "height") {
                  clr  <- array(c(est[-ngrid, -ngrid], est[    -1, -ngrid],
                                  est[-ngrid,     -1], est[    -1,     -1]),
                                dim = c(ngrid - 1, ngrid - 1, 4))
                  clr  <- apply(clr, 1:2, function(x)
                     if (length(which(is.na(x))) > 1) NA else mean(x, na.rm = TRUE))
                  brks <- seq(opt$ylim[1], opt$ylim[2], length = 101)
                  clr  <- diverge_hcl(100)[cut(c(clr), brks, labels = FALSE)]
                  clr  <- matrix(c(clr), nrow = ngrid - 1, ncol = ngrid - 1)
               }
               if (exists("reference")) {
                  sdiff <- reference * mask
                  sdiff <- array(c(sdiff[-ngrid, -ngrid], sdiff[    -1, -ngrid],
                                   sdiff[-ngrid,     -1], sdiff[    -1,     -1]),
                                 dim = c(ngrid - 1, ngrid - 1, 4))
                  sdiff <- apply(sdiff, 1:2, function(x)
                     if (length(which(is.na(x))) > 1) NA else mean(x, na.rm = TRUE))
                  sdiff <- matrix(c(sdiff), nrow = ngrid - 1, ncol = ngrid - 1)
                  # if (all(opt$order[1:2] == 2:1)) sdiff <- t(sdiff)
                  se.breaks <- c(-3, -2, 2, 3)
                  # col.pal   <- rev(rainbow(length(se.breaks) + 1, start = 0/6, end = 4/6))
                  col.pal   <- diverge_hcl(length(se.breaks) + 1)
                  se.breaks <- c(min(-3, sdiff, na.rm = TRUE) - 1, se.breaks,
                                 max(3, sdiff, na.rm = TRUE) + 1)
                  clr <- col.pal[cut(c(sdiff), se.breaks, labels = FALSE)]
                  clr <- matrix(clr, ngrid - 1, ngrid - 1)
               }
               if (display %in% c("persp", "lines")) {
                  if (any(is.na(opt$x1lim))) opt$x1lim <- range(u[[opt$order[1]]])
                  if (any(is.na(opt$x2lim))) opt$x2lim <- range(u[[opt$order[2]]])
                  persp(u[[opt$order[1]]], u[[opt$order[2]]], aperm(est * mask, opt$order[1:2]),
                        xlab = xlab[opt$order[1]], ylab = xlab[opt$order[2]], zlab = ylab,
                        ticktype = "detailed", col = c(aperm(clr, opt$order[1:2])),
                        xlim = opt$x1lim, ylim = opt$x2lim, zlim = ylim,
                        d = 10, theta = opt$theta, phi = opt$phi)
               }
               else if ((display %in% "rgl") &&
                        (requireNamespace("rgl", quietly = TRUE) &
                         requireNamespace("rpanel", quietly = TRUE))) {
                  # yy   <- model$y - model.fitted + mui
                  # ylim <- range(yy, aperm(est * mask, opt$order[1:2]), na.rm = TRUE)
                  # if (exists("reference"))
                  # 	 clr <- col.pal[cut(c(reference), se.breaks, labels = FALSE)]
                  # opt$scaling <- rp.plot3d(x[, 1], yy, x[, 2],
                  # 				xlab = xlab[opt$order[1]], ylab = ylab, zlab = xlab[opt$order[2]],
                  # 				xlim = opt$x1lim, ylim = ylim, zlim = opt$x2lim,
                  # 				size = 0.5, col = "black")
                  if (any(is.na(opt$x1lim))) opt$x1lim <- range(u[[opt$order[1]]])
                  if (any(is.na(opt$x2lim))) opt$x2lim <- range(u[[opt$order[2]]])
                  se.breaks <- c(-(4:2), 2:4)
                  # col.pal   <- rev(rainbow(length(se.breaks) + 1, start = 0/6, end = 4/6))
                  col.pal   <- diverge_hcl(length(se.breaks) + 1)
                  se.breaks <- c(min(-4, reference, na.rm = TRUE) - 1, se.breaks,
                                 max( 4, reference, na.rm = TRUE) + 1)
                  clr <- col.pal[cut(c(reference), se.breaks, labels = FALSE)]
                  clr <- matrix(clr, ngrid, ngrid)
                  new.window <- ("new.window" %in% names(opt) && !opt$new.window)
                  opt$scaling <- rp.plot3d(rep(u[[opt$order[1]]], ngrid),
                                           c(aperm(est * mask, opt$order[1:2])),
                                           rep(u[[opt$order[2]]], each = ngrid),
                                           xlab = xlab[opt$order[1]], ylab = ylab, zlab = xlab[opt$order[2]],
                                           xlim = opt$x1lim, zlim = opt$x2lim,
                                           new.window = !new.window,
                                           ylim = ylim, cex = opt$cex,
                                           type = "n")
                  surf.ids <- sm.surface3d(cbind(u[[opt$order[1]]], u[[opt$order[2]]]),
                                           aperm(est * mask, opt$order[1:2]), opt$scaling,
                                           col = c(aperm(clr, opt$order[1:2])),
                                           col.mesh = opt$col.mesh,
                                           alpha = opt$alpha, alpha.mesh = 1, lit = FALSE)
                  if (opt$key) {
                     rp.colour.key(col.pal, se.breaks, par.mar = c(3, 0, 1, 2.5) + 0.1)
                     mtext(ylab, side = 4, line = 1.1, font = 1)
                     layout(1)
                  }
               }
               else if (("leaflet" %in% names(opt)) && requireNamespace("leaflet", quietly = TRUE)) {
                  del  <- diff(u[[opt$order[1]]])[1]
                  lng1 <- u[[opt$order[1]]] - del/2
                  lng2 <- u[[opt$order[1]]] + del/2
                  lng1 <- rep(lng1, ngrid)
                  lng2 <- rep(lng2, ngrid)
                  del  <- diff(u[[opt$order[2]]])[1]
                  lat1 <- u[[opt$order[2]]] - del/2
                  lat2 <- u[[opt$order[2]]] + del/2
                  lat1 <- rep(lat1, each = ngrid)
                  lat2 <- rep(lat2, each = ngrid)
                  brks <- seq(opt$ylim[1], opt$ylim[2], length = 101)
                  clr  <- diverge_hcl(100)[cut(c(est * mask), brks, labels = FALSE)]
                  ind  <- which(!is.na(clr))
                  lng1 <- lng1[ind]
                  lng2 <- lng2[ind]
                  lat1 <- lat1[ind]
                  lat2 <- lat2[ind]
                  clr  <- clr[ind]
                  if (!("fillOpacity" %in% names(opt))) opt$fillOpacity <- 0.8
                  mp   <- opt$leaflet %>%
                     addRectangles(lng1, lat1, lng2, lat2, col = clr,
                                   fillOpacity = opt$fillOpacity, stroke = FALSE)
                  print(mp)
                  assign("leaflt", mp, envir = env)
                  assign("mask", mask, envir = env)
               }
               else {
                  if (opt$key) layout(matrix(c(1, 2), ncol = 2), widths = c(7, 1))
                  par(mar = c(3, 3, 1, 0.5) + 0.1, mgp = c(1.5, 0.2, 0), tcl = -0.2)
                  if (opt$eqscplot)
                     MASS::eqscplot(x[ , opt$order[1:2]], type = "n", xlab = xlab[1], ylab = xlab[2])
                  else {
                     plot(x[ , opt$order[1]], x[ , opt$order[2]], type = "n", axes = FALSE,
                          xlab = xlab[opt$order[1]], ylab = xlab[opt$order[2]])
                     usr <- par("usr")
                     rect(usr[1], usr[3], usr[2], usr[4], col = grey(0.9), border = NA)
                     grid(col = "white", lty = 1)
                     axis(1, lwd = 0, lwd.ticks = 2, col = grey(0.6), col.ticks = grey(0.6),
                          col.axis = grey(0.6))
                     axis(2, lwd = 0, lwd.ticks = 2, col = grey(0.6), col.ticks = grey(0.6),
                          col.axis = grey(0.6))
                  }
                  # brks[is.infinite(brks) & (brks > 0)] <- max(y, model$y, na.rm = TRUE) + 1
                  # brks[is.infinite(brks) & (brks < 0)] <- min(y, model$y, na.rm = TRUE) - 1
                  
                  dfrm <- data.frame(x = rep(u[[opt$order[1]]], length(u[[opt$order[2]]])),
                                     y = rep(u[[opt$order[2]]], each = length(u[[opt$order[1]]])),
                                     z = c(t(aperm(est * mask, opt$order[1:2]))))
                  ind  <- !is.na(dfrm$x + dfrm$y + dfrm$z)
                  dfrm <- subset(dfrm, ind)
                  if (requireNamespace("interp", quietly = TRUE)) {
                     grdx  <- seq(min(dfrm$x), max(dfrm$x), length = 200)
                     grdy  <- seq(min(dfrm$y), max(dfrm$y), length = 200)
                     intrp <- interp::interp(dfrm$x, dfrm$y, dfrm$z, grdx, grdy)
                     dfrmx <- dfrm$x
                     dfrmy <- dfrm$y
                     dfrm  <- data.frame(x = rep(intrp$x, length(intrp$y)),
                                         y = rep(intrp$y, each = length(intrp$x)),
                                         z = c(intrp$z))
                     if (exists("reference")) {
                        rfr <- reference
                        if (all(opt$order[1:2] == 2:1)) rfr <- t(rfr)
                        intrpr <- interp::interp(dfrmx, dfrmy, c(rfr)[ind], grdx, grdy)
                     }
                     dfrm <- list(x = grdx, y = grdy, z = intrp$z)
                     if (exists("reference")) dfrm$r <- intrpr$z
                  }
                  else {
                     dfrm   <- as.list(dfrm)
                     dfrm$z <- matrix(dfrm$z, nrow = dim(model$y)[1])
                  }
                  
                  image(intrp$x, intrp$y, t(intrp$z), zlim = ylim, col = opt$col.palette, add = TRUE)
                  # xlab = xlab[opt$order[1]], ylab = xlab[opt$order[2]])
                  if (exists("reference")) {
                     lvls <- pretty(c(2, max(c(2, dfrm$r), na.rm = TRUE)))
                     mmx <- max(c(2, dfrm$r), na.rm = TRUE)
                     if (mmx >= 2) {
                        lvls <- if (trunc(mmx) > 5) pretty(c(2, trunc(mmx))) else 2:trunc(mmx)
                        # contour(mx[ , 1], mx[ , 2], mr, add = TRUE, col = "blue", levels = lvls, lty = 1)
                        contour(dfrm$x, dfrm$y, dfrm$r, add = TRUE, cex = opt$cex,
                                col = rev(opt$col.palette)[1], levels = lvls, lty = 1)
                     }
                     lvls <- pretty(c(-2, min(c(-2, dfrm$r), na.rm = TRUE)))
                     mmn <- min(c(-2, dfrm$r), na.rm = TRUE)
                     if (mmn <= -2) {
                        lvls <- if (trunc(mmn) < -5) pretty(c(-2, trunc(mmn))) else (-2):trunc(mmn)
                        # contour(mx[ , 1], mx[ , 2], mr, add = TRUE, col = "blue", levels = lvls, lty = 2)
                        contour(dfrm$x, dfrm$y, dfrm$r, add = TRUE, cex = opt$cex,
                                col = opt$col.palette[1], levels = lvls, lty = 2)
                     }
                  }
                  
                  if (is.function(opt$foreground.plot)) opt$foreground.plot()
                  
                  del  <- 0.04 * diff(ylim)
                  brks <- seq(ylim[1] - del, ylim[2] + del, length = length(opt$col.palette) + 1)
                  assign("brks", brks, envir = env)
                  ylb <- ylab
                  assign("ylb", ylb, envir = env)
                  
                  if (opt$key) {
                     rp.colour.key(opt$col.palette, brks, par.mar = c(3, 0, 1, 2.5) + 0.1)
                     mtext(ylb, side = 4, line = 1.1, font = 1)
                     layout(1)
                  }
                  
               }
            }
            
            #------------------------------------------------------------------------
            #                                 3-d terms
            #------------------------------------------------------------------------
            
            else if (ndim == 3) {
               
               mask  <- sm.mask(x[ , opt$order[1:2]], cbind(u[[opt$order[1]]], u[[opt$order[2]]]),
                                mask.method = opt$mask.method)
               mdl   <- list(x = cbind(u[[opt$order[1]]], u[[opt$order[2]]]), z = u[[opt$order[3]]],
                             y = aperm(est, opt$order))
               mdl$y <- array(c(mdl$y) * c(mask), dim = dim(mdl$y))
               
               if (opt$reference == "no effect" & opt$se != "none") {
                  rfr           <- aperm(reference, opt$order)
                  mdl$reference <- array(c(rfr) * c(mask), dim = dim(rfr))
               }
               bg.plot <- if (is.function(opt$background.plot)) opt$background.plot else NULL
               fg.plot <- if (is.function(opt$foreground.plot)) opt$foreground.plot else NULL
               if (length(xlab) < 3) {
                  xl <- character(0)
                  for (j in 1:length(xdims)) {
                     if (xdims[j] > 1) {
                        cs  <- cumsum(xdims[1:j])
                        ind <- (cs - xdims[j] + 1):cs
                        xln <- if (!is.null(colnames(x[ , ind]))) colnames(x[ , ind])
                        else paste(xlab, 1:xdims[j], sep = "-")
                        xl <- c(xl, xln)
                     }
                     else
                        xl <- c(xl, xlab[j])
                  }
                  xlab <- xl
               }
               ylb <- ylab
               if (opt$deriv.order == 1) ylb <- paste(ylb, "(1st deriv.)")
               if (opt$deriv.order == 2) ylb <- paste(ylb, "(2nd deriv.)")
               if (!("z.window.pars" %in% names(opt)))
                  opt$z.window.pars <- c(min(x[ , opt$order[3]]), sd(x[ , opt$order[3]])/5)
               if (!("panel" %in% names(opt)))
                  opt$panel <- TRUE
               if (any(is.na(ylim))) {
                  rng            <- range(mdl$y, na.rm = TRUE)
                  del            <- 0.04 * diff(rng)
                  opt$col.breaks <- seq(rng[1] - del, rng[2] + del, length = length(opt$col.palette) + 1)
               }
               else
                  opt$col.breaks <- seq(ylim[1], ylim[2], length = length(opt$col.palette) + 1)
               rp.plot4d(x[ , opt$order[1:2]], x[ , opt$order[3]], mui, mdl,
                         ylab = ylb, z.window.pars = opt$z.window.pars, panel = opt$panel,
                         zlab = xlab[opt$order[3]], x1lab = xlab[opt$order[1]], x2lab = xlab[opt$order[2]],
                         cex = opt$cex, col.palette = opt$col.palette, col.breaks = opt$col.breaks,
                         hscale = opt$hscale, vscale = opt$vscale,
                         colour.key = opt$colour.key, z.key = opt$z.key,
                         new.window = opt$new.window,
                         background.plot = bg.plot, foreground.plot = fg.plot, display = display)
               if (length(components) == 1 & ndim == 3) assign("mdl", mdl, envir = env)
            }
         })
      }
   }
   
   if (length(components) == 1 & display == "image" & ndim == 2) {
      plotinfo         <- plotinfo[[1]]
      plotinfo$ylab    <- ylb
      plotinfo$brks    <- brks
      plotinfo$palette <- opt$col.palette
   }
   if (length(components) == 1 & ndim == 3) {
      plotinfo       <- plotinfo[[1]]
      plotinfo$model <- mdl
   }
   if (length(components) == 1 &
       ("leaflet" %in% names(opt)) && requireNamespace("leaflet", quietly = TRUE)) {
      plotinfo         <- plotinfo[[1]]
      plotinfo$leaflet <- leaflt
      plotinfo$mask    <- mask
   }
   if (length(components) == length(ndim1)) {
      plotinfo     <- plotinfo[[1]]
      plotinfo$plt <- plt
   }
   invisible(plotinfo)
}

#----------------------------------------------------------------------------
#                                 predict
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
   else                         length(newdata[[1]])
   X    <- list()
   
   for (i in 1:length(model$terms.smooth)) {
      ii     <- model$terms.smooth[i]
      inv    <- which(model$involved[ , ii] == 1) - 1
      nvars  <- length(inv)
      X[[i]] <- matrix(nrow = nnew, ncol = 0)
      for (j in 1:length(inv)) {
         newvar  <- eval(parse(text = model$xlabels[[i]][j]), newdata)
         X[[i]]  <- cbind(X[[i]], newvar)
      }
   }
   
   inrange <- rep(TRUE, nnew)
   for (i in 1:length(model$terms.smooth))
      for (j in 1:ncol(X[[i]]))
         inrange <- inrange & X[[i]][ , j] >= model$xrange[[i]][j, 1] &
      X[[i]][ , j] <= model$xrange[[i]][j, 2]
   outrange <- which(!inrange)
   if (length(outrange) > 0 & verbose > 0)
      warning("some evaluation points are out of range and have been omitted.")
   nnew <- length(which(inrange))
   
   B.linear <- model.matrix(model$formula.linear, newdata)
   if (nrow(B.linear) == 0) B.linear <- matrix(1, nrow = nnew, ncol = 1)
   
   B    <- matrix(c(B.linear[inrange, ]), ncol = ncol(B.linear))
   for (i in 1:length(model$terms.smooth)) {
      mat <- ps.matrices(as.matrix(X[[i]][inrange, ]), xrange = model$xrange[[i]],
                         ndims = model$ndims[[i]], nseg = model$nseg[[i]],
                         pord = model$pord[[i]], period = model$period[[i]])
      B   <- cbind(B, mat$B)
   }
   
   fv          <- rep(NA, nnew)
   fv[inrange] <- c(B %*% model$alpha)
   
   if (model$nterms == 1 & model$ndims[[1]] == 1 & deriv > 0) {
      mat         <- ps.matrices(as.matrix(X[[1]][inrange, ]), xrange = model$xrange[[1]],
                                 ndims = model$ndims[[1]], nseg = model$nseg[[1]], bdeg = model$bdeg - deriv,
                                 pord = model$pord[[i]], period = model$period[[1]])
      h           <- (model$xrange[[1]][,2] - model$xrange[[1]][,1]) / mat$nseg
      D           <- diff(diag(length(model$alpha[-1])), differences = deriv)
      fv[inrange] <- model$alpha[1] + c(mat$B %*% D %*% model$alpha[-1]) / (h^deriv)
   }
   # if (model$nterms == 1 & model$ndims[[1]] == 1 & deriv > 0) {
   #   mat    <- ps.matrices(as.matrix(X[[1]][inrange, ]), xrange = model$xrange[[1]],
   #                         ndims = model$ndims[[1]], nseg = model$nseg[[1]], bdeg = model$bdeg - deriv,
   #                         period = model$period[[1]])
   #   alpha1 <- diff(model$alpha[-1], differences = deriv)
   #   h      <- model$xrange[[1]][,2] - model$xrange[[1]][,1]
   #   fv[inrange] <- c(mat$B %*% alpha1) / (h / mat$nseg)^deriv
   # }
   
   results     <- list(fit = fv)
   
   if (se.fit)
      results$se.fit = sqrt(diag(B %*% model$cov.alpha %*% t(B)))
   
   return(invisible(results))
}

#----------------------------------------------------------------------------
#                                 anova
#----------------------------------------------------------------------------

anova.pam <- function(model, terms = 1:length(model$b.ind), method = "QF", weighted = TRUE, verbose = 1) {
   
   terms.obj <- terms(model$pam.formula, specials = "s")
   involved  <- attr(terms.obj, "factors")
   ord       <- attr(terms.obj, "order")
   ind       <- vector("logical", length = 0)
   for (i in terms) {
      inv <- which(involved[ , i] > 0)
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
      if (weighted) {
         # weighted by covariance matrix
         eig  <- eigen(model$cov.alpha[ind, ind] / model$sigma^2)
         pos  <- which(abs(eig$values) > .Machine$double.eps)
         n0   <- length(eig$values) - length(pos)
         if (n0 > 0 & verbose > 1)
            cat(n0, "eigenvalues omitted out of", length(eig$values), "\n")
         inv  <- eig$vectors[, pos]  %*% diag(1/eig$values[pos]) %*% t(eig$vectors[, pos])
         A1   <- B1[ , ind] %*% inv %*% B1[ind, ]
      }
      else {
         # simple unweighted form
         A1   <- B1[ , ind] %*% B1[ind, ]
      }
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
         # print(c(Fobs, p))
         # print(c(Fobs, model$df.error, model$df.model, df.m))
         # return(invisible(list(p = pval, Fobs = Fobs, df1 = df1, df2 = df2)))
      }
      else
         stop("method not recognised.")
      
   }
   
   names(p) <- model$term.labels[terms]
   pmat <- matrix(round(p, 3), ncol = 1, dimnames = list(names(p), "p-value"))
   if (verbose > 0) print(pmat)
   
   return(invisible(list(p = p, F = Fobs)))
   
}

#----------------------------------------------------------------------------
#                                 summary
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
   d.smooth        <- data.frame(adf, nvars, high - low,
                                 # pmax(abs(high), abs(low)),
                                 row.names = paste("   ", 1:length(adf), c(xl, "model")))
   names(d.smooth) <- c("df", "nvars", "max. size")
   
   if (verbose > 0) {
      cat("\nSmooth terms:\n")
      print(d.smooth)
      cat("\nError s.d.: ",  signif(model$sigma))
      if ("sigma.r" %in% names(model))
         cat("\nR.e.  s.d.: ",  signif(model$sigma.r))
      if (!is.null(model$R.squared))
         cat("\nR-squared :  ", round(model$R.squared, 1), "%", sep = "")
   }
   
   invisible(list(linear = d.linear, smooth = d.smooth))
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
#                          fitted and residuals
#----------------------------------------------------------------------------

fitted.pam <- function(model) model$fitted

residuals.pam <- function(model) model$y - model$fitted
