"sm.surface3d" <- function(eval.points, surf, scaling, 
                              col = "green", col.mesh = "black", alpha = 0.7, alpha.mesh = 1, 
                              lit = TRUE, ...) {

      #     This function adds a surface to the current rgl plot.
      
      if (!is.function(scaling)) stop("a scaling must be specified.")
      
      if (all(is.na(col)))                  col   <- "green"
      if ((length(col) == 1) && (col == 1)) col   <- "green"

      ep <- eval.points
      if (is.matrix(ep) && ncol(ep) == 2) {
         ep1 <- ep[ , 1]
         ep2 <- ep[ , 2]
      }
      else if (is.list(ep) && length(ep) == 2) {
         ep1 <- ep[[1]]
         ep2 <- ep[[2]]
      }
      else
         stop("the form of eval.points in sm.surface3d is invalid.")

      ngrid1 <- length(ep1)
      ngrid2 <- length(ep2)         
      col      <- matrix(c(col),      nrow = ngrid1, ncol = ngrid2)
      col.mesh <- matrix(c(col.mesh), nrow = ngrid1, ncol = ngrid2)
      
      xg1    <- rep(ep1[-ngrid1], ngrid2 - 1)
      xg2    <- rep(ep1[     -1], ngrid2 - 1)
      xg3    <- rep(ep1[     -1], ngrid2 - 1)
      xg4    <- rep(ep1[-ngrid1], ngrid2 - 1)
      zg1    <- rep(ep2[-ngrid2], each = ngrid1 - 1)
      zg2    <- rep(ep2[-ngrid2], each = ngrid1 - 1)
      zg3    <- rep(ep2[     -1], each = ngrid1 - 1)
      zg4    <- rep(ep2[     -1], each = ngrid1 - 1)
      yg1    <- c(surf[-ngrid1, -ngrid2])
      yg2    <- c(surf[     -1, -ngrid2])
      yg3    <- c(surf[     -1,      -1])
      yg4    <- c(surf[-ngrid1,      -1])
      col1   <- c(col[-ngrid1, -ngrid2])
      col2   <- c(col[     -1, -ngrid2])
      col3   <- c(col[     -1,      -1])
      col4   <- c(col[-ngrid1,      -1])
      ind1   <- !is.na(yg1 + yg2 + yg3)
      ind2   <- !is.na(yg1 + yg3 + yg4)
      xg     <- c(c(rbind( xg1,  xg2,  xg3)[, ind1]), c(rbind( xg1,  xg3,  xg4)[, ind2]))
      yg     <- c(c(rbind( yg1,  yg2,  yg3)[, ind1]), c(rbind( yg1,  yg3,  yg4)[, ind2]))
      zg     <- c(c(rbind( zg1,  zg2,  zg3)[, ind1]), c(rbind( zg1,  zg3,  zg4)[, ind2]))
      colg   <- c(c(rbind(col1, col2, col3)[, ind1]), c(rbind(col1, col3, col4)[, ind2]))
      ind3   <- is.na(yg3) & !is.na(yg1 + yg2 + yg4)
      xg     <- c(  xg, c(rbind( xg1,  xg2,  xg4)[, ind3]))
      yg     <- c(  yg, c(rbind( yg1,  yg2,  yg4)[, ind3]))
      zg     <- c(  zg, c(rbind( zg1,  zg2,  zg4)[, ind3]))
      colg   <- c(colg, c(rbind(col1, col2, col4)[, ind3]))
      ind4   <- is.na(yg1) & !is.na(yg2 + yg3 + yg4)
      xg     <- c(  xg, c(rbind( xg2,  xg3,  xg4)[, ind4]))
      yg     <- c(  yg, c(rbind( yg2,  yg3,  yg4)[, ind4]))
      zg     <- c(  zg, c(rbind( zg2,  zg3,  zg4)[, ind4]))
      colg   <- c(colg, c(rbind(col2, col3, col4)[, ind4]))
      a      <- scaling(xg, yg, zg)
      id1    <- triangles3d(a$x, a$y, a$z, col = colg, alpha = alpha, lit = lit,...)
      
      xg1    <- rep(ep1[-ngrid1], ngrid2)
      xg2    <- rep(ep1[     -1], ngrid2)
      xg3    <- rep(ep1         , each = ngrid2 - 1)
      xg4    <- rep(ep1         , each = ngrid2 - 1)
      zg1    <- rep(ep2         , each = ngrid1 - 1)
      zg2    <- rep(ep2         , each = ngrid1 - 1)
      zg3    <- rep(ep2[-ngrid2], ngrid1)
      zg4    <- rep(ep2[     -1], ngrid1)
      yg1    <- c(surf[-ngrid1, ])
      yg2    <- c(surf[     -1, ])
      yg3    <- c(t(surf[     , -ngrid2]))
      yg4    <- c(t(surf[     ,      -1]))
      col1   <- c(col.mesh[-ngrid1,        ])
      col2   <- c(col.mesh[     -1,        ])
      col3   <- c(t(col.mesh[     , -ngrid2]))
      col4   <- c(t(col.mesh[     ,      -1]))
      ind1   <- !is.na(yg1 + yg2)
      ind2   <- !is.na(yg3 + yg4)
      xg     <- c(c(rbind( xg1,  xg2)[, ind1]), c(rbind( xg3,  xg4)[, ind2]))
      yg     <- c(c(rbind( yg1,  yg2)[, ind1]), c(rbind( yg3,  yg4)[, ind2]))
      zg     <- c(c(rbind( zg1,  zg2)[, ind1]), c(rbind( zg3,  zg4)[, ind2]))
      colg   <- c(c(rbind(col1, col2)[, ind1]), c(rbind(col3, col4)[, ind2]))
      a      <- scaling(xg, yg, zg)
      id2    <- segments3d(a$x, a$y, a$z, col = colg, alpha = alpha.mesh, lit = lit, ...)

      invisible(c(id1, id2))
      }
