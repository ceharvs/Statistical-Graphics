parabolaTree <- function(clus, lab, 
  padF = 0.04, parabolaF = 1/12, labelF = .15, nameGapF=.01,
  title="Cluster Tree", cex = .75, titleCex = 1.1,
  col.grid=gray(.65), col.line="#FF0000", col.outline=gray(0),
  xGridLines, parabolaRes=50, padY = .5)
{

# clus:       hclust object
# lab:        case labels, default to integers
# padF:       extends the x-axis scale on both sides
# parabolaF:  x-scale fraction for the parabola
#               height at first lowest merge 
# labelF:     x-scale fraction for labels 
# nameGapF:   x-scale fraction for the gap
#               for parabola to label
# xGridlines: locations, computes a default 
# cex:        case label font size
# titleCex:   title label font size
# parabolRes: number of points in polylines
# padY:       y-scale limits (1-padY, ncase+padY)

# The parabola calculation algorithm was originally
# written with y as merge height, x as groups id.
# The result was subsequently transposed for display
# Know this helps clarify variable names used
# in that part of the script.   

  x <- match(1:length(clus$order), clus$order)
  h <- clus$height
  hR <- range(clus$height)
  dropX <- parabolaF*diff(hR)
  padX <- padF*diff(hR)
  xsh <- nameGapF*diff(hR)
  leftX <- (parabolaF + nameGapF + labelF) * diff(hR)
 
  join <- clus$merge
  joinN <- nrow(join)
  ngrp.x <- rep(NA, joinN)
  ngrp.y <- h

  if(missing(lab)) lab <- as.character(1:(joinN + 1))
  rx <- c(hR[1] - leftX- padX,hR[2] + padX)
  ry <- c(1-padY, joinN + 1 + padY)
  panelScale(rx, ry)
  if(missing(xGridLines)) {
     xGridLines <- panelInbounds(rx)
     xGridLines <- xGridLines[xGridLines > min(h)]
  }
  axis(side=1,at=xGridLines,tck=1,labels=F,
    col = col.grid)
  axis(side=1,at=xGridLines,tck=0,mgp = c(1,.1,0),
    cex.axis=cex)
  mtext(side=1, line=1.2,"Merge Height",cex=cex)
  mtext(side=3, line=.5, title, cex=titleCex)
    
  grp1 <- join[, 1]
  grp2 <- join[, 2]
  for(i in 1:joinN) {   
    g1 <- grp1[i]
    g2 <- grp2[i]
    if(g1 < 0.) {
	g1 <- abs(g1)
	grp1.x <- x[g1]  # ith gene
      grp1.y <- h[i] - dropX 
      text(grp1.y - xsh, grp1.x, lab[g1],
        adj = 1, cex = cex, col = 1.)
    } else {
      grp1.x <- ngrp.x[g1]
      grp1.y <- ngrp.y[g1]
    }
    if(g2 < 0.) {
	g2 <- abs(g2)
	grp2.x <- x[g2]
	grp2.y <- h[i] - dropX
	text(grp2.y - xsh, grp2.x, lab[g2],
        adj = 1., cex= cex, col = 1)
    } else {
	  grp2.x <- ngrp.x[g2]
	  grp2.y <- ngrp.y[g2]
    }
    ym <- h[i]
    k1 <- (grp1.y/ym - 2.)^2.
    k2 <- (grp2.y/ym - 2.)^2.
    c1 <- ifelse(k1 > 1.,  - sqrt(k1 - 1.),  - sqrt(1. - k1))
    c2 <- ifelse(k2 > 1., sqrt(k2 - 1.), sqrt(1. - k2))
    xm <- (c1 * grp2.x - c2 * grp1.x)/(c1 - c2)

    # save the new peak 
    #ngrp.x[i] <- (grp1.x + grp2.x)/2
    b <- (grp2.x - xm)/c2

    # plot the parabola
    ngrp.x[i] <- xm
    px <- seq(grp1.x, grp2.x, length = parabolaRes)
    py <- (-ym) * sqrt(1. + ((px - xm)/b)^2.) + 2. * ym

    # plot the lines 
    lines(py, px, col = col.line, lwd = 2)
  }
  panelOutline(col <- col.outline)
}

