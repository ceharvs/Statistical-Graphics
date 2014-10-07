# Matrix panel functions 
# From R CookBook

panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...){
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x,y, use="complete.obs"))
  txt <- format( c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep='')
  if (missing (cex.cor))cex.cor <- .8/strwidth(txt)
  text(0.5,0.5,txt, cex = cex.cor*(1.3 + abs(r))/2) 
}

panel.hist <- function (x, ...){
  usr <- par("usr")
  on.exit(par(usr))
  par(usr=c(usr[1:2], 0, 1.5))
  h <- hist(x, plot=FALSE)
  breaks <- h$breaks
  nB <- length(breaks)
  y <- h$counts
  y <- y/max(y)
  rect(breaks[-nB],0, breaks[-1],y,col=rgb(.9,1,.9),...)
}

# Appears to work not verify working 
panel.persp <- function (x,y,col=par("col"),bg=NA,pch=par("pch"),
  cex=1,col.smooth="black",...){
  dat <- cbind(x,y)
  bin2d <- bin2(dat, nbin=c(34, 34))
  bin2d.sm <- ash2(bin2d)
  rx <- range(bin2d.sm$x)
  ry <- range(bin2d.sm$y)
  usr <-par("usr")
  par(usr=c(rx,ry),new=TRUE)
  on.exit(par(usr))
  persp(bin2d.sm,xlab='',
  col=rgb(.5,.8,1), r=4, phi=40, shade=.2)
}

panel.lm <- function (x, y, col=par("col"), bg=NA,
  pch=par("pch"), cex=1, col.smooth="black",...){
  points(x, y, pch=pch, col=col, bg=bg, cex = cex)
  abline(stats::lm(y~x), col=col.smooth, ...)
}



