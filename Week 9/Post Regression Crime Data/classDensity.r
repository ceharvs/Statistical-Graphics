classDensity <-
  function(x, bw="nrd0", adjust=1, kernel="g",
    main="Density Plot", xlab="", ylab="", 
    cexL=1, cexS=.9,
    col=rgb(0, .85, 1), border=rgb(.3, .3, .3),
    alpha=.35){
 
  den = density(x ,bw=bw, kernel=kernel,adjust=adjust)
  xLoc <- den$x

  yLoc <- den$y

  n <- length(xLoc)
  yLow <- -max(yLoc)/3000 # Slightly less than zero
  xPoly <- c(xLoc[1], xLoc, xLoc[n])
  yPoly <- c(yLow, yLoc, yLow)

  plot(xLoc, yLoc, type='n', xlab='', ylab='',
  main="", las=1, axes=FALSE)
  usr <- par("usr")
  rect(usr[1], usr[3], usr[2], usr[4],
    density=-1, col=rgb(.88, .88, .88),
    border="black")
  grid(col="white", lty=1)
  polygon(xPoly, yPoly, density=-1, col=col, border=border)
  grid(col=rgb(1,1,1,alpha), lty=1)
  rect(usr[1], usr[3], usr[2], usr[4], density=0, border="black") 
  axis(side=1, tck=0, mgp= c(2, .1, 0), cex.axis=cexS)
  axis(side=2, tck=0, mgp= c(2, .2, 0), las=1, cex.axis=cexS)
  mtext(side=1, line=1.8, xlab, cex=cexS)
  mtext(side=2, ylab, cex=cexS)
  mtext(side=3, line=.5, main , cex=cexL, font=2)
}
 
