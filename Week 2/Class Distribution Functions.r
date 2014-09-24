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

classEda <- function(
  x, 
  bw="nrd0", adjust=1,kernel="g",
  dist = "norm", QQxlab="Normal Quantiles",
  varLab1 = "Variable Label 1",
  varLab2 = "Variable Label 2",
  varUnits = "Variable Units",
  textLines = c(.7,.5,.2),
  textFonts = c(2,2,1),  
  cexS = 1, cexL = 1.3,
  panelCol = rgb(.95, 1.00, .95),
  densityCol = rgb(0,.85,1),
  densityLineCol = rgb(.3,.3,.3)
)

#  Debug
#  bw="nrd0"; adjust=1;kernel="g"
#  dist = "norm"; QQxlab="Normal Quantiles"
#  varLab1 = "Variable Label 1"
#  varLab2 = "Variable Label 2"
#  varUnits = "Variable Units"
#  textLines = c(.7,.5,.2)
#  textFonts = c(2,2,1)  
#  cexS = 1; cexL = 1.3
#  panelCol = rgb(.95, 1.00, .95)
#  densityCol = rgb(0,.85,1)
#  ensityLineCol = rgb(.3,.3,.3)

{ require(car)
  layout(mat=matrix(1:4 , ncol=2))
  oldpar <- par(mai=c(.72, .68, .4, .2))
  plot(c(0, 1), c(0, 1), axes=FALSE, type='n',
    xlab='', ylab='', main='')
  usr <- par("usr")
  rect(usr[1], usr[3], usr[2], usr[4], density=-1,
    col=panelCol, border="black")
  labs <- c(varLab1,varLab2,varUnits)
  cex <- c(cexL,cexL,cexS)
  text(.5,textLines, labs, cex=, font=textFonts)

  qqPlot(x, dist=dist, las=1, 
    xlab= QQxlab, ylab=varUnits,
    main="Q-Q Plot")

  classDensity(x, col=densityCol,
    border=densityLineCol, xlab=varUnits) 

  boxplot(x, horizontal=TRUE, col=densityCol,
    tck=.05, mgp=c(2, 0, 0), main="Box Plot", xlab=varUnits)
 
  par(oldpar)
}

#windows(w=7,h=7)
#x <- rnorm(100, mean=100, sd=16)
#classEda(x)

#windows(w=5,h=5)
#x <- rexp(100)
#classEda(x,dist="exp",QQxlab="Exponential Quantiles")
 
classFunction <-
  function(x, y, yline=2,
    main="Density Plot", xlab="", ylab="", 
    cexL=1, cexS=.9,
    col=rgb(0, .85, 1), border=rgb(.3, .3, .3),
    alpha=.35){
  
  xLoc <- x
  yLoc <- y

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
  mtext(side=2, line=yline, ylab, cex=cexS)
  mtext(side=3, line=.5, main , cex=cexL, font=2)
}

classEcdf <- function(x){
  q <- sort(x)
  n <- length(x)
  jump <- rep(1/n, n)
  p <- cumsum(jump)

  q <- c(q[1], q)  # add 0 prob at first quantile (technically just to the left) 
  p <- c(0, p)

  gPlot(q, p, type='n', xlab='', ylab='Cumulative Probabilities',
     main='Empirical Cumulative Distribution Function')
  mtext(side=1, line=1.7, "Sorted Data") # better spacing x axis
  lines(q, p, type='s', col='black', lwd=3)
  dx <- .03*diff(range(q))
  arrows(q[n+1], 1, q[n+1]+dx, 1, lwd=3, length=.1, col='red')
  arrows(q[1], 0, q[1]-dx, 0, lwd=3, length=.1, col='red')
}




gridPlot = function(x,y,type,main,xlab,ylab,
                    fillCol = "#D0D0D0",
			  gridCol = "white",
                    outlineCol= "black",
			  gridLwd = 2,
			  outlineLwd = 1,
                    titleCex,
			  axisLabelsCex,
			  gridLabelsCex,
                    lineTitle=1.5,
			  lineXlabel = 2,
			  lineYlabel = 2,
                    lineGXlabel= .3 ,
			  lineGYlabel = .5,
			  lasXlabel = 1,
			  lasYlabel = 0,
                    lasGXlabel = 1,
			  lasGYlabel = 1,...)
 { 

   plot(x,y,type='n',main='',xlab='',ylab='',axes=F,...)
   graphPar = par()
   cex = graphPar$cex
   xyBnds = graphPar$usr
   if(missing(titleCex))titleCex=cex
   if(missing(axisLabelsCex))axisLabelsCex=cex  
   if(missing(gridLabelsCex))gridLabelsCex=cex


# Fill
   rect(xyBnds[1],xyBnds[3],xyBnds[2],xyBnds[4],
        col=fillCol,density=-1,border=NULL,xpd=T)  # Fill
# Grid lines
   axis(side=1,tck=1,labels=F,col=gridCol,lwd=gridLwd)
   axis(side=2,tck=1,labels=F,col=gridCol,lwd=gridLwd)

# Outline
   rect(xyBnds[1],xyBnds[3], xyBnds[2],xyBnds[4],
        col=outlineCol,density=0,border=T,lwd=outlineLwd,xpd=T) #Outline

#Grid line labels 
   axis(side=1,tck=0,mgp=c(2,lineGXlabel,0),lty=0,las=lasGXlabel)
   axis(side=2,tck=0,mgp=c(2,lineGYlabel,0),lty=0,las=lasGYlabel)

# Title and xlab and ylab
   if(!missing(main))mtext(main,side=3,line=lineTitle,cex=titleCex)
   if(!missing(xlab))mtext(side=1,line=lineXlabel,xlab,cex=axisLabelsCex,las=lasXlabel)
   if(!missing(ylab))mtext(side=2,line=lineYlabel,ylab,cex=axisLabelsCex,las=lasYlabel)

   if(missing(type))type='p'
   points(x,y,type=type,...)
}


gPlot = function(x,y,type,main,xlab,ylab,
                    fillCol = "#D0D0D0",
			  gridCol = "white",
                    outlineCol= "black",
			  gridLwd = 2,
			  outlineLwd = 1,
                    titleCex,
			  axisLabelsCex,
			  gridLabelsCex,
                    lineTitle=1.5,
			  lineXlabel = 2,
			  lineYlabel = 2,
                    lineGXlabel= .3 ,
			  lineGYlabel = .5,
			  lasXlabel = 1,
			  lasYlabel = 0,
                    lasGXlabel = 1,
			  lasGYlabel = 1,...)
 { 
   plot(x,y,type='n',main='',xlab='',ylab='',axes=F,...)
   graphPar = par()
   cex = graphPar$cex
   xyBnds = graphPar$usr
   if(missing(titleCex))titleCex=cex
   if(missing(axisLabelsCex))axisLabelsCex=cex  
   if(missing(gridLabelsCex))gridLabelsCex=cex

# Fill
   rect(xyBnds[1],xyBnds[3],xyBnds[2],xyBnds[4],
        col=fillCol,density=-1,border=NULL,xpd=T)

# Grid lines
xTicks = axTicks(side=1)
yTicks = axTicks(side=2)
dx = .005*diff(xyBnds[1:2])
dy = .005*diff(xyBnds[3:4])
   axis(side=1,tck=1,at=xTicks-dx,labels=F,col="gray",lwd=1)
   axis(side=2,tck=1,at=yTicks+dy,labels=F,col="gray",lwd=1)
   axis(side=1,tck=1,labels=F,col=gridCol,lwd=gridLwd)
   axis(side=2,tck=1,labels=F,col=gridCol,lwd=gridLwd)

# Outline
   rect(xyBnds[1],xyBnds[3], xyBnds[2],xyBnds[4],
        col=outlineCol,density=0,border=T,lwd=outlineLwd,xpd=T) #Outline

#Grid line labels 
   axis(side=1,tck=0,mgp=c(2,lineGXlabel,0),lty=0,las=lasGXlabel)
   axis(side=2,tck=0,mgp=c(2,lineGYlabel,0),lty=0,las=lasGYlabel)

# Title and xlab and ylab
   if(!missing(main))mtext(main,side=3,line=lineTitle,cex=titleCex)
   if(!missing(xlab))mtext(xlab,side=1,line=lineXlabel,cex=axisLabelsCex,las=lasXlabel)
   if(!missing(ylab))mtext(ylab,side=2,line=lineYlabel,cex=axisLabelsCex,las=lasYlabel)

   if(missing(type))type='p'
   if(type!="n")points(x,y,type=type,...)
}



