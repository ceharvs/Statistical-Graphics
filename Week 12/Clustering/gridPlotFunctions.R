
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



