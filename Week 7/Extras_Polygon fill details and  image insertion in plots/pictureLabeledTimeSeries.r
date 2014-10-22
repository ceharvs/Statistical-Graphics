Picture label time series 

This is to suggest the idea.  There are
other implementations.  


0.  Setup 
# Install package jpeg
library(jpeg)
source("panelFunctions.r")

# Read in a pictre
danPicType <- image_type(source="Dan-S.jpg")
danPic <- readJPEG(source="Dan-S.jpg")
dim(danPic)
# 300 200  3   x is 200  y is 300

windows(w=6.5,h=9)
pan <- panelLayout(nrow=10,ncol=2,borders=rep(.1,4),
       topMar=.4,rightMar=.5,rowSep=.05,
       colSize=c(1,10))

# Row 1 with image and data 
panelSelect(pan,1,1)
panelScale()

# Scaling the panel for the image rasters
# and then inserting the image
panelScale(rx=c(0,201),ry=c(0,301))
rasterImage(danPic,.5,.5,200.5,300.5)

panelSelect(pan,1,2)
panelScale(c(2007.5,2014.5),c(170,245))
panelFill(col=gray(.9))
panelGrid(x=2008:2014,y=c(190,210,230),col="white")
axis(side=4,at=c(190,210,230),label=TRUE,tck=0,mgp=c(2,.4,0),
  las=1, cex.axis=.75)

x <- 2008:2014
y <- c(220,235,205,190,181,195,202)
points(x,y,type='o',col='blue',pch=16, cex=1.2,lwd=2)
panelOutline()

for(i in 2:9){
  panelSelect(pan,i,1)
  panelScale()
  panelFill(col="white")
  panelOutline()
}

for(i in 2:9){
 panelSelect(pan,i,2)
 panelScale(c(2007.5,2014.5),c(170,245))
 panelFill(col=gray(.9))
 panelGrid(x=2008:2014,y=c(190,210,230),col="white")
 panelOutline()
 axis(side=4,at=c(190,210,230),label=TRUE,tck=0,mgp=c(2,.4,0),
  las=1, cex.axis=.75)
}
axis(side=1,at=2008:2014,label=TRUE,tck=0,mgp=c(2,0,0), cex.axis=.75)





