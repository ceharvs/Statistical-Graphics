# By Dan Carr
# Version date 10/15/2014
# See confidentClassMapDebug.r

confidentClass3WayMap <- function(
dFrame, 
refRegion = FALSE,
rowVar=1, colVar=2, colorVar=3,
rowLab="Mathematics Grade 8", 
colLab="Mathematics Grade 4",
colorLab="Reading Grade 8",
title ="2009 State Average NAEP Scores Compared to Virginia",
myGray = gray(.70),
myFill = "#D0FFFF",
colorFill = c("#AF5FFF", gray(.815), "#40D040"),
colorLine = "black",
backFill= rgb(1,1,.9),
backLine = gray(.70),
panelFill = rgb(.9,1,1),
panelLine = gray(.70), 
cexTitle=1.2,
cexLabel=1.12,
barSep=.29){ 

# 1. Define panel Layout_____________________________________

panels <- panelLayout(nrow=3, ncol=3,
  borders=rep(.2,4),
  topMar=1, bottomMar=.6,
  leftMar=.7, rightMar=0)

# 2. Set map scales  

# note na.rm removes missing values
rx <- range(stateVisBorders$x, na.rm=T) 
ry <- range(stateVisBorders$y, na.rm=T)

rx <- mean(rx)+ diff(rx)*1.06*c(-.5, .5)
ry <- mean(ry)+ diff(ry)*1.06*c(-.5, .5)

# 3.  Draw Maps

stateName <- row.names(dFrame)
statePolyId <- stateVisBorders$st[is.na(stateVisBorders$x)]

if(!is.na(refRegion)) {
  dFrame[refRegion,c(rowVar,colVar,colorVar)] <- 2
}

for(i in 1:3){
for(j in 1:3){
  panelSelect(panels, i, j) 
  panelScale(rx, ry)
  panelFill(col=panelFill) # panel fill
  panelOutline(col=panelLine)
  polygon(stateVisBorders$x,  stateVisBorders$y, col=backFill,
    border=backLine, lwd=1) # fill and outline
  fore <- 4-i==dFrame[, rowVar] & j==dFrame[, colVar]  
  if(any(fore)){
    foreNam <- stateName[fore]
    goodBnd <- !is.na(match(stateVisBorders$st, foreNam))
    goodPolyId <- !is.na(match(statePolyId, foreNam))
    subs <- match(statePolyId[goodPolyId], stateName)
    polyCol <- colorFill[dFrame[subs, colorVar]]
   
    # plot states in gray with white outlines
    polygon(stateVisBorders$x[goodBnd], stateVisBorders$y[goodBnd],
      col=polyCol, border="black", lwd=1) # fill and outline
   }
}
}

# 4. Highlight reference state if any

goodBnd = stateVisBorders$st==refRegion
if(any(goodBnd)){
  panelSelect(panels,2,2)
  panelScale(rx,ry)
  polygon(stateVisBorders$x[goodBnd],
    stateVisBorders$y[goodBnd],
    col=colorFill[2],border="black",lwd=2) # fill and outline
}

# 5. Labeling

# top___________________________________________________

panelSelect(panels, mar="top")
panelScale(inches=TRUE)

bnds <- panels$coltabs[3,]

xl <- bnds[1]
xr <- bnds[2]
xm <- mean(bnds)
topCenX <- c(xl, xm, xr)

sep <- barSep
topRectX <- c(xl+sep, xm, xr-sep)
text(topRectX, rep(.35, 3),
  c("Below", "Similar To", "Above"), adj=.5, cex=cexLabel)
wid <- c(.29, .42, .29)
rect(topRectX-wid, rep(.08, 3), 
  topRectX+wid, rep(.23, 3), col=colorFill)
text(xm,.57, colorLab, adj=.5, cex=cexLabel)
text(xm, .87, title, adj=.5, cex=cexTitle) 

# bot__________________________________________________

panelSelect(panels, mar="bottom")
panelScale(inches=TRUE)

botX <- apply(panels$coltabs[2:4, ], 1, mean)
text(botX, rep(.49, 3), 
  c("Below", "Similar To", "Above"), adj=.5, cex=cexLabel)
text(botX[2], .24, colLab, adj=.5, cex=cexLabel)

# left______________________________________________

panelSelect(panels, mar="left")
panelScale(inches=TRUE)

leftY <- rev(apply(panels$rowtabs[2:4, ], 1, mean))
text(rep(.53, 3), leftY, srt=90, c("Below", "Similar To", "Above"), 
  adj=.5, cex=cexLabel)
text(rep(.23, 3), leftY[2], srt=90, 
  rowLab, adj=.5, cex=cexLabel)

}

