File        NP_Confident Class Comparison Map.r				    
By          Daniel B. Carr
Copyright   2010, 2011, 2012, 2013 

Due         The pdf file.  

            Prototype script
            Labels on left rather the right
            Color variants: gray states 
                            white borders
            Reference variant: compared to VA

Uses        NP_NAEP_MergedDat.csv or
            VA_NAEP_MergedDat.csv
            stateVBorders.csv
            nationVBorders.csv

            This will work for Virginia if you 
            change the prefix below and
            hand edit the top title in Section 5
            to say Virginia instead of National
            Public.  (With a little more time
            this could be automated)
            
            If other states are of interest
            you can get the data.  2011
            data is available.

## Run from here to end of file    

# 1.read the data, select variables, set output 

prefix <- "NP_"  # "VA_"
fin <- paste(prefix,"NAEP_MergedDat.csv",sep='')
tmpDat <- read.csv(fin, row.names=1)

colnames(tmpDat)

pdfFile <- paste(prefix,
  "Confident Comparison Class Map.pdf",sep='')
# pdfFile <- NULL

mapDat <- tmpDat[, c(1, 3, 7)]

refRegion = "NP"
if(prefix!="NP_"){
 stId <- substring(prefix,1,2)
 refRegion <- stId
 i <- which(row.names(mapDat)==stId)
 mapDat[i,] <- c(2,2,2)
}

# 2. read Region boundary files set map xy limits

stateVBorders <- read.csv('stateVBorders.csv',
  row.names=NULL, header=T, as.is=TRUE)
statePolyId <- stateVBorders$st[is.na(stateVBorders$x)]
nationVBorders <- read.csv('nationVBorders.csv',
   blank.lines.skip=F, row.names=NULL, header=T)
names(stateVBorders) # st= state ids, x, y polygon coordinates  
names(nationVBorders)   # 

# note na.rm removes missing values
rx <- range(stateVBorders$x, na.rm=T) 
ry <- range(stateVBorders$y, na.rm=T)

rx <- mean(rx)+ diff(rx)*1.06*c(-.5, .5)
ry <- mean(ry)+ diff(ry)*1.06*c(-.5, .5)

# 3. Define panel Layout_____________________________________
width=9.7
height=7.1

if(!is.null(pdfFile)){
  pdf(width=width, height=height, file=pdfFile)
} else {
  windows(width=width, height=height)
}

topMar= 1
bottomMar <- .6
leftMar <- .7
rightMar <- 0
borders=rep(.2, 4)
nc <- 3
nr <- 3

panels <- panelLayout(nrow=nr, ncol=nc,
  borders=borders,
  topMar=topMar, bottomMar=bottomMar,
  leftMar=leftMar, rightMar=rightMar)

# 4.  Draw Maps

stateName <- row.names(mapDat)

myGray <- rgb(.5, .5, .5)
myColor <- c("#AF5FFF", "#D0D0D0", "#40D040")
for(i in 1:nr){
for(j in 1:nc){
  panelSelect(panels, i, j) 
  panelScale(rx, ry)
  panelFill(col="#D0FFFF") # panel fill
  panelOutline(col="gray")
  polygon(stateVBorders$x,  stateVBorders$y, col="white",
    border="gray", lwd=1) # fill and outline
  panCol <- myColor[mapDat[, 3]]
  fore <- 4-i==mapDat[, 2] & j==mapDat[, 1]  
  if(any(fore)){
    foreNam <- stateName[fore]
    goodBnd <- !is.na(match(stateVBorders$st, foreNam))
    goodPolyId <- !is.na(match(statePolyId, foreNam))
    subs <- match(statePolyId[goodPolyId], stateName)
    polyCol <- myColor[mapDat[subs, 3]]
   
    # plot states in gray with white outlines
    polygon(stateVBorders$x[goodBnd], stateVBorders$y[goodBnd],
      col=polyCol, border="black", lwd=1) # fill and outline
   }
}
}

# highlight reference state if any
goodBnd = stateVBorders$st==refRegion
if(any(goodBnd){
  panelSelect(panels,2,2)
  panelScale(rx,ry)
  polygon(stateVBorders$x[goodBnd],
    stateVBorders$y[goodBnd],
    col=polyCol,border="black",lwd=2) # fill and outline
}

5. Labeling

# top___________________________________________________

panelSelect(panels, mar="top")
panelScale(inches=TRUE)

# Coordinate experiments (could get from panel Layout)
xl <- 2.88 + leftMar
xr <- 5.73 + leftMar
xm <- mean(c(xl, xr))
topCenX <- c(xl, xm, xr)
#points(topCenX, rep(.08, 3), pch=21, bg='red', cex=1.25) 

text(xm, .59, 
  "Reading Average Scores: Grade 8, 2009", cex=1.12)
text(xm, .9,
  "States Compared to National Public", adj=.5, cex=1.2) 

sep <- .67
topRectX <- c(xl+sep, xm, xr-sep)
text(topRectX, rep(.38, 3),
  c("Below", "Similar To", "Above"), adj=.5, cex=1.12)
wid <- c(.27, .40, .27)
rect(topRectX-wid, rep(.08, 3), 
  topRectX+wid, rep(.27, 3), col=myColor)


# bot__________________________________________________

panelSelect(panels, mar="bottom")
panelScale(inches=TRUE)

botX <- apply(panels$coltabs[2:4, ], 1, mean)
text(botX, rep(.48, 3), 
  c("Below", "Similar To", "Above"), adj=.5, cex=1.12)
text(botX[2], .18,
  "Mathematics Average Scores: Grade 4,  2009", adj=.5, 
  cex=1.12)

# left______________________________________________

panelSelect(panels, mar="left")
panelScale(inches=TRUE)

leftY <- rev(apply(panels$rowtabs[2:4, ], 1, mean))
text(rep(.53, 3), leftY, srt=90, c("Below", "Similar To", "Above"), 
  adj=.5, cex=1.12)
text(rep(.23, 3), leftY[2], srt=90, 
  "Mathematics Average Scores: Grade 8,  2009", adj=.5, cex=1.12)

if(!is.null(pdfFile))dev.off()


