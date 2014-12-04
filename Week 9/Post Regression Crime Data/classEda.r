classEda <- function(
  x, 
  lab1 = "",
  lab2 = "",
  units= "Variable Units",
  bw="nrd0", adjust=1,kernel="g",
  dist = "norm", QQxlab="Normal Quantiles",

  textLines = c(.6,.5,.4),
  textFonts = c(2,2,1),  
  cexS = 1, cexL = 1.3,
  panelCol = rgb(.95, 1.00, .95),
  densityCol = rgb(0,.85,1),
  densityLineCol = rgb(.3,.3,.3)
)

#  Debug
#  bw="nrd0"; adjust=1;kernel="g"
#  dist = "norm"; QQxlab="Normal Quantiles"
#  lab1 = "Variable Label 1"
#  lab2 = "Variable Label 2"
#  units = "Variable Units"
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
  labs <- c(lab1,lab2,units)
  cex <- c(cexL,cexL,cexS)
  text(.5,textLines, labs, cex=, font=textFonts)

  qqPlot(x, dist=dist, las=1, 
    xlab= QQxlab, ylab=units,
    main="Q-Q Plot")

  classDensity(x, col=densityCol,
    border=densityLineCol, xlab=units) 

  boxplot(x, horizontal=TRUE, col=densityCol,
    tck=.05, mgp=c(2, 0, 0), main="Box Plot", xlab=units)
 
  par(oldpar)
}

#windows(w=7,h=7)
#x <- rnorm(100, mean=100, sd=16)
#classEda(x)

#windows(w=5,h=5)
#x <- rexp(100)
#classEda(x,dist="exp",QQxlab="Exponential Quantiles")

 

