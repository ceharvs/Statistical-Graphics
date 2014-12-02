File        Regression and smoothing 2D domain.r
By          Daniel B. Carr

Topic       Regression, smoothing, graphics, and diagnostics
See         Class companion notes             

Data        Abrasion Loss
            For a discussion of the data see the Visualizing
            Data by William Cleveland 1993
		
Sections    1.  Read the abrasion loss data

            2.  Exploratory 1-D distribution views of variables

            3.  Scatterplot matrices with smooths
                and linked point selection

            4.  A point leverage example

            5.  An introduction to linear models and basic output

            6.  Common regression diagnostics 
            6.1 Built in diagnostic plots
            6.2 Influential and high leverage cases
            6.3 Large studentized Residuals
            6.4 Spread-Location plot
            6.5 Normal Q-Q plot
            6.6 Partial residual or added variable plots
                Regular and detrended

            7.  A loess smooth model with contour and surface views 
            7.1 Contour plot
            7.2 Surface perspective view
            7.3 Lattice graphics for surfaces

            8. Conditioned plots for surface examination

            9. Conditioning for more than three variables:
               discussion


Due: 9pts   2.  One plot of your choice
            3.  The plot
            4.  The plot
            6.1 One plot of your choice
            6.2 The plot  
            6.4 The spread-location plot
            6.5 The detrended partial residual plot
                    for adjusted tensile strength
            7.3 A draped wireframe plot, your choice and
                the contour plot
            8.  The coplot of abrasion loss versue tensile
                strength given hardness
 
Uses:       source:  classDensity, classEda.r, panelFunctions.r
            packages: car, 

Future:     Additions: rotating 3d scatterplot and rgl views
   
0. Setup

source("classEDA.r")
source("classDensity.r")
library(lattice)
source("panelLayoutFunctions.r")

1.  Read the abration loss data

##Run       

abrLossDat <- read.table('abrasionLoss.txt') 
colnames(abrLossDat)=c('hardness','tenStrength','abrLoss') 
abrLossDat   # look at the values

##End___________________________

There are 30 rubber specimens indexed by the
26 letters of the alphabet and the number 1-4.   

As indicated in Cleveland book Visualizing Data,

Tensile strength:  "The force per unit of cross-sectional
area required to break a specimen.  The units are
kilograms per centimeter squared, kg/cm**2

Hardness:  The bound height of a steel indenter dropped
onto a specimen.  The units are degrees Shore

Abrasion loss:  "the amount of material abraded
from a specimen per unit of energy expended in the
rubbing."  The units are grams per horsepower hour,
gr/hp-hour.  

## Run look univariate summary statistics

summary(abrLossDat)

## End__________________________

2. Exploratory 1-D distribution views of variables

##Run
lab1 <- c('Hardness','Tensile Strength','Abrasion Loss')
units <-c('kg/cm**2','degrees Shore','gr/hp-hour')
 
for (i in 1:ncol(abrLossDat)){
  windows()
  classEda(abrLossDat[,i],lab1=lab1[i],units=units[i])
}

##End_______________________

The windows, one per variable, are overplotted!
Move the ones on top to see the ones below.

In the Q-Q plots note the suggestion of
Abrasion loss:    thick right tail
Hardness:         thin tails 
Tensile strength: thin tails

3.  Scatterplot matrices with smooths
    and linked point selection 

##Run

windows(width=6.5,height=7)
par(las=1)
pairs(abrLossDat,row1attop=FALSE,
  gap=0, lwd=2, pch=21, col=rgb(0,.5,.1),
  bg=rgb(0,.5,1),cex=1.3,
  labels=c("Hardness\n  (kg/cm**2)",
    "Tensile Stren.\n (degrees shore)",
    "Abrasion Loss\n (g/hp-hour)"),
  main=c("Abrasion Loss Data"),
  panel=panel.smooth)

## End_________________________________

In the top left panel x = Hardness and y = Abrasion.   
We see a nearly linear inverse relationship 
between hardness and abrasion loss.

In the top middle panel, y = Abrasion Loss and
x = Tensile Strength, we see an abrasion loss
bump centered at about 160 degrees shore for
tensile strength.   

Mentally assessing what will happen when
fitting both hardness and tensile strength
from the two plots is not so easy.   

In the second row left panel
x = hardness and y = tensile strength.
This panel shows the domain for the regression
predictor variables. Note the three points at
the bottom right corner. A thin horizontal
rectangle across the panel containing these
three points has no additional points.  
A thin vertical rectangle above the three points
may contain 1 point near the top. Predicting
abrasion loss for values in the empty space
of the two rectangles will be suspect since there
is little data to support the prediction.

Since the three points are at the edges of the 
model domain they will have high leverage. Their
leverage will allow them to distort the model
more than other points if their dependent
variable values are far from the true underlying
model.  The two low hardness values on the left
will also have substantial leverage.  See how
the smooth curve in the panel above has change
slope to better fit the two values.     

Side note.
     There several ways assess the influence of a point
     on the model. The involves both the domain
     based leverage and the value of the dependent
     variable. One approach is to see how much the
     regression coefficients change when the point
     is omitted.  Cook's distance is one way of
     assessing this.  

     We do not dig into this topic but will show
     regression common regression diagnostics graphics,
     that will call attention to extreme residuals,
     high leverage points, and influential points. 

Now look at the transpose of the plot in the
middle column of the bottom row. Note the 
two low hardness points near the bottom. Now look
at the peak of the red smooth in the top plot
of the column of plots.  There are three points
with high abrasion loss above the peak.  Do two
of these points correspond to the two lowest
hardness points?    
  
## Run

library(lattice) 

splom(abrLossDat,pscale=0,col=rgb(.4,.4,.4))
trellis.focus("panel",1,1,highlight=FALSE)
panel.link.splom(pch=16,col="red")
trellis.unfocus()

## End______________________________


Left click on the lowest two hardness points.  
Then right mouse in the panel and select stop.  

Yes, the two points had high abrasion loss.
Now the R Console should indicate you
selected points 1 and 25. 

Look at the two red points top left panel and then
down the red points in the panel below. Since
the two points are at the end of hardness scale
they the points have more than there fair share
of leverage in the regression. That is,the regression
may change substantially if the were omitted. 
If all value associated with high leverage points
are correct and have small errors we will likely want
to use them in the model.  A problem is that we
don't regenerally know if this is the case.  

Classical regression assumes there are errors in the
dependent variable (abrasion loss in this case).
It assumes there are no errors in the explanatory or 
predictor variables.   

When there are errors in the explanatory 
variables statisticians called it the error
in variables problem. 
 
There are no really good solutions to the problem, 
that don't involve additional data. Some options are
1) Say the regression is conditional given the
   explanatory (independent) variable values
2) Use principle curve regression  
3) Make some special assumptions.
   Apply singular value decomposition
   and use the left eigenvectors associated
   with largest eigenvalues as the independent
   variables. This hopes the contribution
   of bad values emerge primarily in the 
   eigenvectors with low eigenvalues  
     
4. A point leverage example____________________

##Run
x <- rnorm(50)
y <- 2*x+3

# Add high leverage points
# and let y = -2x+3 for them 
x <- c(x,-10,10)
y <- c(y,23,-17)

#linear model
# ~ read as: is modeled by 
# y is modeled by x 
model <- lm(y ~ x)
summary(model)

# model coefficients 
model$coef

#(Intercept)           x 
#   3.634926   -1.427085 

plot(x, y,
  main="Linear regression with two bad high leverage points")
points(x[51:52],y[51:52],pch=21,col="red",bg="red")
abline(model$coef)

##End___________________________

Two high leverage points have changed a perfect slope of 2
for the first 50 points into a slope of  -1.43 
 
5. An introduction to linear models and basic output________

In the linear modeling function lm() below
   data = indicates a data.frame with variables
	    that can specified by name
   AbrLoss appear to the left of ~ which is read "is modeled by".
   AbrLoss is the dependent variable being modeled.

   The "." indicates that all the remaining variables in the
   the data.frame are to be used as predictor variables.
	
   By default the model also fits the grand mean.

## Run
abrFit <- lm(abrLoss ~ . ,data=abrLossDat) 

# summary of the fit
abrSum <- summary(abrFit)
abrSum
## End______________________________

Abrasion Loss Model Summary ___________________

Call:
lm(formula = abrLoss ~ ., data = abrLossDat)

Residuals:
    Min      1Q  Median      3Q     Max 
-79.385 -14.608   3.816  19.755  65.981 

Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept)  885.1611    61.7516  14.334 3.84e-14 ***
hardness      -6.5708     0.5832 -11.267 1.03e-11 ***
tenStrength   -1.3743     0.1943  -7.073 1.32e-07 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

Residual standard error: 36.49 on 27 degrees of freedom
Multiple R-Squared: 0.8402,     Adjusted R-squared: 0.8284 
F-statistic:    71 on 2 and 27 DF,  p-value: 1.767e-11

End Model Summary___________________________

Comment on the Model Summary 

Estimate column
   The fitted linear model is 
   abrLoss = 855 -6.57 * hardness - 1.37 * tenStrength

Std. error column
   Estimate standard error

t value
   Ratio of estimate to standard error
   Similar to a z-score with a theoretical mean of zero
   when the error degrees of freedom is 30 or large

The Pr(>|t|) column 
    indicate categories of statistical significance levels
    using a two-sided t test.
    All three coefficients have p < .001

Residual standard error: 36.49 on 27 degrees of freedom
    The residual standard error is 36.49. Perhaps
    fitting other deterministic variables or stochastic
    components could drive this down.      

Multiple R-Squared: 0.8402 
    The Multiple R-Squared is ratio of the model predicted
    value sum of squares about the grand mean to the data sum
    of squares about the grand men.   

    This can be computed from the residual sum of squares (RSS)
    from the two models.  It is 
    1 - full_model_RSS/mean_only_RSS 

    Modeling in the physical science often produces
    R-Squared values bigger than .5 (or 50% if converted to a percent)
    R-Squared values in the social sciences are often much less than .5.
	
    A high R-squared value simply indicates a reduction in sum of
    squares of residuals.  It indicates neither causality,
    nor the applicability of the model to new data.  One can
    often increase the R-squared value by generating a random
    variable and putting it in the model. For this reason
    some evaluation criteria include a penalty for the number
    linear predictors used in the model.     

F-statistic: 71 on 2 and 27 degrees of freedom,
    the p-value is 1.767e-011 
    
     The small p value for F statistics says the model is fitting
     the data a lot better than just the grand mean.

     This suggests that the model reduction in sum of squares
     is very unlikely to have happen at random which would be
     if the predictor variables totally unrelated to abrasion loss.
   
Correlation of Coefficients:
            (Intercept) Hardness 
   hardness -0.8335             
tenStrength -0.7664      0.2992 

	The regression coefficients are almost always correlated.
	Here Hardness and Ten.strength have a correlation of .2992

6. Common regression diagnostics  
   
See R. Dennis Cook and Sanford Weisberg
  "Regression Diagnostics With Dynamic Graphics",
   Technometrics 31(3) 1989

   "Residuals and Influence in Regression", Chapman Hall 1982
   and class notes.

6.1 Built in diagnostic plots  

Today R has many built in diagnostic plots:

##Run   !!! Click in the active plot to move to the next plot
windows()  
plot(abrFit)
## 

Produces four plots
(1)  Residuals versus fitted values
     Candidate outliers are labeled 
(2)  Standardized residual normal Q-Q plot
     Standardization divides residuals by their estimated variance
     Asymmetry and thick tails are of concern 
(3)  Scale location plot using 
     y = sqrt(abs(standardized residuals))
     x = fitted values    
(4)  Standardized residuals versus leverage
     High leverage points strongly influence the fit
     If points have bad y values the residuals still may be small.
     (Sometimes it is a bad x values the produce high leverage.)  

Sections below revisit some of these plots

6.2 Influential and high leverage cases________________________  

## Run
abrInfluence <- influence.measures(abrFit)
infmat <-abrInfluence$infmat
head(infmat)
influential <- abrInfluence$is.inf
nr <- nrow(infmat)
nc <- ncol(infmat)
rownam <- rownames(infmat)
colnam <- colnames(infmat)

# Check calculation of Cook's D
# h <- infmat[,"hat"]
# p <- length(abrFit$coef)
# abrStanRes <- rstandard(abrFit)
# cooksD <- abrStanRes^2*h/(p*(1-h))
# cbind(cooksD,infmat[,"cook.d"]) # matches

windows(width=10,height=8)
ngrps <- (nr-1)%/% 4 + 1
pan <- panelLayout(nrow=ngrps,ncol=nc+1,
  colSize=c(.7,4,4,4,4,4,4,4),
  rowSize=c(rep(4,ngrps-1),nr%%4),
  topMar=.5,bottomMar=.5)
pan2 <- panelLayout(nrow=1,ncol=nc+1,
   colSize=c(.7,4,4,4,4,4,4,4),
   topMar=.5,bottomMar=.5)

for (i in 1:ngrps){
  panelSelect(pan,i,1)
  subs = (4*i-3):min(4*i,nr)
  y =length(subs):1
  panelScale(c(0,1),c(.3,length(y)+.7))
  text(.5,y,rownam[subs],adj=.5,cex=.9)
}

for (j in 1:nc){
   dat <- infmat[,j]
   suspect <- influential[,j]
   rx <- range(dat)
   rx <- mean(rx)+1.1*diff(rx)*c(-.5,.5)
   for (i in 1:ngrps){ 
      panelSelect(pan,i,j+1)
      subs <- (4*i-3):min(4*i,nr)
      x <- dat[subs]
      y <- length(subs):1
      col <- ifelse(suspect[subs],"red",rgb(0,.5,1))
      pch <- ifelse(suspect[subs],16,16)
      cex <- ifelse(suspect[subs],1.2,1)
      panelScale(rx,c(.3,max(y)+.7))
#      panelFill(ifelse(i%%2,'#D0D0D0','#C8C8FF'))
      panelFill(ifelse(i%%2,'#D7D7D7','#D7D7D7'))
      grid(lty=1,ny=NA, col="white")
      points(x,y,col=col,pch=pch,cex=cex)
      panelOutline(col="black")
      if(i==ngrps)axis(side=1,tck=0, mgp=c(2,.1,0))
#      if(i==ngrps)axis(side=1,tck=-.1,mgp=c(2,.3+.8*j%%2,0))
   }
}

for (j in 1:nc){
panelSelect(pan2,1,j+1)
panelScale()
panelOutline()
mtext(side=3,line=.3,colnam[j])
}

pan3 <- panelLayout(nrow=1,ncol=1,
                   topMar=.5,bottomMar=.5)
panelSelect(pan3,1,1)
panelScale()
title('Abrasion Loss Model: Influence Diagnostics')
## End

Cases A and S have statistics flagged as filled
red dots.  The above layout might background
filled vertical rectangles to indicate the regions
where the values are suspect.

6.3  Large studentized Residuals_____________________________

##Run

windows()

abrStRes <- rstudent(abrFit)  #studentized residuals	  
x <- seq(along=abrStRes)
y <- abrStRes
fives <- (x-1)%%5 + 1
plot(range(x),range(y),type='n',axes=FALSE,
	xlab='Case Sequence Number',
	ylab='Studentized Residuals',
	main="Look for Extreme Residuals",las=1,)
usr <-  par()$usr
rect(usr[1],usr[3],usr[2],usr[4],col="#D7D7D7")

grid(col="white",lty=1)
axis(side=1,tck=0,mgp=c(2,.1,0))
axis(side=2,tck=0,mgp=c(2,.2,0),las=1)
abline(h=c(-2,2),lwd=2,col='red')
box() 
col <- c("red","orange","#00B000","#0080FF","purple") 
points(x,y,pch=16,col=col,cex=1.7)
big <-  abs(y) > .9*2
text(x[big]+.03*diff(range(x)),y[big],as.character(x[big])) 

##End___________________________

6.4 Spread-Location plot_________________________________

Plot square root absolute residuals (y-axis)
versus predicted values (x-axis)

Look for horizontal smooth across the plot 
This may motivate transforming the data to
stabilize the variance. 

In some data sets there in a nonlinear
relationship between the the dependent variable
and the predictor variables.  A non-horizontal
smooth can also be a symptom of nonlinearity. 

We expect some variation in the smooth and
in takes some experience get comfortable about
wiggles that are likely of little importance.
There are more comments below.  
  

##Run
windows()
scatter.smooth(abrFit$fitted,sqrt(abs(abrFit$res)),
	xlab='Fitted Abrasion Loss',
	ylab='Square Root Abs(Studentized Residuals)',
	main='Spread Location Plot',las=1,pch=16,cex=1.2,col="red")
##End

A common regression concern is that variance of noise
is not constant but rather increases (or decreases)
with the magnitude of the dependent variable.

The residuals serve as a surrogates for noise and the
predicted or fitted values serve as a surrogates for the
underlying dependent variable values.    

The smooth in the plot does not look problematic.  
It is pretty flat.  If there is a problem, fairly 
often there is a data transformation to remove it.  
We don't have time pursue transforms but move
on to other topics.   

6.5  Normal Q-Q plot_______________________________________

Plot expected Normal Quantiles (x) versus
sorted studentized residuals (y)

Look for
Tail thickness and extreme points
	    
*For a positive dependent variable examine
plots for various values of Lamba in
a Box-Cox transformation (not explained here)

## Run

library(car)
qqPlot(abrStRes,las=1,
  main="Q-Q plots",
  ylab="Studentized Residuals",
  xlab="Standard Normal Quantiles")

## End

The left tail looks thick enough to cause some concern   

 
6.6  Partial residual or added Variable Plots______________
     Regular and Detrended

Variables are usually included in models
if their improvement to the fit is statistically
significant.  Some improvement may be due 
to one or a very few cases. This motivates
examining such cases to as the reliability
of their values.  Sometimes a variable or case
should be excluded from the model.   

Model deficiencies can also appear in the
form of added variable outliers or
high leverage points.
	
The construction of an added variable plot
is interesting. It regresses a selected 
predictor variable on all the other
predictor variables to obtain a residual
vector that is the predictor's 
unique contribution to the full
predictor vector space.  

The construction regresses dependent
variable on all the other predictor variables
to obtain a residual vector that represents
what has not been explained by the
other predictor variables.

The last construction step regresses
the dependent variable residual vector
on the predictor variable residual vector.
The slope coefficient for this model is
the same as the coefficient the predictor
variable when fitting all the predictor
variables.  However  the points in a scatterplot of
y = dependent variable residual vector versus
x = predictor variable residual vector
reveals which cases are important to fitting
structure that has not yet been modeled.

The procedure can be applied to all predictor
variables when there are not too many.  Some
of the linear regressions can be avoided as
the example script illustrates. 

Since it is easier to visually assess departures
from a horizontal line than from a regression line
line with non-zero slope, the script also
produces detrended added variable plots that remove the 
regression fit. Adding smoothes to this plot 
helps us seen a possible function relationships
in the residuals.

##Run  
# Create a matrix that omits the dependent variable
depname=colnames(abrLossDat)[3]
mat <- as.matrix(abrLossDat[,-3])
vnames <- colnames(mat)

for (i in 1:ncol(mat)){
windows()
   xres =lm(mat[,i]~mat[,-i])$res
   scatter.smooth(xres,abrFit$res,
      xlab=paste('Adjusted',vnames[i]),
      ylab='Abrasion Loss Residuals',
      main='Detrended Partial Residual Plot',
      col="red",pch=16,cex=1.2, las=1)
}

for(i in 1:ncol(mat)){
windows()
   plot(xres,abrFit$res+abrFit$coef[i+1]*xres,
      xlab=paste('Adjusted',vnames[i]),
      ylab=depname,
      main='Partial Residual (Added Variable) Plot',
      col="red",pch=16,cex=1.2,las=1)
   abline(0,abrFit$coef[i+1])
}

##End

There are four overplotted windows.  Move the top ones
to see the ones below

Both added variable plots show many points supporting
the regression lines.  This is good.  

The quadratic appearance in the partial residual variable
for tensile strength plots is suggestive but what action to
take is not clear.

7. A loess smooth model with contour and surface views 

We fit a locally quadratic polynomial a model f() a surface.
That is, z = f(x, y) + e.  Here z is abrasion loss, x is
hardness and y is tensile strength.

To produce conventional graphics for the surface we generate a
regular grid of (x, y) points where we use models to estimate
the corresponding z values.  Graphics provide contours and
surface views are designs to work using such regular grids. 


## Run
abrSmooth <- loess(abrLoss~hardness*tenStrength,
  degree=2, span=.5, data=abrLossDat)

# generate 2D grid for the explanatory variable domain 
hardness <- abrLossDat$hardness
tenStrength <- abrLossDat$tenStrength
gridMargins <- list(
  hardness=seq(min(hardness),max(hardness),length=20),
  tenStrength=seq(min(tenStrength),max(tenStrength),length=20))
gridFull <- expand.grid(gridMargins)

# predict (estimate) the surface avlues
abrGridPredict <- predict(abrSmooth,gridFull)

## End__________________

7.1 Contour Plot

##Run

contour(gridMargins$hardness,gridMargins$tenStrength,
  abrGridPredict, nlevels=15, 
  xlab='Hardness',ylab='Tensile Strength',
  main='Predicted Abrasion Loss')

##End__________________

You many need to close or move windows to bring
the active window into view.

7.2  Surface perspective view

##Run
persp(gridMargins$hardness,gridMargins$tenStrength,abrGridPredict,
      xlab='Hardness',ylab='Tensile Strength',zlab='Abrasion Loss')
title(main='Predicted Surface - Try 1')

##End________________________________

7.3 Lattice graphics for surfaces

##Run

library(lattice)

gridDat <- data.frame(Abr = as.vector(abrGridPredict),
  Hardness=gridFull$hardness,
  TenStr=gridFull$tenStrength)

# The following sequence of 5 windows rotates the 3D plot
# Move the windows to see the progression.
# 
# One student animated the 3D plots in R but perhaps use
# a different 3d plotting functions that wireframe.  
# I have not gotten wireframe to run in a for() loop.    


windows()
wireframe(Abr~Hardness*TenStr,data=gridDat,drape=T)

windows()
rotZX <- list(z=55,x=-60)
wireframe(Abr~Hardness*TenStr,screen=rotZX,data=gridDat,drape=T)

windows()
rotZX <- list(z=70,x=-60)
wireframe(Abr~Hardness*TenStr,screen=rotZX,data=gridDat,drape=T)

windows()
rotZX <- list(z=105,x=-60)
wireframe(Abr~Hardness*TenStr,screen=rotZX,data=gridDat,drape=T)

## End_________________________________

Now a level and a contour plot

windows()
levelplot(Abr~Hardness*TenStr,data=gridDat,main="Lattice Level Plot")

# Nice colors could be added as in previous a previous assignment
windows()
contourplot(Abr~Hardness*TenStr,data=gridDat,cuts=15,
  main="Lattice Contour Plot")
##End

The juxtaposition of a contour plot below or above a surface plot
can be helpful.

8. Conditioned plots for surface examination

Conditioning typically partitions data into disjoint subsets
based on the values of categorical variables or intervals
of continuous variables.  This enables three tasks

1) Focused assessment of the individual subsets.
   The assessment can include models or residuals
   as well as distributions.  

2) Lowering the plot dimensionality and providing a basis
   for organized comparison across subsets.

3) Restricts the variation due to the conditioning variables.

Cleveland used conditioning for models such as
z = f(x,y) + e to exploit the power and familiarity
of 2D graphics!  While common practice is to use
disjoint intervals the cover the range each variable,
he notes that the conditioning intervals can overlap
and calls such intervals shingles.  This is
especially advantageous when using loess smoothes
which are less reliable at the edges of the intervals. 
With shingles we can discount estimates at interval
edges and still have good coverage of the whole domain
except for the two edges near the domain endpoints. 

The co.intervals() function below define intervals
for continuous variables. The interval are allowed
to overlap.  Unless made to do so will not 
typically provide a strict partitioning.  

## Run
hardnessIntervals <- co.intervals(hardness,number=4,overlap=1/4)
tenStrIntervals <- co.intervals(tenStrength,number=4,overlap=1/4)

coplot(abrLoss~hardness | tenStrength, data=abrLossDat, 
	given.values=tenStrIntervals,
	panel=function(x,y,...) panel.smooth(x,y,span=.7))

## End____________________________________

The top panel shows the conditioning intervals for tensile strength.
The two lowest bars (position and value wise) give
    the conditioning intervals for the lowest panels
    in the matrix of panels below.  
The increasing intervals for bottom to top
    correspond to the matrix of scatterplot panels
    starting at the bottom, reading left to right
    and then looking a the next row of panels.

At a first glance the conditioned smooth looks linear 
and differs mainly in the intercept.  

To appreciate the changes in intercept
compare the vertical distances to the red lines 
from a common grid location in each panel such
as x=50, y=300.  In the bottom left panel the 
red line is a little above the point.  
In the top right panel it is far below the point.

The last three points in the lower left plot suggest a change
in slope.  

## Run

windows()
coplot(abrLoss~tenStrength | hardness, data=abrLossDat,
	given.values=hardnessIntervals,
	panel=function(x,y,...) panel.smooth(x,y,span=.8,col="black"))
## End________________


The smoothes against tensile strength conditioned on hardness
show more variety.  

Note that the conditioned plot shows data points and smooth
so suggest where residual are large, at least conditionally.

Look at a smoothed surface plot again. It does not show
the data.  A surface plots plus points with line from points
to the surface can show large residuals. A translucent surface
can help reveal points and lines above and below the surface.
The Stat 763 class uses rgl lighting model rendered 3-D graphic
to show the surfaces and residuals.  

9. Conditioning for more than three variables: discussion

While conditioning for 3 variable applications can be
helpful as Cleveland illustrates, the available 3-D graphs
can do pretty well.  In Chapter 5 Cleveland addresses hypervariate
data which means four or more  variables.  He has good graphics
to show the conditioning intervals along with a matrix of panels.
show 2D scatterplots with smoothes. For example see page 277.     

In my opinion conditioning is more important when there
are four or more variables.  The commenst below
address the situation of continuous variables but
note that approach is also applicable to the
levels of categorical variables.  

We can look at the relationship among two or three variables for
cases whose   
  4th variable values are in an interval, 
  5th variable values are in an interval
  and so on.
If there are 3 such intervals for the 4th variable
and          3 such intervals for the 5th variable
this yields 3 x 3 =  9 conditioned panels to view.    
The multiple panel views provided by lattice conditioning
and ggplot2 faceting are designed to address such situations.
I know the lattice can handle layouts for additional condition
variables that may span many printed pages.  

Incorporating more variables and our visual analysis broadens
context scope of our thinking.  In principle this is good
when the addition variables help the data tell a more
details and accurate story.     

There are two major issues.
1) the resolution lost by effectively treating the
   value of the conditioning variables in a 
   conditioning interval as being equivalent.
2) the difficulty we have in obtaining a gestalt
   view and understanding from the collection of panel
   views.

That is, we miss an important part of the story and
we may not be able to fully comprehend or easily
communicate a complex story. We have our limits, 
but restricting ourselves to two variables
impoverishes our thinking.   

Note the CCmaps design uses conditioning to address
five "variables": two geospatial coordinates and
three attributes.  The two threshold sliders for each
of the non-spatial variables which yield three
intervals for each variable.  These partitions the cases
such as states into cells of a 3 x 3 x 3 array.
CCmaps use color to indicate the levels (or classes) of
one variable so it suffices to use a 3 x 3 set of map
panels and show the states in their respective colors. 
  
 
