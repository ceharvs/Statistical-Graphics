          Crime Data Linear Regression
By:       Daniel Carr
File:     Crime data linear regression

Data
Source:   http://archive.ics.uci.edu/ml/datasets/Communities+and+Crime+Unnormalized
               
Read:     Crime data comments.pdf

Note:     Crime data clean.r has some
          discussions in sections 1-3
          and writes file crimeClean.csv

Uses:     Data:  crimeClean.csv
          Source functions: classDensity.r, classEda.r
          Packages: car, ellipse, scagnostics
            
Topics:   4.  Look at candidate dependent varibales
          4.1 Exploratory 1D graphics
          4.2 Studying the relationship between pairs
              of crime rates across selected communities 
          4.3 Correlation of crime rates
          4.4 Scatterplot matrices of crime rates:
              overplotted, hexagon binning
          4.5 Scatterplot matrices and smooths            

          5.  Selecting explanatory variables
          5.1 Scanning for variables to transform
              and looking at their transformed
              distributions
          5.2 Outlier discussion

          6.  Regression model fitting and dependent
              variable boxcox power transformation              
          6.1 Regression diagnostics before transfromation
          6.2 Regression diagnostics after transformation
          6.3 Stepwise variable remove

          7.  Looking at variables remaining in the model
          7.1 Looking at their univariate distributions
          7.2 Caution in interpreting regression
              coefficients  
          7.3 Comments on partial residual plot and scagnostics plots

          8.  Closing remarks
          
Due:      4.  The all violent crime plots before and after
              transformations
          4.3 Your choice of a correlation plot
          4.4 A hexagon binned scatterplot with the
             .5 power transformation of the count
          4.5 The splom with smooths.
               The EDA 1 variable plot
          6.2 A disagnostics plot of you choice
          6.3 The same diagnostics plot after transformation
          7.1 Choose and Eda plot of a variable with a thick tail
  
Uses:     Data:  crimeClean.csv
          Source functions: classDensity.r, classEda.r
          Packages: car

The goal here is look at the data in different ways.  We will
look at candidate dependent variables separately and then see
how that related to each other.  Then we will select explanatory
variables and some transform of them.       

We will also look at the data through the lense of a
simple linear regression model, picking a transformation
of the dependent variable based on models.  The models
will tell us a story about which variables most useful
as explanatory variables.  Here the choice is to model
the crime rates computed as counts per 100,000 peope using
simple multiple linear regression. Other models may be better
suited to the data and yield somewhat different stories.  
 
Generalized linear models based on Poisson or negative
binomial models for count data can model crime counts.
The mean and variance is the same for the Poisson distribution.
Fairly often the models diagnostics incidate that
the variance is larger than the mean. The negative binomial
with two parameters is better suited to handle such cases.

Sometimes there are too may zero counts to be well fitted by Poisson
and negative binomial models.  There are zero inflated extension of
the two kind of models to address this.     

The models will be limited by the data quality relative the task
and will be limited by  small amount of understanding we put into
model building. 

This class does not provide expertise in understanding
the factors related to violent crime.  The class does
not assume or provide deep expertise in linear regression.
The primary focus here is on learning about more ways to look
at data    

4. Look at candidate dependent variables 

4.1  Exploratory 1D graphics

Here we take a look at the distributions of different crime rates
that are candidate dependent variables for our crime rate modeling.

The modeling in this assignment focuses on the rate of all
violent crimes.  This includes the crimes classified as murder,
rape, robbery and assault.  In the modeling context will also
also look at the distributions of the explanatory variables.

The following graphs provide three views of a variable's distributions
that are useful for univariate data description/exploration.  
The 1D views are box plots, density plots and theoretical
Q-Q plots. 

Note:   Below we use a revised classEda function to produce
        the plots. The function could be further refined
        to match density and boxplot scales. It could highlight
        boxplot values on the density plot. We might also
        transpose the Q-Q plot so all the horizontal axes
        are data quantile axes.     

## Run: Extract crime rate variables and violence rates

crimeClean <- read.csv(file="crimeClean.csv",
  header=TRUE, row.names=1)
colnames(crimeClean)

subs <- c(106, 108, 110, 112, 121, 114, 116, 118, 120, 122)
crimeRates <- crimeClean[,subs]
colnames(crimeRates) <-  c('Murder','Rape', 'Robbery','Assault',
  'All Violent','Burglary','Larceny','AutoTheft','Arson','Non Violent') 
violenceRates <- crimeRates[,1:5]

## End_________________________________

## Run: Look at Violent Crime Variables

source('classDensity.r')
source('classEda.r')
library(car)

# Make better labels
lab1 <- colnames(violenceRates)
lab1[5] <- "All Violent Crimes"

# loop through the variables
for (i in 1:5){
  windows()
  classEda(violenceRates[,i],
  lab1=lab1[i],
  units = "Rate per 100,000")
}

## End__________________________________________________

We do we see?  

For murder the density plot is very high around 0 murders
per 100000.  There is long right tail that gets close to 
100 deaths per 100000.  

The Q-Q plot suggests that many communities have
rates at or close to 0.  A simple linear regression
model is not well suited to handling a mixture of
discrete 0s and continuous values.  Such distributions
call for more specialized modeling. 

The median of the murder distribution is where line for
the normal distribution median, x=0,crosses the string
of dots on the Q-Q plot. We might guess 3 deaths
per 100000 but can check.  

## Run: Basic murders rates statistics  
summary(violenceRates$Murder)

# End

(In the future we could simple statistics 
in the top plot left panel.)
we see a thin left tail.  Toward the left, the murders
quantiles are between the robust fit line that goes below
the plots and median y=2.46.

We see a thick right tail relative to a normal
distribution. As we look further to the right
the quantiles are far above the robust fit line
and NOT between the robust fit line and the median
y=2.46.    

The box plot suggests the median is around 3 and that
the upper adjacent value is a little over 20.  

For rape, the description is similar except that fewer 
communities have a rate of 0 and that the highest
rates extend to 400 per 100000.

For robbery there are even fewer zero values and the highest
rates begin to approach 2500 per 100000.

For assault there still appears to be some zero values but few enough
that simple linear regression seems reasonable.  The distribution
is still not even close to a normal distribution.  The highest
rates extend well above 3000 per 100000 or 3 per 100 people.  

The highest community rates for all violent crime extends
past 5000 per 100000 which about 1 per 20 people.  I suspect
we would not choose to live in such communities or at least
not in some parts of those communities.  

Since rates are non-negative we can consider power
transformation to bring in thick right tails. Because
we have zeros we can not use logarithms directly.  
Adding a small constant and the taking logs is an
option.  Below we we quickly  using a 0.2 power
transformation.

## Run

# loop through the variables
for (i in 1:5){
  windows()
  classEda(violenceRates[,i]**.2,
  lab1=lab1[i],
  lab2 = "Raised to the .2 Power",
  units = "Rate per 100,000 **.2")
}
  
## End__________________________________

Later will use the box-cox transformation
to pick the power in a regression setting.

4.2 Studying the relationships between
    pairs of crime rates across the select communities.

In studying this data we should keep in mind that
we are using available data. The communities selected
may provide a very poor representatives for the collection
of all US communities. Further our data cleaning
has removed communities that may make our results
even less representative. We are simply using
statistics to describe the relatively complete
available data.  

4.3 Correlation of crime rates

The functions cor() and var() will give use correlation and
covariance matrices for variables in a data set.  We look
at the violent crimes in a small table.   

## Run 
round( cor(violenceRates), 2)
##  

Looking at bottom row, "All Violent", we see that rape
has lowest correlation followed by murder.  While
these are pretty high correlations, the regression
models for rape and murder may include some different
explanatory variables than the model developed below
for the all violent crime rate.  

Also remember the presence of many zero rates
for rape and murder suggests using more elaborate
models.  

Here we chose graphics to show correlations for
all the kinds of crime rates.  

Deepayan Sarkar, 2008, in "Lattice" indicates that 
the "Corrgram (Friendly, 2002) are visual
representations of correlation matrices that usually 
encode correlations by more than just color or gray level
and reorder, the rows and columns by some measure
of similarity."

Below we use a dissimilarity matrix to reorder
rows and column.  We base many methods such as
clustering on dissimilarity or distance matrices.
Common practice converts a similarity matrix into
a dissimilarity matrix.

When two variables have high positive or negative
correlation they can be viewed as being similar up
to a change of sign. The choice here is to use
1-abs(correlation) as the dissimilarity.
Highly correlated values irrespective of the sign
will then have a dissimilarity value close to 0.
A 0 correlation leads to a dissimilarity of 1.
     
Below I show two versions of corrgrams from the
lattice book. The first uses the ellipse
package of Murdoch et al., 2007.    Later in the
course I may show my version that uses position
along a scale encoding. 

## Run

corMat <- cor(crimeRates)
distMat <- 1-abs(corMat)
xloc <- cmdscale(distMat,k=1)
ord <- order(xloc)
xloc[ord,1]
corMatOrd <- corMat[ord,ord]

source("panelCorrgram.r")
library(lattice)
library(grid)
library(ellipse)

windows()
levelplot(corMatOrd, at = do.breaks(c(-1.01,1.01),20), 
 xlab=NULL, ylab=NULL, colorkey = list(space="top"),
 scales = list(x=list(rot=90)),
 panel = panel.corrgram, label=TRUE)

windows()
levelplot(corMatOrd, xlab=NULL, ylab=NULL,
  at = do.breaks(c(-1.01,1.01),100),
  panel = panel.corrgram.2,
  scales = list(x=list(rot=90)),
  colorkey =  list(space = "top"),
  col.regions = colorRampPalette(c("red","white","blue")))
## End

The plots tells there are high correlation among the
variables. We suspect some of this is due to some 
communities have some zero rates for the same kinds of
crimate.  The correlations may tell us very little about the
bivariate patterns that we can see bivariate density
plots and plots of smooth functional relationships. 
Note that bivariate points on quadratic curves can have
a correlation of zero.  It is true that correlation of zero
suggests two variables are independent when they have a
normal distribution. However points on a curve
don't have a normal distribution. They can be
total dependent and have a correlation of zero.     

4.4  Scatterplot matrices and hexagon binning

We return our attention to the violent crimes
because there seems to be text scaling issue
with the lattice splom graphics that makes
the labels hard to read.

The scatterplot matrix provide way of looking at
several variables in the context of each other.
Point plots can be quite informative for small data sets
but over plotting can hide patterns in large data sets.

Hexagon binning can help in visualizing data
density while still suggesting the presence
of the underlying point structure. Contours,
level plots, and perspective surface views
are additional ways to encode data density so
we can see patterns.    


## Run
windows(width=8,height=8)
pairs(violenceRates, main="Scatterplot Matrix", 
  gap=0, las=1, row1attop=FALSE)
 
library(lattice)
library(hexbin)

windows(width=8,height=8)
splom(violenceRates,
 cex.labels =.2,
 pscale=0,
 panel = panel.hexbinplot
)

windows(width=8, height=8)
splom(violenceRates,
 cex.labels =.2,
 pscale=0, trans=function(x)x^.5,
 panel = panel.hexbinplot
)

windows(width=8,height=8)
splom(violenceRates,
  pscale=0, type=c('g','smooth'))

## End____________________________

Move the windows to see the plots.  

The first example shows heavy overplotting
of points.

The next two examples below calls attention to a few high
counts cells and their locations. These provides important
context for assessing how well the smooth curve below
represents the relations of the variable pairs. For some
domain of one variable domain the variation in the other
variable may be small.  For part the variation may 
be very large variation. 

To provide better gray level resolution 
for lower count cells we can take a fractional
power of the bin counts before the conversion
to gray level. The choice in the second
hexbin example above was  .5. 
We could use the argument
   trans=sqrt
however 
   trans = function(x) x^.5
shows it is easy to use a power such as .2
or smaller. 

Side note on hexagon binning_____________________  

Examples with hexagon binning appear in both
lattice and ggplot two packages. Often these
are in the context of category-index panel layouts.
showing pair of continuous variables. 

The binning within the context of scatterplot matrix
poses an extra challenges to higher order
graphics functions. For example
a legend for the bin counts requires
gathering results from all the panels.  

I don't see any scatterplot matrix examples
in ggplot2 book by Hadly Wickham.   

I have not yet found a way to make
the hexagon binning work as panel
function to pairs() function. 

The lattice splom() plot used the graphics
bottom up convention. The diagonal
goes from lower left to upper right.  
 
The default for pairs() is the top down
reading convention, but I used its option
for the bottom up design so it is
compatible with the splom() plots.

End Note____________________________________

The smoothes can be very deceptive
if they are not viewed in the context
of the data density. I have not
yet been able to overplot the curves
on splom hexbin plots.  At least 
showing the points helps.    

4.5 Scatterplot matrix with smoothes

## Run
windows(width=8,height=8)
splom(violenceRates,
  main="Rates per 100000",lwd =2 ,col.line="red",
  pscale=0, type=c('p','g','smooth'))

## End___________________________


5. Selecting explanatory variables 
  
Some variables may seem or be undesirable as explanatory variables.  
For our cursory inspection we will look at their names.    

## Run

nam <- colnames(crimeClean)[1:104]
nam

numSubs <- which("Num"==substring(nam,1,3))
numSubs
nam[numSubs]
nam[numSubs+1]

medSubs <- which("Med"==substring(nam,1,3))
nam[medSubs]

medSubs 

## End______________________________ 

I don't want to use place and state in the model. 

I have concerns about using population
size related variables since the violent crime
rate per capita already incorporates population
size. Of course the impact of population size and
related variables may not be linear. I will leave
population in the model but remove the count
variables that start with "Num" above.  
The data set still contains three of them
that were converted to percents.  

There were several police variables such as
counts and percents in different race and
ethnic categories. There are also
counts and percents of police serving
in different roles such as being a member
of the drug unit. 

These variables may be helpful in fitting the
data but they may not necessarily lead to much
insight about other factors related to crime.
Some police variables may correlate well with some
some crime variables because the crime rates motive
the hiring of police.  

The regression model below omits the police
variables. Part of the motivation is that
most variables did not pass the missing data
filter and I did not want to address missing
data imputation in this class.

Many variables can be sufficiently correlated that
removing one or more can lead to an almost equally
good fit.  We will pick representative variables
from clusters of variables that have absolute
correlations higher than 0.95. 
This may loose a little information, but will
simplify looking at the variables retained.     

The script below converts the correlation
matrix into a distance matrix. Then
it uses multidimensional scaling to make
a scatterplot of variables names. In many
cases the highly correlated variable names
so they are hard to read. 

The script continues by making a
list with vectors containing
variable name. The plan is to 
select on variable to used from
each vector of highly correlated
variables.

## Run

subs = c(3:29,31:51,53,55:93,96:103)
crimeExplain <- crimeClean[,subs]
crimeCor <- cor(crimeExplain)
diag(crimeCor)<- 0  # make the diagonal 0
range(crimeCor)

maxAbs <- function(x) max(abs(x)) > .95
highCorVar <- apply(crimeCor,1,maxAbs)

smallCor <- crimeCor[highCorVar,highCorVar]

xy <- cmdscale(1-abs(smallCor),k=2)

windows()
plot(xy,type='n',pty='sq',las = 1)
text(xy[,1],xy[,2],rownames(xy))

smallNam <- rownames(smallCor)
namList <- vector("list",length=length(smallNam))
names(namList) <- smallNam

for(i in 1:length(smallNam)){
  namList[[i]]<-smallNam[abs(smallCor[i,])>.95 ]
}

namList

## End

Below I copied the vectors and indicate
the subscript and my choices

1Yes $population
     [1] "numbUrban"

2No $numbUrban
    [1] "population"

3Yes $medIncome
     [1] "medFamInc"

4No   $medFamInc
     [1] "medIncome"

5Yes  $perCapInc
      [1] "whitePerCap"

6No   $whitePerCap
      [1] "perCapInc"

7No    $PctBSorMore
       [1] "PctOccupMgmtProf"

8Yes   $PctOccupMgmtProf
       [1] "PctBSorMore"

9Yes   $MalePctDivorce
       [1] "TotalPctDiv"

10Yes  $FemalePctDiv
       [1] "TotalPctDiv"

11No   $TotalPctDiv
       [1] "MalePctDivorce" "FemalePctDiv"  

12Yes  $PctFam2Par
       [1] "PctKids2Par"

13NO   $PctKids2Par
       [1] "PctFam2Par"

14Yes  $PctRecentImmig
       [1] "PctRecImmig5"  "PctRecImmig8"  "PctRecImmig10"

15No   $PctRecImmig5
       [1] "PctRecentImmig" "PctRecImmig8"   "PctRecImmig10" 

16No   $PctRecImmig8
       [1] "PctRecentImmig" "PctRecImmig5"   "PctRecImmig10" 

17No   $PctRecImmig10
        [1]    "PctRecentImmig" "PctRecImmig5"
               "PctRecImmig8"   "PctForeignBorn"

18Yes  $PctLargHouseFam
       [1] "PctLargHouseOccup"

19No   $PctLargHouseOccup
       [1] "PctLargHouseFam"

20Yes  $PctPersOwnOccup
       [1] "PctHousOwnOcc"

21No   $PctHousOwnOcc
       [1] "PctPersOwnOccup"

22No   $OwnOccLowQuart
       [1] "OwnOccMedVal"  "OwnOccHiQuart"

23Yes  $OwnOccMedVal
       [1] "OwnOccLowQuart" "OwnOccHiQuart" 

24No   $OwnOccHiQuart[1]
       "OwnOccLowQuart" "OwnOccMedVal"  

25No   $RentLowQ
       [1] "RentMedian"

26Yes $RentMedian
      [1] "RentLowQ"  "RentHighQ" "MedRent"  

27No  $RentHighQ
      [1] "RentMedian" "MedRent"   

28No  $MedRent
      [1] "RentMedian" "RentHighQ" 

29Yes $PctForeignBorn
      [1] "PctRecImmig10"

## Run

subs <- c(1,3,5,8,9, 10, 12, 14, 18, 20, 23, 26, 29)
dropNam <- names(namList)[-subs]
dropNam

good <- is.na(match(
  colnames(crimeExplain),dropNam))

crimeExplain<- crimeExplain[,good]
head(crimeExplain)


## Run
    
crimeReg <- cbind(crimeExplain,
            ViolentCrimesPerPop = crimeClean[,121])
dim(crimeReg)
write.csv(crimeReg,file="crimeReg.csv",quote=FALSE)
head(crimeReg)

## End__________________________________________


5.1  Scanning for predictor variables to transform
     and looking at their transformed distributions

We will scan for positive explanatory variables that
are suitable for a log transformation and look at the
log transformed distributions. We will pick variables
with a minimum greater than .001 to avoid taking
log of 0 and maximum to minimum ratio greater than
50. As I remember Dennis Cook had recommended considering
making a log transformation when the ratio 10 or
larger and definitely trying the log when the
ratio 100 or larger.         
 
## Run

nam <- names(crimeReg)
n <- ncol(crimeReg)
logTrans <- rep(FALSE,n)

# The last column is the dependent variable

for(i in 1:(n-1)){
  rx <- range(crimeReg[,i])
  if( rx[1] > .001) {
    if(rx[2]/rx[1] > 50 ){
      logTrans[i] <- TRUE
     }
  }
}

devAskNewPage(ask=TRUE)
for(i in 1:(n-1)){
  if(logTrans[i]){
    units= paste("Log",nam[i])
    classEda(log(crimeReg[,i]),
       lab1=nam[i],units=units)
  }
}

nam[logTrans]

classEda(crimeReg$PopDens)
## End


## Run

crimeRegLog <- crimeReg
for(i in 1:(n-1))
 if(logTrans[i])crimeRegLog[i] <- log(crimeReg[i])
nam <- colnames(crimeReg)
namLog <- paste("Log",nam,sep="_")

namLog <- ifelse(logTrans,namLog,nam)
colnames(crimeRegLog) <- namLog
head(crimeRegLog)

## End___________________________

5.2. Outlier discussion
    
In a regression context the impact of outliers depends
on the type of model used and role variables play
in the model. 

Explanatory variable outliers can influence performance
of modeling tools that we use to help bring out patterns.
The distribution of spacings (gaps) between points can
be important for both smoothing and density estimation.
Points borrow strength from their neighbors. Transformations
change the spacings between points so determine the
neighborliness of the nearest neighbors.  

In linear regression outliers in explanatory variables
may have a large influence on the model coefficients,
predicted values, residuals and residual variance.  

Advanced classes provide more background and build
on knowledge of matrix algebra.  In class I briefly show
the construction of a projection matrix using matrix
algebra. This may not be clear. This basic message to
get is that we can assess the leverage of case and
how much influence it has on the predicted values and
the coefficients in a linear regression model. Such
cases warrant special attention.  
  
A case's leverage is given by case's diagonal element
in the projection matrix. If the diagonal element
is 1 the case rules.  The predicted value is the same
as observed value. Cases with diagonal values greater
than 2p/n, where p is the number of explanatory variables
and n is the sample size, are considered to have high
leverage.  

Cook's distance assesses how much a case influences
the regression coefficients.  A high leverage case
may be consistent with the body of the data. That is
its removal does not change the coefficient very much.
However if a high leverage case's dependent variable is telling
a very different story the coefficients change. That is,
case influence on the model depends on both the dependent
variable value and leverage from explanatory variables.

6. Power transformation for a regression and
   dependent variable boxcox transformation

The violent crime rate distribution is really skewed.  
The boxcox() function can use results from
a linear model to suggest a power transformation.

## Run_________________________________

lm1 <- lm(ViolentCrimesPerPop ~ ., data=crimeRegLog)

library(MASS)
windows(w=6, h=6)
boxcox(lm1)
power <- boxcox(lm1, lambda=seq(-1, 1, .05), plot=FALSE)
power

# There is power around .2 that proved a maximum
# Which one is it

sub <- which(power$y == max(power$y))
power$x[sub]

## End________________________________________

Okay it is .2.  

## Run

dep <- crimeReg$ViolentCrimesPerPop
var <- dep**.2
classEda(var,
  lab1 = "Violent Crimes",
  lab2 = "In Communities",
  units = "(Rate per 100,000 People)**.2")

## End

6.1 Regression diagnostics before transformation___________ 

Does the transformation make a difference in 
terms of the regression diagnostics?  

The plots below are interactive.  Click in the window
to advance to the next plot.     

## Run

windows()
plot(lm1)

## End_______________________________

The diagnostics plots indicated problems. 

1) y = Residuals versus x=fitted values. 
   The wedge shape shows variance increasing dramatically 
   as function of the fitted values.  This violates
   the constant variance assumption behind statistical
   inference parameter estimates, etc.

2) Q-Q normal plot.  There are thick tails at both ends of the
   Q-Q plot and three cases as flagged as outliers

3) Spread location plot.  Y = square root absolute standardized residual
   plot versus X, the fitted values is badly increasing
  
4) The influence plot: y = standardized residuals versus
   x = Leverage with Cook's Distance boundary lines.  
    
6.2 Regression diagnostics after transformation

## Run

lm2 <- lm(ViolentCrimesPerPop**.2 ~ ., data=crimeRegLog)
windows()

plot(lm2)

# 

6.3 Using forward subset selection to obtain
    a smaller number of variables.

## Run

library(leaps)
regfit.fwd=regsubsets(ViolentCrimesPerPop**.2 ~ .,
  data=crimeRegLog,nvmax=16,method="forward")

reg.summary=summary(regfit.fwd)
names(reg.summary)
reg.summary$rsq
 
windows(width=6, height=6)
par(mfrow=c(2,2))
# 1st row 1st  column
plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")

# 1st row 2nd  column
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
loc <- which.max(reg.summary$adjr2)
loc
points(loc,reg.summary$adjr2[loc], col="red",cex=2,pch=20)

# 2nd row 1st column
plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp",type='l')
loc <- which.min(reg.summary$cp)
loc
points(loc,reg.summary$cp[loc],col="red",cex=2,pch=20)

# 2nd row 2nd column
plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC",type='l')
loc <-  which.min(reg.summary$bic)
loc
points(loc,reg.summary$bic[loc],col="red",cex=2,pch=20)

## End___________________

# BIC stand for Bayesian information criterion
# see page 255 in the RFE text
#
# While rhw adjusted r-squared is still increasing toward
# .7 and the the Cp still decreasing, the BIC curve suggest
# the models are beginning to get a little worse variables
# as the number of varaiables in the model increases.  
# Maybe the best variables are among the first 16 variables
# found via forward stepwise regression.    
  
# Obtain the 16 variables picked
#   The -1 below excludes the intercept
#   We don't need it name as it
#   will be included by default

## Run: Make smaller data frame
#  include the power transformed
#  dependent variable.  

varNamesLogical <-reg.summary$which[16,-1]
varNamesLogical[varNamesLogical==TRUE]
crimeRegUse <- crimeRegLog[,c(varNamesLogical,TRUE)]
names(crimeRegUse)
crimeRegUse$crimeRateTrans<- crimeRegUse[,17]**.2
crimeRegUse = crimeRegUse[,-17]
names(crimeRegUse)

## End

7. Writing a prediction function

## Run
This is from the ISLR recommended text

predict.regsubsets=function(object,newdata,id,...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}

## End____________

# 8.  10 fold cross validation for best subset selection
#     for each number of variables in the model  
#     The best subset of variables for a given
#     number variables does not always match
#     those found via forward and backward
#     subset stepwise selection.
#
# Fit again the reduced data frame.  
# Get the coefficients of the
# the best 16 variable model
#
# Remember this was best for the
# Cp criterion

## Run

# Define subsets of cases for 10 fold
# crossvalidation
k=10

nvMax=16
set.seed(1)
folds=sample(1:k,nrow(crimeRegUse),replace=TRUE)

# Create matrix to MSE for each fold test set
# and using a model with from 1 to nvMax variables 
cv.errors=matrix(NA,k,nvMax, dimnames=list(NULL, paste(1:nvMax)))

# The fold and number of variables loops 
for(j in 1:k){ # fold loop
  best.fit=regsubsets(crimeRateTrans ~ .,
      data=crimeRegUse[folds!=j,],nvmax=nvMax)
  for(i in 1:nvMax){ # number of variable loop
    pred=predict.regsubsets(best.fit,crimeRegUse[folds==j,],id=i)
    cv.errors[j,i]=mean(
      (crimeRegUse$crimeRateTrans[folds==j]-pred)^2)
  }
}
mean.cv.errors=apply(cv.errors,2,mean)
round(mean.cv.errors,2)
par(mfrow=c(1,1))
plot(mean.cv.errors,type='b')
loc=which.min(mean.cv.errors)
loc

# The plot shows that Model 11 is barely
# better than Model 10. The difference
# in the forth digit. I would be inclined
# to go with the simpler model.  

mean.cv.errors[10]
mean.cv.errors[11]

# As a side note
# The average 10 fold MSE for model 10
# is much larger than the non cross-validated MSE
# from much earlier. 

model10.MSE

## End

# Cross validation helps to tell an overfitting story.  

# The authors go with model 11 as best 

## Run

reg.best=regsubsets(Salary~.,data=Hitters, nvmax=19)
round(coef(reg.best,loc), 3)

## End


## End

The residuals versus fitted plot looks much better. 
Flagged cases are 444,941 2177

The Q-Q plot upper tail looks better but the lower tails is 
still very thick. Flagged cases are 386, 444, 941

The spread location plot show a reduction from left to right but
not too bad.  Flagged cases are 386, 444, 941 as in the Q-Q plot. 

The influence plot flags case 386, 941 and 1639

The case numbers are from the original data sets 

## Run
crimeRegLog["386",]
crimeReg["386",]
## End

This small population community could be quite different.

## Run

summary(lm2)

## End

The multiple R-Square value is .717. There is a
substantial amount, 28.2 percent, of variability
that is not explained by the explanatory variables.  

Note the variables list on the console summary
with at least asterisk on the right are statistically
significant with p-values <.05. Some have a much
smaller p-values. Still, many are not statistically
significant. To have fewer variables to consider
could can use forward, backward and both to 
pick out variables.  Here we use the default
which is backward stepwise regression and then
focus the variables that survive.


6.3 Stepwise regression variable removal________________

With the default trace = 1 in the step() function below,
the list of variables in each model appears in the 
R console and gets shorter one variable at a time.  
The steps get faster as more variables are removed.
In many not seem like R is doing anything for a while.  

I liked seeing the steps so I know progress was being made.
In this case I don't think showing the output takes too
much additional time. 

The criterion used to remove variables is the Akaike
information criterion, AIC.  This is 2p - 2ln(L) where
p is the number of variables in the model and L is the
likelihood.  The goal is to minimize this value.  

Having fewer parameters helps to reduce the AIC. However
this reduces the likelihood L. The likelihood, L is a
positive value bounded by 0 and 1 so ln(L)is negative. 
Reducing L make ln(L) a more extreme negative number. 
Then the -2ln(L) term is a larger positive number. 
Hence the minimization seeking a balance between
the two AIC terms.    
 
## Run

n <- nrow(crimeRegLog)
lm3 <- step(lm2, direction="forward",
       k=2,steps=12)
plot(lm3)

summary(lm3)b

## End

This is progress of sorts.  The R-squared barely dropped
(From .7168 to .7135).  There are many fewer variables
left in the model.  

Call:
lm(formula = ViolentCrimesPerPop^0.2 ~ Log_population + racePctWhite + 
    Log_racePctAsian + Log_racePctHisp + agePct12t29 + agePct65up + 
    pctUrban + pctWWage + pctWInvInc + pctWRetire + blackPerCap + 
    AsianPerCap + OtherPerCap + Log_PctPopUnderPov + PctNotHSGrad + 
    PctEmploy + PctEmplManu + PctEmplProfServ + PctOccupMgmtProf + 
    MalePctDivorce + MalePctNevMarr + PersPerFam + PctFam2Par + 
    PctTeen2Par + PctWorkMomYoungKids + PctWorkMom + PctKidsBornNeverMar + 
    PctImmigRec10 + PctSpeakEnglOnly + PersPerOccupHous + Log_PctPersDenseHous + 
    PctHousOccup + PctVacMore6Mos + MedYrHousBuilt + OwnOccMedVal + 
    RentQrange + MedOwnCostPctIncNoMtg + PctSameState85 + PctUsePubTrans, 
    data = crimeRegLog)

Residuals:
     Min       1Q   Median       3Q      Max 
-1.39731 -0.24787 -0.01031  0.24409  1.71231 

Coefficients:
                        Estimate Std. Error t value Pr(>|t|)    
(Intercept)            1.066e+01  2.983e+00   3.572 0.000364 ***
Log_population         8.530e-02  1.342e-02   6.356 2.60e-10 ***
racePctWhite          -8.051e-03  1.300e-03  -6.193 7.25e-10 ***
Log_racePctAsian       3.162e-02  1.569e-02   2.015 0.044053 *  
Log_racePctHisp        5.141e-02  1.434e-02   3.586 0.000345 ***
agePct12t29           -8.372e-03  5.436e-03  -1.540 0.123714    
agePct65up             2.338e-02  6.754e-03   3.462 0.000548 ***
pctUrban               1.082e-03  2.665e-04   4.058 5.15e-05 ***
pctWWage              -7.057e-03  4.846e-03  -1.456 0.145466    
pctWInvInc            -1.541e-02  2.247e-03  -6.855 9.64e-12 ***
pctWRetire            -5.768e-03  3.381e-03  -1.706 0.088154 .  
blackPerCap           -1.649e-06  1.113e-06  -1.482 0.138613    
AsianPerCap            1.710e-06  1.053e-06   1.624 0.104599    
OtherPerCap            3.089e-06  1.166e-06   2.648 0.008167 ** 
Log_PctPopUnderPov     9.179e-02  4.031e-02   2.277 0.022905 *  
PctNotHSGrad           6.785e-03  2.575e-03   2.635 0.008486 ** 
PctEmploy              1.561e-02  3.760e-03   4.153 3.43e-05 ***
PctEmplManu           -4.685e-03  1.571e-03  -2.983 0.002895 ** 
PctEmplProfServ       -4.202e-03  2.875e-03  -1.462 0.143931    
PctOccupMgmtProf       1.247e-02  2.983e-03   4.179 3.06e-05 ***
MalePctDivorce         3.070e-02  8.425e-03   3.644 0.000276 ***
MalePctNevMarr         1.178e-02  3.828e-03   3.078 0.002114 ** 
PersPerFam            -7.377e-01  1.827e-01  -4.039 5.59e-05 ***
PctFam2Par            -8.035e-03  4.332e-03  -1.855 0.063764 .  
PctTeen2Par            3.254e-03  2.256e-03   1.442 0.149479    
PctWorkMomYoungKids    4.836e-03  2.839e-03   1.704 0.088628 .  
PctWorkMom            -9.716e-03  3.923e-03  -2.476 0.013358 *  
PctKidsBornNeverMar    3.059e-02  8.771e-03   3.487 0.000499 ***
PctImmigRec10         -2.103e-03  8.211e-04  -2.562 0.010490 *  
PctSpeakEnglOnly       6.626e-03  1.583e-03   4.185 2.98e-05 ***
PersPerOccupHous       5.327e-01  1.515e-01   3.516 0.000449 ***
Log_PctPersDenseHous   1.120e-01  3.046e-02   3.676 0.000244 ***
PctHousOccup          -8.819e-03  2.375e-03  -3.713 0.000211 ***
PctVacMore6Mos        -2.052e-03  8.833e-04  -2.323 0.020269 *  
MedYrHousBuilt        -3.465e-03  1.403e-03  -2.469 0.013636 *  
OwnOccMedVal          -7.539e-07  2.389e-07  -3.156 0.001627 ** 
RentQrange             6.757e-04  1.560e-04   4.332 1.56e-05 ***
MedOwnCostPctIncNoMtg -2.483e-02  7.846e-03  -3.164 0.001579 ** 
PctSameState85         3.758e-03  1.695e-03   2.217 0.026718 *  
PctUsePubTrans        -6.757e-03  2.592e-03  -2.607 0.009209 ** 
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.3785 on 1861 degrees of freedom
Multiple R-squared:  0.7135,    Adjusted R-squared:  0.7075 
F-statistic: 118.9 on 39 and 1861 DF,  p-value: < 2.2e-16


7. Looking at variables in the model ______________

Below we restrict attention of variables still in the 
lm3 model to illustrate looking at variables.
There can be advantages to transforming skewed or heavy
tailed variables.  This can really change how the 
variables will correlated with each other and with 
the dependent variable. Some variables still have extreme
values that we may want to address.   

7.1  Looking at their univariate distributions

## Run

# obtain term labels
tmp <- lm3$terms
termLab <- attr(tmp, which="term.labels")

windows()
for(i in 1:length(termLab)){
par(ask=TRUE)
  classEda(crimeRegLog[,termLab[i]],
  lab1=termLab[i])
}


## End___________________________________________


7.2  Caution in interpreting regression coeffients  

The variables are non-negative

A first thought might be that signs of the coefficients
in the regression model directly tell a story.

A positive signs means higher variable values lead to 
higher crime rates.  Similarly a negative sign means
high variable values lead to lower crime rates.  

HOWEVER, many variables are correlated and the signs
be misleading.  A variable may be countering balancing
over or under estimates due to other variables.

Below is a one form of a color matrix.  
You may prefer heat maps or other variations.  
There are many variations.  

An assignment I may give this class 
shows representing a correlation matrix
use position along a scale to show
magnitudes and color to show the sign
of the correlation.   

Again we use  seriation method to order
the rows and columns.  As earlier in the
assignment the approach here
uses 1-abs(corMat) as a distance matrix
which is sometimes called the Pearson
absolute distance, and multidimensional
scaling into the 1D to obtain coordinates
to order the variables. 

To separate the strong positive
and negative correlations we could
use 1-corMat as the distance matrix.
This maps a correlation of -1 into a distance
of 2.  I maps a correlation of 1 to 0.

## Run

# compute and order correlation matrix

corMat <- cor(crimeRegLog[, termLab])   # get correlations
x <- cmdscale(1-abs(corMat), k=1)     # seriation 
ord <- rev(order(x))
corMatOrd <- corMat[ord, ord]
round(corMatOrd, 2)

# define colors
mat <- matrix(c(
120,  60, 180, 
175, 141, 195, 
231, 212, 232, 
255, 255, 255,
217, 240, 211,
127, 191, 123, 
40,  140, 100), ncol=3, byrow=TRUE)
colors=rgb(mat[, 1], mat[, 2], mat[, 3], max=255)

# assign color classes

tmp <- as.vector(corMatOrd)
brk <- c(-1.01, -.70 , -.40, -.10, .10, .40, .70,  1.01)
colorSub <- cut(tmp, brk, label=FALSE)

# plot

windows(w=9, h=7)
par(mai=c(.4, .5, .3, .2))
nr <- nrow(corMatOrd)
x <- 1:nr-.5
cen <- expand.grid(list(y=rev(x), x=x))
plot(c(-10, nr+2), c(0, nr+.3), type='n', axes="FALSE", ylab='')
mtext(side=3,line=0,
   'Diverging Scale:  Shades of purple negative, Shades of green positive')
mtext(side=1, line=0, 
  'Seriation by 1D MDS, Pearson Distance: 1-abs(cor)')
# could use image()
rect(cen$x-.5, cen$y-.5, cen$x+.5, cen$y+.5, col=colors[colorSub],
  border=rgb(.2, .2, .2))
text(rep(-10.5, 35), y=rev(x), rownames(corMatOrd), adj=0, cex=.75)

## End_________________________________

The plot could use some more work.  For example it should
show correlation class scale to explain the colors.

As is it shows some patterns such as blocks of squares on the diagonal
with strongly correlated variables, positive or negative. 
There is set of nine variable at the top left where most are
are highly correlated with each other.  

A 2 x 2 block near the middle shows male percent never married
and age percent 16 to 24.  This is consistent with what
most of us would assume. 

 
7.3 Comments: Partial residual plots and scagnotics plots

We could learn more about the unique contribution
of variables using partial residual plots introduced
earlier in some of my classes
 
The scagnostic package provide a search for 
several patterns of potential interest in the 
scatterplot panels of a scatterplot matrix. 

Consider a 30 continuous variable data set.  
The lower triangle of the scatterplot matrix has
435 scatterplots. I am not aware of people that
look actually all these plots.  

The scagnostics function will evaluate the
plots in terms of nine patterns are called
outlying, skewed, clumpy,  sparse,
striated, convex, skinny, stringy, and monotonic. 
The diagnostics are all measures on a [0  1] scale
with large values being of interest.

It is good idea to get calibrated by looking
scatterplots with small values.  

I tend to be most interested in the outlying and 
monotonic patterns. The outlying pattern identifies
plots with outliers. This includes bivariate outliers
that are not evident in the x and y margin 
univariate EDA plots. 

The output of the scagnostics function can be treated
as a diagnostics data matrix with an index to a 
scatterplot as rows and the 9 diagnostics as columns.
Each point in a scatterplot matrix of this
diagnostic matrix represents a scatterplot in
the original scatterplot matrix. 

We can apply the scagnotics function to a diagnostic
matrix and use the outlying measure to pick 
diagnostic panel plot to examine. A bivariate
outlier in this panel would lead us to the  
scatterplot in the original scatterplot matrix
that was usual in terms of at least two patterns.      

Because this assignment is already long and as far as I
know the use of scagnostics has gained little
traction in terms of common use I am putting
an example in a separate folder for those that
are interested.    


8. Closing remarks
 
The regression assignment stops here.  It could go on.
Linear regression methodology has developed over the years
and there is a lot to learn. 
  
We could have transformed more explanatory variables or
considered powers and products of the explanatory
variables.  There are modern more automated modeling methods
that address transformation.   

We can only go so far with the data at hand.  The
web site indicates there are other important factors
to consider that are not a part of the data set.  For
example the web site example cites the connection between
tourists and crime. There is no tourist data.  

We will turn attention to random forests where the modeling
approach is a little more protective of new analysts.














