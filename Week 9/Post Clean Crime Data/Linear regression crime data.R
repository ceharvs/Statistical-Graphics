          Linear Regression Crime Data
By:       Daniel Carr
File:     Regression.r 

Topics:   1.  String processing for labels
          2.  Fixing and reading the data 
              Change the missing data codes to NA
          3.  Dealing with missing data
          3.1 Removing variables
          3.2 Removing cases
          4.  Using scatterplots to look a
                candidate dependent variables
          5.  Outlier and case influence discussion
          6.  Regression
          6.1 Variable Selection
          6.2 Exploratory 1D graphics
          6.3 Power transformations
          6.4 Regression diagnostics
          6.5 Stepwise variable removal
          7.  Looking at the Explanatory Variables
          7.1 Looking at the univariate distributions 
          7.2 Looking variables pairs and correlations
          7.3 Bivariate outliers and scagnostics 
          8.  Close remarks
          
Due:      4.  The last two pairs() plots
          6.2 The EDA 1 variable plot
          6.3 The boxcox plot and the EDA 1 variable plot
          6.4 The last 3 lm1 model diagnostic plots
          6.5 The last 3 diagnostic plots for the lmPower2 model
          7.1 A skewed density plot, your choice
          7.2 The colored correlation matrix plot
          7.3 The single scagnostics plots    

Read:     Crime data comments.pdf

Data
Source:   http://archive.ics.uci.edu/ml/datasets/Communities+and+Crime+Unnormalized
               
           
1. String processing to extract variable labels______________

The download of the description file did not work.
I copied text from the web site html file for variable
descriptions and pasted this into Wordpad.  

Next steps included
1) Hand removal of county Code, community Code and fold number variable
   label lines and blank lines.
2) Hand removing "--" and at the beginning of the lines.  
   In hind sight I could have removed "-- ", but then chose
   to illustrate removing blank in R.    
3) Observations 
   The variable description lines are one and not convenient
       as variable labels.  
   The text left ":" on each line can be variable label
   strsplit() can split such strings
   scan() can read a line at time with "\n" as the delimiter  

## Run

varNam <- scan(file="CrimeVarNames.txt", what="a", sep="\n")
tmp <- strsplit(varNam, ":") 
tmp

## End

The result is a list structure with 144 length 2 vectors
The first of each pair is the string we want.
unlist() will make a vector from the list
We will extract the strings with odd numbers subscripts     

## Run

varLab <- unlist(tmp)[seq(1, 288, by=2)]
varLab

## End

The first character is blank
nchar() produces the lengths of strings in the vector.
substring () will let us pick the beginning and 
ending of the substring we want to keep.
Below we skip over the 1st character.  

## Run

n <- nchar(varLab)
varLab <- substring(varLab, 2, n)

# some labels are too long

varLab[1] <- "place"
varLab[2] <- "state"
varLab[3] <- "population"

varLab

## End

2. Reading and fixing the data________________________________ 

When saving the downloaded comma delimited data file, I chose 
the .csv extension so I could read it in Excel.       

I hand deleted the columns and labels corresponding to the
variable descriptions I had removed above.  

It is wise to document hand editing and save a copy of
annotations removed from .csv files for latter reference. 

I observed that the missing data code was "?".  This is easy
to fix in R, so I chose not to use hand search and replace methods. 
I prefer using R scripts to limit the amount hand editing
because itdocuments the work and enable easy replication.
   
## Run:  Read data, address missing data codes and variables labels

crimeDat <- read.csv(file="crimeDat.csv", header=FALSE, na.string="?")
colnames(crimeDat) <- varLab
head(crimeDat)

## End

3. Addressing missing data______________________________

The treatment of missing data is an important and
substantially studied topic but little addressed here.  

Some outliers have a story to tell.  Carr and Nicholson [1983]
provided a scatterplot matrix approach to looking at outliers
by using the plots scale frame coordinates as missing data
coordinates. Their plots shows that missing chemical 
concentration values were related to small volume rain fall
samples.  However sometimes concentrations were reported
for even smaller volumes than some shown as missing.   
Such values seemed suspect.  There are issues to address
when values are close to detection limits.  

There many reasons data is not available and hence in 
some since missing.  

Federal agencies often suppress values with high uncertainty.
This is often related to small sample sizes.  

When small populations are involved federal agencies often
suppress values due to concerns about confidentiality.

The absence of data is often overlooked. Sometimes data is 
absent because it purposely not collected or totally hidden.  
Detailed data on the distribution of cigarette smoke exposure to 
local populations is not collected. Better than this would
be individual data on doses for variety of toxics, carcinogens,
and mutagens.  Data collection is expensive, but the expense
is not necessary dominant reason for data not begin collected.
No data. No responsibilty. No liability. We could learn a
lot if we could see the flow of money.      

Missing data can be characterized if different ways.
In some situation analysts proceed assuming the data
is missing at random.  When data is missing at random
the chances of producing a biased analysis by omitting
cases and variables with missing data may be reduced.
Further the use of imputation methods to fill in missing
data values is easier to justify. 
  
There are various approach to imputation. 
Random Forests provides a data imputation approach.
I have little background so will not address this or
than say I am more comfortable when imputation
seems more like interpolation and extrapolation. 

This assignment jumps in and removes variables and cases
with missing data. The task is to illustrates some
approaches to regression and difficulties in terms of
variables selection and model interpretation relative to
the variables.
      
3.1 Find and remove columns with a lot of missing data________________ 
    Do not remove the planned dependent variable.  

## Run: count the number missing in each column and remove variables  

myNA <-  function(x) sum(is.na(x))
missDat <- apply(crimeDat, 2, myNA)

missDat   

# Keep the Violent Crime column with  221 or less missing
crimeFix1 <- crimeDat[, missDat < 222]

dim(crimeFix1)
head(crimeFix1)

## End

3.2  Find and remove cases with missing data________________________

## Run:

caseMiss <- apply(crimeFix1, 1, myNA)
table(caseMiss)
crimeClean <- crimeFix1[caseMiss == 0, ]

dim(crimeDat)
dim(crimeClean)

## End

4. Using scatterplots to look a candidate dependent variables______

## Run

colnames(crimeClean)
subs <- c(106, 108, 110, 112, 114, 116, 118, 120, 121, 122)
colnames(crimeClean)[subs]
windows(width=8, height=8)
pairs(crimeClean[, subs], gap=0, las=1)

windows(width=8, height=8)
violent <- c(106,108,110,112,121)
pairs(crimeClean[, violent], gap=0,las=1)

windows(width=8, height=8)
subs <- c(114, 116,118, 120, 122, 121)
pairs(crimeClean[, subs], gap=0, las=1)

## End

5. Outliers and case influence discussion____________________

When present, the extreme outliers set variable scales
so cause graphics resolution problems when seeking to 
look at the body of the data. 

In some cases making bigger plots can reduce the plotting
density and help reveal patterns. However but reclaiming the scale
space used to accommodate outliers is an important alternative.  
Two options to keeping the outliers are to 
1) transform that data to bring in heavy tails.  
2) Winsorize the outliers (assign a threshold value and recode
                           all values beyond the threshold
                           as the threshold value.
3) For graphics sliders, I have used piecewise line scales, 
   since nonlinear transformation make the units of measure
   foreign to many people.  
    
Outliers can influence performance of modeling tools we
use to help bring out patterns. The distribution
of spacings between points can be important for
both smoothing and density estimation. 

Outliers treatment options in the modeling
context also include use robust/resistant procedures.
      
In a regression context the impact of outliers depends
on the type of model used and role variables play
in the model.  In the terminology here, the dependent
variable is the variable being modeled and 
other variables are loosely called either explanatory
or predictor variables.    

In linear regression outliers in explanatory variables
and may have a large influence on the model coefficients,
predicted values, residuals and residual variance.  
See the class notes on 
Random Basis Vectors and Linear Regression as Projection.pdf

The influence of a case depends on both the dependent variable
value and leverage provide by its explanatory variables.  
A cases leverage is given by cases diagonal element in the
projection matrix. If the diagonal element is 1 the case rules.
The predicted value is the same as observed value. Cases with
diagonal values greater than 2p/n, where p is the number of
explanatory variables and n is the sample size, are considered to
have large leverage.  

Cook's distance assesses how much a case influences the regression
coefficients.  A high leverage case may be consistent with the body
of the data and its remove changes the coefficients little. 
However if a high leverage case's dependent variable is telling
a very different story the coefficients change. In summary words,
case influence depends on both the dependent variable value and
leverage from explanatory variables.

Random Forests regression uses explanatory variables to partition
the data. Only explanatory variable ranks matter.  A monotone 
transformation of an explanatory variables makes no difference.
The size gaps (greater than zero) between that sorted variable
values makes no difference. In other words the influence of a
case with an outlier in a explanatory variables is quite limited.

The impact of outliers in the dependent variables can
be thought of in terms of how residuals are treated
in model fitting and assessment.  The mean square error
criterion squares residuals before taking the average.
Hence a dependent variable outlier producing large residual,
contributes heavily to the MSE which the model seeks
to minimize.  If the outlier is a bad value it can distort
the model.  Note the outliers can be perfect good value
and extend the domain where we have confidence in the model.  

Other modeling approach can be more resistant to anomalous
values.  For example using the average of absolute residuals
as a model fitting evaluation criterion, can reduce
the relative influence of cases with anomalous values. 

Both common linear regression and random forests use MSE.
Since the random forests samples cases when building trees,
a few cases with  "bad" outliers are likely to omitted a substantial 
fraction of the trees.  The this reduces the impact of the
"bad" outliers on the forest voting. 

I am more comfortable with linear models and a modest number of
variables.  My sense is the random forests provide some protect
which I am jumping in with new data.     

6.  Regression___________________________________________

6.1 Variable Selection____________________________________

Some variables may be undesirable as predictors.

I have concerns about using population as variable
since the rate per capital variables have already
incorporate population. Of course the impact of 
population size may not be linear.  

I have concerns about incorporating police variables
since they may both reflect response to crime or
influence crime. I, like most people, am inclined
to think in terms of cause and effect, and am
disinclined to think of police causing crime, but
what we have are associations. Still was tempted
to remove Variable 104, "LemasPctOfficDrugUn" based
on its label.

Many variables can be sufficiently correlated that
remove one or more can lead to an almost equally
good fit.  Variables 85 and 89, "OwnOccQrange" and
"RentQrange" are like this.  This hindsight removal
will slightly reduce the output below.     

I start by including most variables and see what
the modeling does.  I try to remember that my first
look will likely bias my thinking throughout the
exploration. 

The script below removes the other candidate
dependent variables.  Those related to non-violent
crime might be retained.  There as some variables
such a street number and counts that are not populations
adjusted that are tempting to remove up front.   

## Run: select variables

subs = c(3:84, 86:88, 90:104, 121)
colnames(crimeClean)[subs]
    
crimeReg <- crimeClean[,subs]
write.csv(crimeReg,file="crimeReg.csv",quote=FALSE)

## 

6.2 Exploratory 1D graphics_______________________________

The following provide three views of a variable
and is useful for univariate data description/exploration.  
It could be turned into a function and improved.
Two plotting functions used were created by others.
More time is needed to address inconsistencies among
the plots  terms of background fill, grid lines, ticks,
tick labels and x-axis labels. The basic context is 
okay.    

The example includes the variable names and emphasizes
units of measure. Graphics for final presentations and projects
should include units of measure.     

## Run

install.packages("car")
library(car)
source("myDensity.r")

windows(w=7, h=7)
layout(mat=matrix(1:4, ncol=2))
oldpar <- par(mai=c(.72, .68, .4, .2))
cexL <- 1.3
cexS <- 1
var <- crimeReg$ViolentCrimesPerPop
varLab1 <- "Violent Crimes"
varLab2 <- "In Communities"
varUnits <- "Rate per 100,000 People"

plot(c(0, 1), c(0, 1), axes=FALSE, type='n', xlab='', ylab='', main='')
usr <- par("usr")
rect(usr[1], usr[3], usr[2], usr[4], density=-1,
  col=rgb(.95, 1.00, .95), border="black")
text(.5, .7, varLab1,  cex=cexL, font=2)
text(.5, .5, varLab2,  cex=cexL, font=2) 
text(.5, .2, varUnits,  cex=cexL, font=1)

qqPlot(var, las=1, xlab="Normal Quantiles", ylab=varUnits,
  main="Q-Q Plot")

myDensity(var, units=varUnits) 

boxplot(var, horizontal=TRUE, col=rgb(0, 0.85, 1),
  tck=.05, mgp=c(2, 0, 0), main="Box Plot", xlab=varUnits)

## End

6.3 Power transformations________________________

The crime rate distribution is really skewed.  
The boxcox() function can use results from
a linear model to suggest a power transformation.

## Run
lm1 <- lm(ViolentCrimesPerPop ~ ., data=crimeReg)

library(MASS)
windows(w=6, h=6)
boxcox(lm1)
power <- boxcox(lm1, lambda=seq(-1, 1, .05), plot=FALSE)
power

# There is power around .2 that proved a maximum
# Which one is it

sub <- which(power$y == max(power$y))
power$x[sub]

## End

## Run
windows(w=7, h=7)
layout(mat=matrix(1:4 , ncol=2))
oldpar <- par(mai=c(.72, .68, .4, .2))
cexL <- 1.3
cexS <- 1
dep <- crimeReg$ViolentCrimesPerPop
var <- dep**.2
varLab1 <- "Violent Crimes"
varLab2 <- "In Communities"
varUnits <- "(Rate per 100,000 People)**.2"

plot(c(0, 1), c(0, 1), axes=FALSE, type='n', xlab='', ylab='', main='')
usr <- par("usr")
rect(usr[1], usr[3], usr[2], usr[4], density=-1,
  col=rgb(.95, 1.00, .95), border="black")
text(.5, .7, varLab1,  cex=cexL, font=2)
text(.5, .5, varLab2,  cex=cexL, font=2) 
text(.5, .2, varUnits,  cex=cexL, font=1)

qqPlot(var, las=1, xlab="Normal Quantiles", ylab=varUnits,
  main="Q-Q Plot")

myDensity(var, units=varUnits) 

boxplot(var, horizontal=TRUE, col=rgb(0, 0.85, 1),
  tck=.05, mgp=c(2, 0, 0), main="Box Plot", xlab=varUnits)

## End

6.4 Regression diagnostics________________________________ 

Does the transformation make a difference in 
terms of the regression diagnostics?  

The plots below are interactive.  Click in the window
to advance to the next plot.     


## Run

windows()
plot(lm1)

## End

The diagnostics plots all indicated problems. 

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
   Case 22 has strong influence as does case 128.      
 

## Run 

crimeReg[row.names(crimeReg)=="22", ]

## End

Community 22 has population of 7.3 million.  The values for count
variables like Number Urban are high.  These size variables give
the community very high leverage. We will deleted all the count variables
except population. 

## Run

crimeReg2 <- crimeReg[, -c(11, 28, 50, 52, 90, 91)]

lm2 <- lm(ViolentCrimesPerPop ~ ., data=crimeReg2)
windows()
plot(lm2)

## End

The same problems appear. Its time to try
the boxcox() power transformation from above.

## Run

lmPower1 <- lm(ViolentCrimesPerPop**.2 ~ ., data=crimeReg2)

windows()
plot(lmPower1)

## End

1) Residuals versus fitted values
   Not too bad 
   Flagged Case: 444, 941, 2177  

2) Q-Q normal plot. 
   Thick tails but not as bad
   Flagged cases: same as above  

3)  Spread location plots:
    Looks pretty good  
    Flagged cases: same as above

4) The influence plot: 
   Flagged cases: 386, 1639, 22
   Case out of bounds: none  


## Run

summary(lmPower1)

## end 

 
6.5 Stepwise Variable Removal_____________________________________________
 
The R-Square value is .71. There is a substantially amount
(29 percent) of variability that is not explained by the
explanatory variables.  Note the variables list on the console
summary with at least asterisk on the right are statistically
significant with p-values <.05. Some have
have much smaller values. Still many are not statistically
significant. To have fewer variables to consider
we can turn on backward stepwise regression and
then focus the variables that survive.

With the default trace = 1 in the step() function below,
the list of variables in each model appears in the 
R console and gets shorter one variable at a time.  
The steps get faster as more variables are removed.  

I liked seeing the steps so I know progress was being made.
In this case I don't think showing the output takes too
much additional time. 

The criterion used to remove variables is the Akaike
information criterion, AIC.  This is 2p - 2ln(L) where
p is the number of variables in the model and L is the
log likelihood.  The goal is to minimize
this values. Having fewer parameters helps
and so does a higher likelihood. 
Likelihoods are positive values bounded by 0 and 1. 
The log of such likelihoods is negative but less
extreme with the likelihood is larger. The product
with -2 is positive.  We want sum to be small
so don't want the likelihoods to get small from 
removing to many variables and fitting the data
poorly.    
  
## Run

lmPower2 <- step(lmPower1)
plot(lmPower2)

summary(lmPower2)

## End

This is progress of sorts.
The fit is about the same and the variables are fewer.

Call:
lm(formula = ViolentCrimesPerPop^0.2 ~ householdsize + racePctWhite + 
    racePctHisp + pctUrban + medIncome + pctWWage + pctWInvInc + 
    pctWRetire + medFamInc + blackPerCap + OtherPerCap + PctPopUnderPov + 
    PctBSorMore + PctEmploy + PctEmplManu + PctOccupMgmtProf + 
    MalePctDivorce + MalePctNevMarr + PersPerFam + PctFam2Par + 
    PctKids2Par + PctTeen2Par + PctWorkMomYoungKids + PctWorkMom + 
    PctKidsBornNeverMar + PctRecImmig10 + PctNotSpeakEnglWell + 
    PctLargHouseFam + PctLargHouseOccup + PersPerOccupHous + 
    PctPersOwnOccup + HousVacant + PctHousOccup + PctHousOwnOcc + 
    PctVacMore6Mos + OwnOccHiQuart + RentLowQ + RentHighQ + MedOwnCostPctIncNoMtg + 
    PctForeignBorn + PctSameState85 + PctUsePubTrans + LemasPctOfficDrugUn, 
    data = crimeReg2)

Residuals:
     Min       1Q   Median       3Q      Max 
-1.51810 -0.24292 -0.00446  0.24283  1.80674 

Coefficients:
                        Estimate Std. Error t value Pr(>|t|)    
(Intercept)            7.122e+00  6.496e-01  10.964  < 2e-16 ***
householdsize         -2.593e-01  9.918e-02  -2.614 0.009013 ** 
racePctWhite          -7.873e-03  1.357e-03  -5.800 7.76e-09 ***
racePctHisp            3.366e-03  1.715e-03   1.962 0.049866 *  
pctUrban               1.257e-03  2.610e-04   4.815 1.59e-06 ***
medIncome             -2.377e-05  6.777e-06  -3.507 0.000464 ***
pctWWage              -2.145e-02  4.428e-03  -4.845 1.37e-06 ***
pctWInvInc            -1.249e-02  2.312e-03  -5.401 7.48e-08 ***
pctWRetire            -1.008e-02  3.558e-03  -2.833 0.004659 ** 
medFamInc              2.196e-05  5.967e-06   3.680 0.000240 ***
blackPerCap           -1.951e-06  1.132e-06  -1.723 0.084988 .  
OtherPerCap            2.944e-06  1.180e-06   2.496 0.012643 *  
PctPopUnderPov        -1.305e-02  3.725e-03  -3.504 0.000470 ***
PctBSorMore           -4.873e-03  3.385e-03  -1.440 0.150159    
PctEmploy              1.129e-02  4.166e-03   2.709 0.006804 ** 
PctEmplManu           -4.696e-03  1.443e-03  -3.253 0.001162 ** 
PctOccupMgmtProf       1.174e-02  3.975e-03   2.953 0.003189 ** 
MalePctDivorce         4.477e-02  7.929e-03   5.646 1.89e-08 ***
MalePctNevMarr         1.113e-02  3.271e-03   3.401 0.000685 ***
PersPerFam            -7.612e-01  3.114e-01  -2.444 0.014617 *  
PctFam2Par             2.169e-02  7.787e-03   2.785 0.005403 ** 
PctKids2Par           -3.413e-02  6.933e-03  -4.922 9.31e-07 ***
PctTeen2Par            3.952e-03  2.308e-03   1.713 0.086942 .  
PctWorkMomYoungKids    4.059e-03  2.855e-03   1.422 0.155231    
PctWorkMom            -1.009e-02  3.978e-03  -2.537 0.011271 *  
PctKidsBornNeverMar    2.301e-02  8.784e-03   2.619 0.008885 ** 
PctRecImmig10         -1.778e-02  9.976e-03  -1.783 0.074820 .  
PctNotSpeakEnglWell   -2.977e-02  8.692e-03  -3.425 0.000629 ***
PctLargHouseFam        6.082e-02  3.076e-02   1.977 0.048207 *  
PctLargHouseOccup     -6.706e-02  3.308e-02  -2.027 0.042768 *  
PersPerOccupHous       9.889e-01  2.674e-01   3.698 0.000224 ***
PctPersOwnOccup       -2.059e-02  6.427e-03  -3.203 0.001383 ** 
HousVacant             6.110e-06  1.680e-06   3.637 0.000283 ***
PctHousOccup          -7.244e-03  2.349e-03  -3.083 0.002079 ** 
PctHousOwnOcc          1.623e-02  6.395e-03   2.538 0.011232 *  
PctVacMore6Mos        -2.275e-03  8.665e-04  -2.625 0.008725 ** 
OwnOccHiQuart         -7.076e-07  2.696e-07  -2.624 0.008754 ** 
RentLowQ              -7.384e-04  1.939e-04  -3.807 0.000145 ***
RentHighQ              5.039e-04  1.781e-04   2.829 0.004724 ** 
MedOwnCostPctIncNoMtg -2.093e-02  8.154e-03  -2.567 0.010330 *  
PctForeignBorn         2.085e-02  5.429e-03   3.840 0.000127 ***
PctSameState85         3.772e-03  1.824e-03   2.068 0.038790 *  
PctUsePubTrans        -6.812e-03  2.703e-03  -2.520 0.011802 *  
LemasPctOfficDrugUn    1.148e-02  3.361e-03   3.416 0.000649 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

Residual standard error: 0.3816 on 1857 degrees of freedom
Multiple R-squared: 0.7095,     Adjusted R-squared: 0.7028 
F-statistic: 105.5 on 43 and 1857 DF,  p-value: < 2.2e-16 

There are fewer variables and the R-squared is about the same.

7. Looking at the Explanatory Variables____________________________

Below we restrict attention of variables still in the 
lmPower2 model to illustrate looking at variables.
There can be advantages to transforming skewed or heavy
tailed variables.  This can really change how the 
variables will correlated with each other and with 
the dependent variable. Some variables still have extreme
values, that we may want to address.   

7.1  Looking at the univariate distributions

## Run

tmp <- lmPower2$terms
termLab <- attr(tmp, which="term.labels")
termLabPlus <- c(termLab, colnames(crimeReg2)[95])
crimeReg3 <- crimeReg2[, termLabPlus]

library(lattice)

windows()
par(ask=TRUE)
for(i in 1:length(termLab)){
show(densityplot(crimeReg3[, termLab[i]], xlab=termLab[i]))
}

## End

I thought about removing the LemasPctOfficDrugUn variable at
the beginning because I would hope it is more a response
to crime than a "causal" variable.  Now I wish I had.  
The clump at zero and the outliers suggest high leverage
and association with large communities.  

With a smaller set of variables we could return to the case removal
for missing values and perhaps regain for cases for a
revised analysis.

7.2  Looking variables pairs and correlations__________________________  

The variables are non-negative

A first thought is that signs of the coefficients
directly tell a story.

A positive signs means higher variable values lead to 
higher crime rates.  Similarly a negative sign means
high variable values lead to lower crime rates.  

HOWEVER, many variable are correlated and the signs
be misleading as variable may be countering balancing
over or under estimates due to other variables.

Below is a one form of a colors matrix.  
You may prefer heat maps or other variations.  
There are many variation.  There previous version
shown in class uses position along a scale to 
show magnitude and color to show the sign.   


A seriation method can be used to order
the rows and columns. The approach here
uses 1-abs(corMat) as a distance matrix
which is sometimes called the Pearson
absolute distance, and multidimensional
scaling into the 1D to obtain coordinates
to order the variables.  

## Run

corMat <- cor(crimeReg3[, termLab])   # get correlations
x <- cmdscale(1-abs(corMat), k=1)     # seriation 
ord <- rev(order(x))
corMatOrd <- corMat[ord, ord]
round(corMatOrd, 2)

# compute colors

tmp <- as.vector(corMatOrd)
low <- tmp< 0
lowBrk <- quantile(tmp[low], c(0, .25, .50, 1))
highBrk <- quantile(tmp[!low], c(0, .50, .75, 1))
brk <- c(-1.01, lowBrk[c(2, 3)], 0, highBrk[2:4])
colorSub <- cut(tmp, brk, label=FALSE)
mat <- matrix(c(
118, 42, 131, 
175, 141, 195, 
231, 212, 232, 
217, 240, 211, 
127, 191, 123, 
27, 120, 95), ncol=3, byrow=TRUE)
colors=rgb(mat[, 1], mat[, 2], mat[, 3], max=255)

nr <- nrow(corMatOrd)
x <- 1:nr-.5
cen <- expand.grid(list(y=rev(x), x=x))

windows(w=10, h=8)
par(mai=c(.4, .5, .5, .2))
plot(c(-10, 43), c(0, 43), type='n', axes="FALSE", ylab='',
main='Double ended scale:  Shades of purple negative, Shades of green positive')
mtext(side=1, line=0, 
  'Two major variable groups. Seriation by 1D MDS, Pearson Distance: 1-abs(cor)')
# could use image()
rect(cen$x-.5, cen$y-.5, cen$x+.5, cen$y+.5, col=colors[colorSub],
  border=rgb(.2, .2, .2))
text(rep(-10.5, 43), y=rev(x), rownames(corMatOrd), adj=0, cex=.75)

## End


The plot shows a set of 13 variables at the top left that are 
for the most part strongly correlated (positively or negatively)
with each other. You can see that pctWorkMom and pctWhite are
negatively correlated with first seven other variables.   

Check out the large set on strongly correlated variables at the
bottom right.  Note that large percent under the poverty level
means high poverty.  Look at this row and the row above, 
malePctDivorce.  On the right the rows are purple except
for a square where they correlated positively with themselves
and each other.  

To learn a bit the unique contribution of 
each explanatory variable we can regress
each explanatory variable against
all the other explanatory variables.  A low
R-squared says the unique contribution can be
substantial.  A high R-squared means the variable
is mostly a linear combination of other variables
interpretation of variables contribution to the 
model is confounded wtih the contributions of
other some of the other variables.  

7.3 Bivariate outliers and scagnostics 

For some bivariate outliers neither of the 
pairs values is a univariate outliers.  

The scagnostic packages is design to find bivariate
outliers and other patterns in a scatterplot
matrix.  The nine patterns are called 
outlying, skewed, clumpy, 
sparse, striated, convex,   
skinny, stringy, and monotonic. 

I am typically most interested in the outlying
and monotonic patterns.

With 44 variables there are 44*43/2 = 946
(This is a much smaller set than described in class.)  

I have written the results to a file that you
can read.   
   
# install.packages("scagnostics")
# library(scagnostics)
# manyPairs <- scagnostics(crimeReg3)
# write.csv(manyPairs, file="manyPairs.csv", quote=FALSE)

## Run

manyPairs <- read.csv("manyPairs.csv", row.names=1)
manyPairs[, 1:3]

## End

The rows provide the nine diagnostics.  All use a
[0 1] scale.  High values suggest presence of
the pattern.  The column labels indicate the 
pair of variables used. The first column has
as skew index of .82.  The household size has
a pretty skewed distribution for starters.     

## 
windows()
colnam <- colnames(crimeReg3)
plot(crimeReg3[, 1] , crimeReg3[, 2], xlab=colnam[1], ylab=colnam[2])
## End

8. Closing remarks
 
The regression assignment stops here.  It could go on.
Linear regression methodology has developed over the years
and there is fair amount to learn.  
  
We haven't transformed explanatory variables or considered
powers and products of the variables.  However we can only
go so far with the data at hand.  The web sites indicates
there are other important factors to consider
that are not a part of the data set.  The web site example
cites the connection between tourists and crime. There
is no tourist data.  

Next we turn attention to random forests where the modeling
approach is a little more protective of new analysts.




















