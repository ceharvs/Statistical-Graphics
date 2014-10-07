# Topic:  Best Subset Select Methods
#
# Reference: Introduction to Statistical Learning
#   with R = ISLR 
# R script source:  
#   www.StatLearning.com script for chapter 6 
# R script comments, added graphics and a few minor edits
#   by Daniel Carr

# Sections
# 1.  Missing data remove and quick looks at the Hitters data
# 2.  Finding best subsets
# 2.1 Redesigning a table
# 2.2 Best subset for 19 variables
#     and model evaluation criteria
# 2.3 Four plots per page showing 4 criteria
#     and with dots indicated best models
# 3.  Forward and Backward Stepwise Selection
# 4.  Split cases into training and test sets
#     Model the training set and use coefficents
#     to predict test set values and obtain
#     the MSE using the observed- predicted values. 
# 4.1 Writing a prediction function
# 4.2 10 fold cross validation
#     best subset selection

# Due:
# Plot from 2.1   

# 0. Setup

library(ISLR)
library(leaps)
library(lattice)
# library(GGally)  for new year additions. 

# 1. Missing data remove and quick looks at the Hitters data

# fix(Hitters) # A window with an editable table  
head(Hitters)
tail(Hitters)
names(Hitters) # same as colnames 
dim(Hitters)  # rows and columns

# Check for missing data
sum(is.na(Hitters$Salary))

# Data preparation: 
#   For this assignment
#   omit rows with missing data
#
#   Sometimes the missing data
#   has a story to tell

Hitters=na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters)) # quick check

# We will only look at the data briefly
# here and focus on subset selection
    
# We can note the categoral variables 

splom(Hitters)

# Removing the provides a little 
# more resolution for remaining the variable.  

splom(Hitters[,-c(14,15,20)])
# We see some functional relationships.  
# The AtBat and Hits are related
# All of the career statistics variables
# are related.  

# 2. Finding best subsets______________ 

# The leaps package will finding best subsets
# of p variables. For p variables there are 2**p
# subsets counting the null subset. The well-
# designed algorithm can identify non-competitive
# subsets of variables so does not have to check
# all  2**p combinations.   

regfit.full=regsubsets(Salary~.,Hitters)
summary(regfit.full)

2.1 Redesigning the table
# The table is a candidate for redesign
# We typically can't see the whole table
# layed out in rows and columns in the R 
# console.
# 
# The " "s are chart junk that make it
# harder to the *s.  
#
# Here a binary heat map could work
# with variable names as rows
# number of variables in model as columns.  
#  
# Focusing and sorting:  
# We could drop the unused variables and
# think about reordering the used ones. 
# 
# Can we find the needed information
# in the regfit.full object to make a plot
# leaps is on old R function. Results
# were often returned as a list
# 

is.list(regfit.full)  # Yes!  
names(regfit.full) # Component names

# What we want is likely in there
# somewhere if we can recognized it.

regfit.full$xnames
regfit.full$nbest
regfit.full$vorder
regfit.full$lopt # Eureka

# This has the stacked indices for 
# variables in the models, starting
# with the model with 1 variable.  

# Rename variables for convenience
varInd <- regfit.full$lopt
head(varInd,10)
varNam <- regfit.full$xnames

# First model index from varInd:  1
varNam[1]
  
# Second model indices: 1, 13
varNam[c(1,13)]

# Third model indices: 1, 3, 13
# 
varNam[c(1,3,13)]

# To avoid looking for the indices of the n variable
# model (counting the mean) we can write a little
# function that returns the desired subscripts.

subs <- function(n){
  if(n==1)return(1)
  b <- n*(n+1)/2
  return(seq(b-n+1,b))
}

# check
subs(1)
subs(2)
subs(3)

# Three variable model 
varNam[ varInd[subs(3)] ]

# Four variable model
varNam[ varInd[ subs(4)] ]

# Below we turn the ISLR table 
# into a plot R that also
# includes the intercept.
# This use R base level graphics

# Set the scales
# By default R will expand these

rx = c(0,8) 
ry = c(1,20)

# Set the plot margins in inches
#   bottom, left, top, right
# and save the previous setting 
oldPar <- par(mai=c(.2,1.8,1.1,.2))

# Just setup the plot
plot(rx,ry,type='n',axes=FALSE,xlab='',
  ylab='',main="Best Hitter Models with 1 to 8 Variables")

# Add points
# Note this plots the first variable at the top 
for (j in 0:n){
   points(rep(j,j+1),21-varInd[subs(j+1)],pch=21,bg="red",cex=1.5)
   abline(h=c(4.5,8.5,12.5,16.5),col=gray(.8)) 
}

# draw the plot border
box()

# put labels in the plot margins
mtext(varNam,side=2,at=20:1,adj=1,line=.3,las=1)
mtext(as.character(0:8),at=0:8, side=3)

# reset the device plot margon 
par(oldPar)

# A redesign could reorder selected
# variables and omit the rest.
# The ordering might be based
# on first appearance or frequency
# of appearance and tie breaking
# might be handled using the
# the original variable order. 
#
# The rows are perceptually grouped
# by drawing light gray horizontal

# 2.2  Best subset for 19 variables
#      and model evaluation criteria
      
regfit.full=regsubsets(Salary~.,data=Hitters,nvmax=19)
reg.summary=summary(regfit.full)
names(reg.summary)
reg.summary$rsq

# Extra:  for a comparison later. 
# MSE = RSS/n 
# Full model MSE 
model10.MSE = reg.summary$rss[10]/nrow(Hitters)

# 2.3 Four plots per page showing 4 criteria
#     and with dots indicated best models 
#
# Uses R base level graphics
# mfrow allocates space putting plot
# panels in row and columns
# Each plot() command advances to the
# next plot.  The points() function 
# add points to the currently 
# active plot

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

# Putting multiple panels in a plot with mfrow() was convenient
# but often wastes a lot of space. The y-axis resolution and
# scale labels are unless the plot is made very tall.
# 
# There is common x-axis.  There could be 4 vertically 
# aligned panels.  Yes, this is a candidate for redesign.
# There are outer layout options such latout() in R
# and my panel Layout functions in a file 
# that sourced.

# The minimum BIC model has a mean and six variables.
# The adjust R-squared is not
# but not that quickly increaseing so the model is  a reasonable, 
# We can take a look at the coeefficients

coef(regfit.full,6)


# The following plot with 4 panels look really bad!
p1ot(regfit.full,scale="r2")
plot(regfit.full,scale="adjr2")
plot(regfit.full,scale="Cp")
plot(regfit.full,scale="bic")

# We can show the plots individually to see the 
# the intended content.
par(mfrow=c(1,1))

# If there is only one active device
# Turning off the active device will make
# a new one active with default parameters
# available when needed.
#
# dev.off()

# The plots are still bad and beg for redesign.
plot(regfit.full,scale="r2")
plot(regfit.full,scale="adjr2")
plot(regfit.full,scale="Cp")
plot(regfit.full,scale="bic")

# A redesign might skip the r2 plot
# and add three panels of curves
# above the transposed red dot plot
# shown above. The above plots
# show diminishing returns for adding
# more variables.
# 
# The gray scale encodingfor a continuous
# variable is inferior.  

# 3. Forward and Backward Stepwise Selection

# The message here is the forward, backward
# and best subset selectiond don't always
# on the same variables for a give number
# of variables.  

regfit.fwd=regsubsets(Salary~.,data=Hitters,nvmax=19,method="forward")
summary(regfit.fwd)
regfit.bwd=regsubsets(Salary~.,data=Hitters,nvmax=19,method="backward")
summary(regfit.bwd)
coef(regfit.full,7)
coef(regfit.fwd,7)
coef(regfit.bwd,7)

# We could modify the earlier red dot plot
# to include all the models and use
# their colors of dots for best, forward
# and backward models of the given number of
# variables. The dot  would be offset 
# to avoid overplotting.

# 4. Split cases into training and test sets
#    Model the training set and use coefficents
#    to predict test set values and obtain
#    the MSE using the observed- predicted values. 

set.seed(1)
train=sample(c(TRUE,FALSE), nrow(Hitters),replace=TRUE)
test=(!train)

# Find the best training set models
regfit.best=regsubsets(Salary~.,data=Hitters[train,],nvmax=19)


# Obtain the test set design matrix 
test.mat=model.matrix(Salary~.,data=Hitters[test,])

# Reserve storage the mean square errors
# using train set cooefficients for 
# for test set datg
val.errors=rep(NA,19)

# For each number of variables
for(i in 1:19){
  # obtain the training set coefficients
  coefi=coef(regfit.best,id=i)

  # predict test set values 
  pred=test.mat[,names(coefi)]%*%coefi
  
  # Obtain the MSF
  val.errors[i]=mean((Hitters$Salary[test]-pred)^2)
}
round(val.errors)

loc = which.min(val.errors)
loc
round(coef(regfit.best,loc), 3)

val.errors[10]


# 4.1 Writing a prediction function

predict.regsubsets=function(object,newdata,id,...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}

# 4.2 10 fold cross validation
#     best subset selection
#
# Fit again with all the data
# Get the coefficients of the
# the best 10 variable model
#
# Remember this was best for the
# Cp criterion
regfit.best=regsubsets(Salary~.,data=Hitters,nvmax=19)
coef(regfit.best,10)

# Defined 10 subsets of cases
k=10
set.seed(1)
folds=sample(1:k,nrow(Hitters),replace=TRUE)

# Create matrix to MSE for each fold test set
# and using a model with from 1 to 19 variables 
cv.errors=matrix(NA,k,19, dimnames=list(NULL, paste(1:19)))

# The fold and number of variables loops 
for(j in 1:k){ # fold loop
  best.fit=regsubsets(Salary~.,data=Hitters[folds!=j,],nvmax=19)
  for(i in 1:19){ # number of variable loop
    pred=predict.regsubsets(best.fit,Hitters[folds==j,],id=i)
    cv.errors[j,i]=mean( (Hitters$Salary[folds==j]-pred)^2)
  }
}
mean.cv.errors=apply(cv.errors,2,mean)
round(mean.cv.errors)
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
# Cross validation helps to tell an overfitting story.  

# The authors go with model 11 as best 
reg.best=regsubsets(Salary~.,data=Hitters, nvmax=19)
round(coef(reg.best,loc), 3)
