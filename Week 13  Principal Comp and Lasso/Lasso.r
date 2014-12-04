                      Lasso Crime Rate Example
                         

This script provides a brief example illustrating a few
glmnet package functions. GLM standard for generalized
linear models. Here we use the gaussian model. This
means that mean squared error is part of model fitting
criterion to be minimization. 

(There are options for other types of data.  For example
for count data there are binomial and poisson models.) 

For glmnet models, the argument alpha = 1 specifies lasso
regression and alpha = 0 specifies ridge regression. These
include different shrinkage penalties as part of the fitting
criterion to be minimized.  See the class notes.

The script below roughly follows the general template in:
Introduction to Statistical Learning by James et al. 2013.    

Sections  1. Read the data, make training and test subsets

          2. Create training and test samples using
             randomly selected row subscripts

          3. Generating shrinkage tuning parameters to try

          4. Fitting lasso models with a vector of tuning
             parameters seeing coefficients go to zero

          5. Estimating the best tuning parameter, lambda,
             via 10-fold cross validation

          6. Making predictions for the test set
             and computing the mean squared error

          7. Looking at the remaining coefficients

Comments  8. Considerations in comparing
             lasso and random forest mode

          9. Quantiative models, graphics and
             verbal reasoning 

1. Read the data, make training and test subsets.

## Run_____________________________

crimeReg <- read.csv(file="crimeReg.csv", header=TRUE, row.names=1)
colnames(crimeReg)

# glmnet wants a Matrix of explanatory variables

x <- as.matrix(crimeReg[,1:80])

# the dependent variable: violent crime rate 

y <- crimeReg[, 81]

head(x)
head(y)

## End_____________________________

2. Create training and test samples using
   randomly selected row subscripts.

## Run___________________

nr <- nrow(x)
nr
trainingSize <- ceiling(nr/2) # half case for training

set.seed(37)
train <- sample(1:nr,trainingSize)

x.train <- x[train,]
y.train <- y[train]
head(x.train)
head(y.train)

# other half for testing
x.test <- x[-train,] 
y.test <- y[-train]
head(x.test)

## End__________________

3. Generating shrinkage tuning parameters to try 

## Run_____________________

lambda <- 10^seq(10, -2, length=100)

## End_____________________


4. Fitting lasso models with a 
   vector of tuning parameters and
   see coefficients go to zero from
   right to left.  
  

## Run_____________________  

library(glmnet)

lasso.mod <- glmnet(x[train,],y[train],
  alpha=1, lambda=lambda)
plot(lasso.mod)

## End ____________________ 

Note that by default glmnet lasso standardizes
variables before running the model. Documentation
says the coefficients are always returned on the
original scale.  I currently take this to mean
dependent_variable_units/explanatory_variable_units.

I have not yet verified the units in the
coefficient plot. 

In the plot the x-axis shows the sum
of the coefficient absolute values.
On the right the sum is large and primarily
based on one large magnitude positive
coefficient (the blue line) and one large
magnitude negative coefficient (the black line). 
The trajetories of two coefficients dominate
the scale of the  plot. A future example will
omit them to reveal more about other
trajectories. 

Going from right to left the tuning parameters
are increasing making the L1 norm smaller the
regression coefficients in the L1 norm shrink
to zero.  The numbers along the top
indicated the number of non-zero coefficients
remaining.  

A rough guess, about 60 coefficients
are still left in the model when the
extreme negative black line coefficient
is driven to zero by the shrinkage penalty.

A run guess is that 25 coefficients are left
when the extreme positive blue line
coefficient is driven to zero.  
  
5. Estimating the best tuning parameter lambda
   via 10-fold cross validation.

## Run____________________

set.seed(37)
cv.out = cv.glmnet(x[train,], y[train] ,alpha=1)
plot(cv.out)

best.lambda <- cv.out$lambda.min

## End__________________________


Ten-fold cross validation partitions the
training set into 10 subsets. It leaves one
subset out, fits a model using the composite
of the other 9 subsets, predicts the values
for the omitted subset, and obtains the errors.

This process is repeat 10 time with a different subset
left out of the model each time.  At the end
every case has been left out of the model once
and it prediction error has been obtained.

The function computes the mean of the squared errors,
MSE. The red dots in plot the MSE for each value
of the 100 tuning parameters. Similarly the function
computes values for bounds shown.

The dash vertical lines draw attention to and
interval where the MSE is about the same.
We pick lambda corresponding to the minimum MSE
but other choises are reasonable.    
 
6. Making predictions for the test set 
   and computing the mean squared error

## Run_________________

lasso.pred <- predict(lasso.mod, s=best.lambda,
  newx=x.test)

mean((lasso.pred-y.test)^2)

## End_________________

7. Looking at the remaining coefficients  

## Run___________________

out <- glmnet(x, y, alpha=1,lambda=lambda)
lasso.coef=predict(out,type="coefficients",
  s=best.lambda)
# lasso.coef is in a sparse matrix format
# 
coefMat <- as.matrix(lasso.coef)
coefMat <- cbind(coefMat,coefMat>0)
colnames(coefMat) <- c("Coefficent","Keep")
round( coefMat[coefMat[,2]==1,] ,3)

## End____________________

                    Coefficent Keep
(Intercept)           1656.956    1
racepctblack             4.376    1
pctUrban                 0.650    1
AsianPerCap              0.001    1
OtherPerCap              0.001    1

MalePctDivorce          32.616    1
PctKidsBornNeverMar     62.190    1
PctPersDenseHous         8.811    1
PctHousLess3BR           0.519    1
HousVacant               0.006    1

PctVacantBoarded         8.832    1
RentQrange               0.222    1
MedRentPctHousInc        1.608    1
PctForeignBorn           1.271    1

8. Considerations in comparing
   lasso and random forest models

The MSE for lasso was 151,730. The MSE for
the choose six variable random forest models
was 131,875  The variables were

racePctWhite
FemalePctDiv
PctKidsBornNeverMar   

PctPersDenseHous
PctUsePubTrans
PctPopUnderPov 

However the comparison was not a fair for several
reasons

  a)  The lasso training set model only made use of
      half of the data.   
  b)  In terms of explanatory variables
      this lasso example does NOT include
      transformed explanatory variables

      Transforming skewed explanatory variables
      could help lasso. As discussed before
      random forests fits remain the same
      for monotone transforms of explanatory
      variables.

Both models might benefit from including interaction
terms and other combinations of explanatory variables.
 
Neither model transformed the dependent variable,
violent crime rate. Both would benefit but not
necessary to the same amount.

9.  Quantitative models, graphics and verbal reasoning

Models provide lenses through which we can see
patterns that are not directly accessible to our vision.
Our vision can help in data curation and in model criticism
and in gaining insight.  Neither the power of models nor
the power of our vision should be ignored.  

There other many other models to try, more graphics to produce
and graphic designs to create.  Research in human perception
cognition and in information visualization and provides
a foundation for creating better designs. Eventually models may
include cognitive complexity penalty functions to provide a
better bridges among worlds of quantitative models, visualization,
verbal reasoning and additional forms of human thought
and motivation.
           
  



