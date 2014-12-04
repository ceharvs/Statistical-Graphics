# Edit script for ISLR Random Forests
# Read along in Section 8.3.3
# Nothing Due from this script

library(randomForest)
library(MASS)

# Look at part of the Boston house price data set
head(Boston)

# Find out about the variables
?Boston

# Make a training set
set.seed(1)
train = sample(1:nrow(Boston),nrow(Boston)/2)

# Fit a Random Forest Model
set.seed(1)
bag.boston=randomForest(medv~.,data=Boston,
  subset=train,mtry=13,importance=TRUE)
bag.boston

# Predict prices for the test set
yhat.bag = predict(bag.boston,newdata=Boston[-train,])

# Plot
boston.test = Boston$medv[-train]
plot(yhat.bag, boston.test,las=1, 
main='Own Occupied Housing Prices in $1000 Units')
abline(0,1,col="red")
mean((yhat.bag-boston.test)^2)

# Try using a smaller tree

bag.boston=randomForest(medv~.,data=Boston,subset=train,mtry=13,ntree=25)
yhat.bag = predict(bag.boston,newdata=Boston[-train,])
mean((yhat.bag-boston.test)^2)

# Assess variable importance using two criteria 

set.seed(1)
rf.boston=randomForest(medv~.,data=Boston,subset=train,mtry=6,importance=TRUE)
yhat.rf = predict(rf.boston,newdata=Boston[-train,])
mean((yhat.rf-boston.test)^2)
round(importance(rf.boston),1)
round(varImpPlot(rf.boston)

