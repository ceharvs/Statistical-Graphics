          Random forest regression for crime data
By:       Daniel Carr
File:     Random forest crime data.r 

Topics:   1. Fitting a random forest regression model
          2. Thinking about case leverage and influence
             in the random forest context
          2.1 Only explanatory variable ranks matter
          2.2 The influence or impact of a dependent
              variable outlier

          3.  Variable importance and selection
          3.1 Reduced model 1
          3.2 A seven variable model
          3.3 Another try with seven variables
          3.4 A six variable model
          3.5 Assignment: Find a different six
              variable model
          3.6 Looking at variable pairs

          4.  Case proximity and variable importance for
              multidimensional scaling, clustering,
              and graphs (vertex and edge)    
          4.1 Multidimenisonal scaling
          4.2 Clustering comment
          4.3 Side note: graphs and the Fiedler projection 
          4.4 Proximity calculation examples
          4.5 Proximity based case distance plots
          4.6 Comments on constructing explanatory variables

          5.  Local variable importance

          6.  Loose end: new data predicted values 


Needs:    crimeReg.csv - see linear regression
                         assignment
          Packages  randomForest, rgl

Due:      3.  Variable importance plot crimeRf
          3.1 Variable importance plot
              Pairs plot
          3.4 Variable importance plot
          3.5 Model results as indicated 
          3.6 Variable pairs plot 
          4.5 One 2d proximity-base MDS scatterplot
                 your choice
              Both 3d proximity-based MDS plot
          5.  The 3d local variable importance
              based scatterplot             

1. Fitting a random forest model 

## Run: set up
  
library(randomForest) 
?randomForest

## End

Read at least part of the help file. 

Parameters I changed

ntree: 1000
  This will take while to run.
  It may be more than needed. 
  You may reduce this to 500 if you want.

importance: TRUE  
  I want to thing about important variables  

keep.forest:    FALSE
  I was concerned this would be big. It probably isn't. 

proximity:      FALSE
  With 1901 cases the proximity (distance) matrix
  is pretty big and takes time to calculate.  
  I am more interest in variable selection than clustering
  cases (communities) based on crime rate prediction
  as least at the beginning

Two more of a few parameters others of interest

mtry:      The number of variables sampled at each node to 
           to use in splitting the cases.  The defaults
           are not necessarily the best choice.   

strata:    A (factor) variable that is used for stratified sampling.
           We might for example to sample different proportions
           of small, middle and large population to better match
           the national proportions.  
  
## Run:  This data from a previous another assignment
#        had removed some varibles and many cases with
#        missing data  
 
crimeReg <- read.csv(file="crimeReg.csv", header=TRUE, row.names=1)
colnames(crimeReg)

set.seed(4543)  # We many want to repeat the analysis
crimeRf <- randomForest(x = crimeReg[ , 1:80], y=crimeReg[, 81],
          importance=TRUE,  proximity=FALSE, ntree=1000,
          keepForest=FALSE)
crimeRf

## End_______________________________________

Call:
 randomForest(x = crimeReg[, 1:80], y = crimeReg[, 81],
   ntree = 1000,      importance = TRUE,
    proximity = FALSE, keepForest = FALSE) 

               Type of random forest: regression
                     Number of trees: 1000
No. of variables tried at each split: 26

          Mean of squared residuals: 125693
                    % Var explained: 66.03

The paper cited below says,  
"All numeric data was normalized (0-1), 
ViolentCrimesPerPop was predicted
(all other crime attributes were eliminated)
Best mean absolute error obtained was .096
(on normalized data)" 

[Redmond and Highley 2009] Redmond, M., and Highley, T.,
Empirical Analysis of Case-Editing Approaches for Numeric
Prediction. In International Joint Conference on Computer,
Information, and Systems Sciences and Engineering (CISSE)
subconference International Conference on Systems, 
Computing Sciences and Software Engineering (SCSS).
University of Bridgeport, CT, December 2009.

How does our minimal thought model compare in terms of the
normalized data mean absolute error? 

## Run: mean absolute error for [0 1] scaled data

dep <- crimeReg[, 81]
sc <- diff(range(dep))
sc
scaledRes <- (dep - predict(crimeRf))/sc 
mean(abs(scaledRes))  # .0476

## End_____________________________________

Our model looks a lot better at first glance.
In fact if we use crime rate to the .2 power
as in the previous assignment regression model,
the percent variance explained goes up to .70
as shown below so we can get an even better fit.
However, reading their paper may reveal differences
that make the comparison inappropriate.

# set.seed(4543)  # We many want to repeat the analysis
# crimeRf <- randomForest(x = crimeReg[ , 1:80],
#            y=crimeReg[, 81]**.2,
#           importance=TRUE,  proximity=FALSE, ntree=1000,
#           keepForest=FALSE)
# crimeRf
#
#
# Call:
# randomForest(x = crimeReg[, 1:80], y = crimeReg[, 81]^0.2, ntree = 1000,      importance = TRUE, proximity = FALSE, keepForest = FALSE) 
#               Type of random forest: regression
#                     Number of trees: 1000
# No. of variables tried at each split: 26
#
#          Mean of squared residuals: 0.1466593
#                    % Var explained: 70.05
#
## End

There can be many reason why the results differ
from the results here. 

In the linear regression crime data assignment
we removed variables that have missing data for
explanatory variable values and cases with
missing dependent variable values. Their work
may have imputed values for those cases, and
this could make a big difference. 

I choose not to address imputation methods
in the class. However could have used
random forests methods for impute values for
the missing values even if our goal was to 
use other methods for the rest of the modeling
Random forest has two methods for imputing
values for missing explanatory variable values
and a method of imputing values for missing
dependent variable in the training set. 

They may have restricted the data they used
in other ways by on criminology theory or
a variety of other reason. They have may 
used cross validation which isn't exactly the
same as the random forest procedure for
assessing error. 

Note that the mean square error reported by
random forests is 125693 which seems huge
but is not that huge when compared to the crime
rate range (max - min) squared which is
4870**2 = 23.7 million. The percent of
variance explained is a scale free so a
better criterion for understanding and
making comparisons. 

Here we will focus primarily on the
violence crime rate without transformation.
The results using a the .2 power
transformation model we considered in 
regression are comment out above.  The
fit is better, but the results would be hard
to explain.

For now we seek a smaller number of variables
that provide a decent fitting model.
This usually leads to easier explanations
which can be help in obtaining deeper 
understanding.  

Given the dependent variable is not transformed,
the major message from the models below is that
in terms of fitting the crime data there are many
almost equally good models.

For example in my biased view, strong family 
cohesiveness or stability is related to lower
crime. There are several variables that are
positively or negatively related to my
notion of family stability and I conjecture
that different subsets of the variables
may served to adjust for family cohesiveness
or stability.  
  
2. Thinking about case leverage and influence
   in the random forest context.

For linear regression think about the 
leverage of case explantory variables. 
 
2.1  Only explanatory variable ranks matter

A monotone transformation of an explanatory
variables makes no difference.  The size of gaps
(greater than zero) between sorted variable values
makes no difference. In other words the influence
of a case with an explanatory variable outlier
is quite limited. 

For a classification problem, the proximity matrix
results provide a way to identify cases that have
atypical explanatory variable values relative to the
other cases in their class.  If the sum of square
proximities to the other cases of the same class
is small the case in not ending in the same leaf
as the other cases very often so values tend to 
be are different.

Extreme outliers  cases can warp the MDS layout
of cases. Outliers can force fairly different
cases to be plotted close together. The squares
of a big distance can dominate the sum of squares
of many moderate distances.    
 
2.2  The influence or impact of dependent variable outlier

We already have noted that taking the .2 power of the
dependent variable improves the fit.  The gaps between
the values do matter, but I suspect not as much as in 
linear regression.

The impact of outliers in the dependent variables can
be thought of in terms of how residuals are treated
in model fitting and assessment.  

In RFs, when an outlier case is oob it contributes
only to 1/3 of the trees when assessing the MSE
from all trees. 

When an outlier case is used in tree building it has
contributes to roughly 2/3ths of the trees.  
With minimum node size of 5, the mean has little
chance to fit the data well and the contribution
to the residual sum of squares will be very large. 
Transformations to reduce the gaps between exteme
values can help.  

3. Explanatory variable importance and selection 

As described in class, random forests
assess variable importance. We can
look at the list, and think about
simple models than may do almost as well.
 
## Run

imp <- importance(crimeRf)
imp
varImpPlot(crimeRf,cex=.8)

## End

In my perhaps biased view, variables at the top 
of the list are related to family cohesiveness.
  PctKidsBornNeverMar
  PctFam2Par
  FemalePctDiv
  PcrYoungKids2Par
  MalePctDiv

Race/ethnic origin are factors
  racePctWhite
  racePctblack
  racePctHisp

Poverty is likely a factor
  HousVacant factor
  PctPopUnderPov
  PctWubAsst
  PercentUnemployed

Population size is factor
  population
  pctUrban

Explanatory variables that have high
rank correlations can partially 
mask the importance of each other.
Scrambling the value of one variable
does matter as much when many nodes
in the tress are use highly correlated
values to define tree branches. The
variable partially act as surrogated
for each other.  

Many of the variables have pretty similar
variable importance. In the plot 
MedRentPctHousInc at the bottom of the
left column increase the mean squared error
by 11.6 percent which is non negligible.
There are similar to MSE increases for some other
other variables that did not make into the
plot.
 
When variables are removed from the model
the remaining variables will be more frequently
used in the trees and the model fit may not be reduced
at all. 

3.1 Reduced model 1

This try at variable reduction will use 
the variables that made the top 20 in at least
one of the columns labeled %IncMSE and IncNode Purity.
    
The script below finds the column subscripts needed
to put the two importance matrix columns
descending order and obtains the union of the 20 top
20 subscripts from for each column.  These become 
subscripts to the rownames (variable names) of the 
importance matrix and yields explanatory variable
name to use in the next model.
     
## Run
n <- 20

ord1 <- rev(order(imp[, 1]))
imp[ord1, ]

ord2 <- rev(order(imp[, 2]))
imp[ord2,]

commonSubs <- sort( union(ord1[1:n], ord2[1:n]) )
# same results below
# commonSubs <- sort(unique(c(ord1[1:n], ord2[1:n])))
varNam <- colnames(crimeReg)[commonSubs] 
varNam

# We can check spearman rho rank correlation.

checkCor <- round( cor(crimeReg[,varNam],
   method="spearman"),2)
checkCor

## End ____________________

As in linear regresssion assignment
We could make a list of highly correlated variables
and drop some these.

## Run______________________________

set.seed(4543)
crimeFocus1 <- randomForest(x = crimeReg[,varNam], y=crimeReg[,81],
          importance=TRUE,  proximity=FALSE, ntree=1000, keepForest=FALSE)

imp <- importance(crimeFocus1)
imp
varImpPlot(crimeFocus1,cex=.9)

## End_____________________________

Call:
 randomForest(x = crimeReg[, varNam], y = crimeReg[, 81], ntree = 1000,
      importance = TRUE, proximity = FALSE, keepForest = FALSE) 
               Type of random forest: regression
                     Number of trees: 1000
No. of variables tried at each split: 8

          Mean of squared residuals: 123288.7
                    % Var explained: 66.68
 
In terms of variance explained this model is very slightly better.
This started with the same random number seed as before.
The difference is due to having few variables to select from
at each node, and hence possibly have better variables used
when making splits at nodes.

The pairs plot below may be slow to appear.  It can be informative
at least on a  big screen.  The rows and columns for population,
OtherPerCap, and HousVacant have lots of white.  This indicates
these variables have at least one substantial outlier.

 ## Run
              
varDep <- colnames(crimeReg)[81]
varNamDep <- c(varNam,varDep )

windows()
pairs(crimeReg[, varNamDep],gap=0)

## End_____________________________

3.2  A seven variable model__________________________   

We will run a model with few variable to
see if the % of variance explained is almost as high.

Note that with seven variables the default number
of variables to try in which picking a split at node
is floor(7/3) = 2.  We could set the argument mtry
to the number we want rather than use the default.

## Run_______________________________

varNam2 <- c("racepctblack", "racePctWhite",       
   "pctWPubAsst",        
   "FemalePctDiv", 
   "PctKidsBornNeverMar",   
   "PctPersDenseHous",
   "PctUsePubTrans")     

set.seed(4543)
crimeFocus2 <- randomForest(x = crimeReg[, varNam2], y=crimeReg[,81],
          importance=TRUE, proximity=FALSE, ntree=1000, keepForest=FALSE)

crimeFocus2
varImpPlot(crimeFocus2)

## End_________________________________________________

Call:
 randomForest(x = crimeReg[, varNam2], y = crimeReg[, 81], ntree = 1000,
      importance = TRUE, proximity = FALSE, keepForest = FALSE) 
               Type of random forest: regression
                     Number of trees: 1000
No. of variables tried at each split: 2

          Mean of squared residuals: 131842.1
                    % Var explained: 64.37

The fit is not quite as good. 

3.3 Another try with seven variables

## Run

varNam3 <- c(
   "racePctWhite",            
   "FemalePctDiv", "PctKidsBornNeverMar",   
   "PctPersDenseHous","HousVacant",
   "PctUsePubTrans", "PctPopUnderPov") 

set.seed(4543)
crimeFocus3 <- randomForest(x = crimeReg[, varNam4], y=crimeReg[,81],
          importance=TRUE, proximity=FALSE, ntree=1000, keepForest=FALSE)

crimeFocus3
varImpPlot(crimeFocus2)

## End______________________________


 Mean of squared residuals: 127481.8
 % Var explained: 65.54

The fit in Section 3.1 with 25 variables in more
more difficult to present and discuss.  I had
66.03% of variance explained whick is only about
0.5% higher.  Changing the random number
seed can make a bigger difference than this.        

3.4  A six variable model

## Run

varNam4 <- c(
   "racePctWhite",            
   "FemalePctDiv", "PctKidsBornNeverMar",   
   "PctPersDenseHous",
   "PctUsePubTrans", "PctPopUnderPov") 
 
set.seed(4543)
crimeFocus4 <- randomForest(x = crimeReg[, varNam4], y=crimeReg[,81],
     mtry=2, importance=TRUE, proximity=FALSE, ntree=1000, keepForest=FALSE)

crimeFocus4
imp <- importance(crimeFocus4)
imp
varImpPlot(crimeFocus4)

## End____________________________________
  
The result is 
% Var explained: 64.36

Changing the random number seed to 37 yields
% Var explained: 64.63

3.5 Assignment: Find a different six variable model.

Try at least two models and pick the one with
a large percent of variance explained.

a)  Report the explanatory variables, the random
    number seed, and the % of variance explained.  

Using the same variables as in a) 
b) Change the seed report the seed % of variance
c) Change the mtry parameter to 3, the kinds of items in a)
d) Raise the power of the dependent variable to 0.2
    and report kinds if item in a)

3.6 A look at variable pairs

## Run:  

varNamPlus <- c(varNam4, colnames(crimeReg)[81])
panel.smooth1 <- function(...)panel.smooth(col.smooth="red",lwd=3,...)
pairs(crimeReg[, varNamPlus], panel=panel.smooth1,
  gap=0, las=1, pch=21, bg=rgb(0,.9,1), cex=1.1,
  main="Violent Crime and Explantory Variables")

## End

The smooth in the next to the last panel on the bottom right
suggests there is a lot of data in the lower left.  

We don't judge density well, even when we can see all the points.  
A matrix showing bivariate densities using contour plots, level plots,
hexagon bin plots, or perspective density surface view, would
help convey the data density.  

4. Case proximities, distance matrices
   multidimensional scaling, case clustering 
   and graphs.

With the forest of trees at hand
proximities result from running both
in-bag and oob cases down the trees.
The proximity for each pair of cases
is the number of trees in which they
appear together in a leaf node divided
by the number of trees.  

Proximities are numbers in the interval [0 1].
The proximity of a case to itself can be
defined as 1. 

We can transform a proximity matrix into
a distance matrix.  Previously when we had
a correlation matrix called cor we used
1-abs(cor) as a distance matrix.  Similarly 
we can use 1- proximity_matrix as a
distance matrix.

4.1 Multidimensional scaling

With a case distance matrix can use
cmdscale() to obtain plotting
coordinates in a chosen dimension
for cases.  One dimensional coordinates
are useful for reordering case so 
cases similar to each other cases are
close to each other.
  
Most multi-dimensional scaling plots
use 2 coordinates and some use 3 coordinates.
The mds coordinates are calculated so that
the distance between point pairs approximates
the distance between cases. The approximation
gets better as we use more coordinates.
 
When the number of coordinate matches the
rank of the distance matrix. Distance between
point should be a perfect match

4.2 Clustering comment

We can use the distance matrix to cluster the
cases. The hclust() function does like large
distance matrices so we don't use it here.
We will use kmean() further below  

4.3 Side note: Graphs and Fiedler projection  

In Stat 763 we will likely create graphs
whose vertices represent cases and whose
edges connect pairs of cases who proximities
are above a chosen threshold.  This
graph can be converted into a graph Laplacian
which is a matrix, say L[i,j]. L[i,i] is the number
edges connecting directly to case i. Off diagonal
elements L[i,j] are -1 if cases i and j share an
edge and 0 otherwise. The sum of each row is 0.
The Fiedler projection uses selected
eigenvectors from the spectral decomposition
of the scaled graph Laplacian to produce
a plot.       

4.4 Proximity calculation examples

The proximity calculation can take time
and produces a large matrix.  For our
1901 case example it is a 1901 x 1901 matrix.
Below we revisit the varNam4 model,
specify the calculation of the proximity
matrix and reduce the number of trees to
500 to speed the calculation.  

Running the script below reveals, that over
93 percent of the off diagonal proximities
matrix are zero.  These pairs of cases
never appeared together in leaf nodes.
 
For regression the default the minimal size
of terminal nodes leaf nodes is 5.  Increasing
this size give cases more opportunities to 
appear together in left nodes. A second
example set the minimum size to 20.   

We will look at multidimensional scaling
results using both. 

I am a little surprise that the scaling
seems to work.  I don't know how big a
matrix R can handle.  Very large
matrices can lead to storage
and numerical accuracy issues.
The accuracy issues are also partly 
related to the matrix content.  

## Run

set.seed(4543)
crimeFocus4a <- randomForest(x = crimeReg[, varNam4],
  y=crimeReg[,81], importance=TRUE,localImp=TRUE,
  proximity=TRUE, ntree=500, keepForest=FALSE)
crimeFocus4a   # % Var explained 64.22            

head(crimeFocus4a$importance)
tmp <- head(crimeFocus4a$localImportance)

prox <- crimeFocus4a$proximity
caseN <- dim(prox)[1] # 1901 x 1901
pct0Node5 <- 100*sum(prox==0)/(caseN*(caseN-1))
pct0Node5  # 93.50%

set.seed(4543)
crimeFocus4b <- randomForest(x = crimeReg[, varNam4],
  y=crimeReg[,81], importance=TRUE, localImp=TRUE,
  nodesize=20,
  proximity=TRUE, ntree=500, keepForest=FALSE)

crimeFocus4b  # $ Var explained 64.48

prox <- crimeFocus4b$proximity
pct0Node20 <- 100*sum(prox>0)/(caseN*(caseN-1))
pct0Node20  # 28.5 

## End__________________________

4.5 Proximity-based case distance plots

# Run: Graphics minimum node size = 5

proximityDist <- 1-crimeFocus4a$proximity
cases3dNode5 <- cmdscale(proximityDist,k=3)
head(cases3dNode5)

windows()
rval <- range(cases3dNode5[,1:2])
plot(cases3dNode5[,1],cases3dNode5[,2],pty="s",
     xlim=rval,ylim=rval,
     main="Communities: Node Size = 5, Proximity MDS")

windows()
rval <- range(cases3dNode5[,2:3])
plot(cases3dNode5[,2],cases3dNode5[,3],pty="s",
     xlim=rval,ylim=rval,
     main="Communities: Node Size = 5, Proximity MDS")

library(rgl)

open3d()
aspect3d(x=c(1,1,1))
bg3d(color=c("white","black"))
par3d(FOV=3)
plot3d(cases3dNode5,radius=.004,col="red",type='s',
      main="Minimum Node Size = 5")
## End____________________________________________

Left click and drag a corner of the rgl plot to resize it.
Left click in the plot and drag to rotate the plot.
If you want right click in the plot and drag to zoom
   in or out
After finding a view you like make a png file using 
the line below.

## Run
rgl.snapshot("CrimeProx5.png")
## End____________________________________________

The amount of structure in the 3D plot is amazing.  
The 3D plot clarifies structure present in the two 2D plots.  
A quick verbal description of the structure, perhaps
triggered by seeing the next 3D plot, is that it looks
like three triangles with the middle triangle sharing
two edges.

## Run 

proximityDist <- 1-crimeFocus4b$proximity
cases3dNode20 <- cmdscale(proximityDist,k=3)
head(cases3dNode20)


## Run:  Graphics minimum node size = 20
open3d()
aspect3d(x=c(1,1,1))
bg3d(color=c("white","black"))
par3d(FOV=3)
plot3d(cases3dNode20,radius=.007,col="red",type='s',
      main="Minimum Node Size = 20")
## End

Enlarge the plot, rotate it and make a .png file. 

## Run
rgl.snapshot("CrimeProx20.png")
## End

The three plane structure leads me to
speculate that three sets of communities
are being modeled by different variables
whose key variation is two dimensional. 
Maybe the three sets different in 
population size or perhaps
in violent crime rates. 

Some more thinking and graphics 
may provide more clues about the
structure. 

One thought not pursued here is to use
kmeans() to cluster the data into three
sets and color the points in one
of the 3d plots above based on their
cluster membership. 

See the local variable importance
example below see the method
applied in that context.  

4.6 Comments on constructing explanatory variables

Dr. Sutton has much experience with tree models
including random forests.  He comments that
constructing explanatory variables is often
help is developing better models.   

When variables are correlated we can use
the first few principle coordinates computed
from the variables to produce new linearly 
independent variables to use instead of
the orginal variables.

Nonlinear transformation of explanatory
variables can lead to a better model. 
Sometimes a thoughtfully chosen ratio
(or product) makes sense and really 
improves the fit.

5. Local variable importance______________________________ 


The script producing the CrimeFocus4a and 4b models,
specified calculating both global variable importance
and local (individual community) variable importance.  
Below we access local variable importance matrix.

## Run

impDat4a <- t(crimeFocus4a$localImportance)
head(impDat4a)

## End

My understanding is that local importance for the
ith case and jth variable is the average value over the
trees in which the ith case was oob. I think
the value for each such tree  is squared error for
the ith case when the jth variable values were scramble
minus
the mean square error for all cases when the
jth variable values were not scrambled.  

The violent crime rate per 100,000 ranges 6.6 to
4877.06 some squared errors could be very large. 
Compute a differece can lead to negative values. 
In some cases scrambling the jth variable can lead
to a better fit.    

## End__________________________
 
Some variables are important for explaining
crime for the whole data set but some subsets of
communities other variable can be more important
for modeling the variability in crime rates.

Maps can help us think geospatially about the
crime dynamics. The dynamics can be different in
different parts of a city, parts of a state
or in different parts of the US. 

Graphics can help us think in variable space about
about in crime dynamics.  (Ideally we think in terms
of variables (attribute), space and time.)
  
First we will used kmeans() to cluster communities based on local
variable importance. Below I chose 3 clusters
hoping there might be some correspondence
between the clusters and the triangle sections in the
proximity distance 3d point plot.  

## Run

clus4a <- kmeans(impDat4a,3,nstart=25)
clus4a$centers
table(clus4a$cluster) # $clusters 
##

Note the clusa is list with components such
as $centers that has the cluster centroids
and $cluster with tells the cluster to which
the community belongs.

Rather than use local importance matrix
to obtain case distances and then cmdscale() to obtain
plotting coordinates, this time we will use a
singular value decomposition to approximate
local importance matrix. From this we will use
the first three left eigenvectors to make a 3-D
scatterplot the communities.  Also we use the cluster
memberships above to assign colors.

## Run
# impDat4a obtained above
head(impDat4a)
xyzCrime4a <- svd(impDat4a)$u[,1:3]
head(xyzCrime4a)

open3d()
aspect3d(x=c(1,1,1))
bg3d(color=c("white","black"))
par3d(FOV=3)
plot3d(xyzCrime4a,radius=.006,
    col=c("green","red","blue")[clus4a$cluster],
    type='s',
   main="Minimum Node Size = 5",
   xlab="x",ylab="y",zlab="z")

## End___________________________

The clusters are consistent with the plot.  We could have
specified more clusters.  We could use the colors in the
case distance maps and see what happens  


6.  Loose end: Predicted values

This are more than enough ideas and details for one assignment. 
However just a few more comments.

Given a random forest model object and a new data set it is
easy to obtain the predicted values. To see the R documentation
use 
?predict.randomForest   

Suppose we had used the crimeRate**.2 power transformation to get
a better model. Then we would apply the inverse transformation
to predicted values:  predictedCrimeRate**5.  




   


