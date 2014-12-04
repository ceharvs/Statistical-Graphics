File     randomForests
By       Quotes from
         http://www.stat.berkeley.edu/
             ~breiman/RandomForests/cc_home.htm 
         My parts are adapted from the randomForest documentation 
         The casement display in 8 my work, 

         Posted lecturs materials now replace notes from Section 1.           

Section  1. Comments on random forests
         2. The classic Iris data
         3. Build a random forest
            and see clasification error rates 
         3.1  A 2D multidimesional scaling plot
         4. Variable importance
         5. Prototype case for each species and a scatterplot
         6. Predictor outlier measures for cases based on proximity   
         7. Voting margins for cases
         8. Showing 4 dimensional prediction regions
            with multivariate graphics
         9. Imputing missing data - just mentions functions
        10. Selected random forest arguments
        11. Partial dependency plots
  

Setup   Use the install options under the package menu
        to install the randomForests 

Due     Plots from 2,3.1, 4.3, 5, 6, 7, 8, 11        

Note    The assigment emphasis is just on classfication problems
        Random forests can do much more     

1. Comments on frandom forests


Random forest models are being used by large companies with lots
money on the line.  While there are some warnings see (Wikipedia)
about overfitting, random forests seem to be a good choice
include in your data exploration and analysis tool kit.  

In terms of supervised classification it has been my understanding
that are two of the preferred out-of-the box methods for modeling
and predicting are support vector machines and random forests.
Support vector machine may require more tuning.   

Years ago I was interested in Jerry Friedman's Rulefit methdology
that used random forest trees to create indicator functions that
were included along with other predictors in a linear regression
model.  The actual fitting used lasso regression that penalized
the magnitudes of coefficients and shrinks many to zero.  
This provides a way to address variable selection in the presence
of very many variables. I have not kept up in terms of knowing
about Rulefit availability and advances. 

Lasso appears as topic in your Introduction to Statistical Learning
text and the recommend text, R for Everyone. It appears frequently
in the context of very high dimension analysis.  I plan to provide
a lasso assignment later in class. Now on the iris data
and random forest graphics.    
 
2. The classic Iris data

We will be using the Iris data to build a supervised classification model

The iris data has: 
  4 continuous predictors:  
    Sepal length and width - millimeters
    Petal length and width - millimeters

  1 categorical dependent variable: 
    species: setosa, versicolor, virginica    

## Run
# install.package(randomForest)
library(randomForest)
data(iris)

# look at 5 random cases from each species
subs = c(sample(1:50,5),sample(51:100,5),sample(101:150,5,))
iris[subs,]

# plot all the data 
species = as.numeric(iris$Species)  # stored as a factor        
irisColor = c(rgb(0,.5,0),rgb(0,.65,1),"red") # one color per species 
caseColor = irisColor[species]      # one color per case

windows(width=8,height=8.5)
pairs(iris[,1:4],gap=0,pch=21,cex=1.2,las=1,
          col=caseColor, # outline
          bg=caseColor,
          labels=c("Sepal Length\n (cm)",
                   "Sepal Width\n  (cm)",
                   "Petal Length\n (cm)",
                   "Pepal Width\n (cm)"),
          main=c(
         "Iris Data:  Green=Setosa,  Turquoise=Versicolor,
         Red=Virginica")
)  # fill
## End

In the pairs function above note the use of \n to include
an extra line that provides the  units of measure.  

If you want the diagonal go from lower left to upper right
you can add the argument: row1attop=FALSE.  
  
Isolating the setosa dots (green) with orthogonal cuts is trivial.  
only one cut is needed.  Separating the versicolor and virginica irises
look pretty easy except for a few cases.  


3. Build a random forest______________________________________

## Run

#  500 trees created by default

set.seed(123)
irisRf = randomForest(x=iris[,-5],y=iris[,5],
                     keepForest=TRUE, proximity=TRUE)

irisRf  # A confusion matrix shows the error rates 
## End 


Look in the R Console window.  The default setting tries only two variable
at each split.  The best variables, petal length and petal width may not
have been tried in some trees or tried late in the paritioning.  
The voting gives the better trees a chance to dominate.    
 
he confusion matrix table in the R Console window
shows the classification errors counts and rates.


3.1 A 2D multidimesional scaling plot 


## Run  
windows(width=8,height=8.5)

MDSplot(irisRf,fac=iris$Species,k=2,palette=irisColor)

## End 

The graphics window shows multidimensional scaling
of the case dissimilarity matrix.  This is constructed from the 
case proximities (similarities) that have been converted
to dissimilarities.  

A 150 x 150 dissimilarity matrix could be full rank. In this case
the singular value decomposition may have to produce many plotting
coordinates (left eigenvectors) so the distances between the points
(the rows provide point coordinates) are good approximation to the
dissimilarity matrix.   

Here the plotted point value pairs are the rows of just 2 eigenvectors, 
but they have a story to tell.    

reduction to 2D can be 
to be a poor approximation.  If the structure is pretty close to 
to being 2 dimension, the 2D MDS plots can work pretty well.

On the left most of the setosa cases (red) are perfectly overplotted. 
They end up in the same level not in all the trees.  A few do not end
in the same leaf node.  For some tree the sepal width and length are
the two variables considered in splitting cases in the root node. This
do not perfectly separate all the setosa cases so some end up in in
different leave notes. Hence we see few more red dots.   

The curve on the right suggests a lot of structure. The positions
of a point or two of one color in a region dominated by another color
such that commonly occurred in leaf nodes with cases of the other species.      

## Run
getTree(irisRf,k=1,labelVar=FALSE)  # 1st tree
getTree(irisRf,k=100,labelVar=FALSE)# 100th tree
## End

Look labels and the first line of the output in the R Console. 
We see which variable was split and the split threshold.  
The status of 1 and -1 distinguishes split nodes from terminal nodes
On the left we see line numbers where the left and right daughters
are addressed.  


4.  Variable Use Counts and Importance__________________________

4.1 Counts of tree branches using the variables

Note that Petal length and width are used more often.  The
random variable selection gives other variables a chance
to get used. 

##Run
cnt = varUsed(irisRf)
names(cnt) = colnames(iris[,-5])
cnt
##End

4.2 Variable Importance   

Remember from 1.3 that out of bag cases are used
in assessing importance.

If a variable is important and it oob variable values
are permuted for the oob cases, using the tree for prediction
we would expect declines in tree prediction accuracy and
and declines in branching split purity based
on the variable.  
   
Tree prediction accuracy decline is assessed by  
Classification:  Increase in percent of misclassifications
Regression:      Increase in squared residuals

Note:  The declines for each tree are averaged and normalized by the
       standard error. (If the standard error is 0, the
       normalizatiom does not occur) 

Variable branch splitting purity decline is assessed by 
Classification:  Gini Index  
Regression:      Residual sum of squares

## Run      
set.seed(4543)
irisTempRf = randomForest(iris[,-5],iris[,5],ntree=1000,
                              keep.forest=FALSE,importance=TRUE)
importance(irisTempRf)
##End


4.3  Variable Importance Dot Plot

## Run
varImpPlot(irisTempRf)
## Run

5.  Prototype case for each species and a scatterplot_______________

The procedure for the classCenter function below

For each class
    Pick the case that has most of its nNBR
         nearest neighbors from it own class
    Compute the median for numeric variables
         of the own class neighbor cases 
    Compute the most categorical variables use the most
         frequent

For a second protoype, repeat using nNMr closed
    neighbors not previously used.  

## Run    
irisP = classCenter(iris[,-5],iris[,5],irisRf$prox)
gPlot(iris[,3],iris[,4],pch=21,xlab="Petal Length (cm)",
      ylab="Petal Width(cm)",
      bg=irisColor[species],main="Iris Data with Prototypes",cex=1.1
      ) 
points(irisP[,3],irisP[,4],pch=21,cex=2,bg=irisColor)

## End  

6. Predictor outliers for cases________________________________

Remember from 1.1 that proximity is based on count pairs
in leaf nodes

Here the outlier measures are in a numeric vector with one value per case.  
The outlier measure for a case is computed as n / sum(squared proximity),
normalized by subtracting the median and divided by the MAD,
within each class.

##Run
plot(outlier(irisRf),type="h", col=caseColor,lwd=2,las=1)
##End

7. Voting margins for cases____________________________________

For random forest margin methods for classification are not like regression

For EACH case the margin is
   the proportion of votes for the correct class MINUS
   the highest proportion of votes among the wrong classes  

When the difference is positive, majority rule predicts the right class.
 
## Run 
set.seed(1)
data(iris)
windows(width=8,height=8.5)
x = seq(along=iris$Species)
y= margin(irisRf,iris$Species)
gPlot(x,y,main="Random Forest Margin Plot for Iris Data",
pch=21,bg=caseColor)
identify(x,y)
## End

Use identify left clicks to label the 6 lowest points and 
right click to access the stop option.

8. Showing Low dimensional prediction regions with multivariate graphics

There are 4 continuous predictor variables.
We can observe prediction regions using 4D graphics + species color
4D graphics options include
   parallel coordinate plots
   scatterplot matrices
   casement display
   stereo or rotating ray glyphs
   other glyphs 

Issues include
   overplotting
   selecting variables to emphasize
   distinguishing data from non-data domains

Current choice
   Overplotting: Casement Display
   Resolution Emphasis: Petal Length and Width 
   Real data domain highlighting:  Not done 

## Run
  
# get ranges for predictors
irisMin=apply(iris[,1:4],2,min)
irisMax=apply(iris[,1:4],2,max)
irisR = irisMax-irisMin

# Select resolution of points accros  the range
#    Petal length and width are the imporant variables
#    Give them more resolution

gridSl = seq(irisMin[1],irisMax[1],len=5)
gridSw = seq(irisMin[2],irisMax[2],len=5)
gridPl = seq(irisMin[3],irisMax[3],len=10)
gridPw = seq(irisMin[4],irisMax[4],len=10)

# Generate predictor matrix and predict

grid4D = expand.grid(list(sl=gridSl,sw=gridSw,pl=gridPl,pw=gridPw))
mat4D = as.matrix(grid4D)
colnames(mat4D) = names(iris)[1:4]

irisPredict = predict(irisRf,mat4D)
predictCaseColor= irisColor[as.numeric(irisPredict)]

# Construct casement display plotting coordinates
#    nesting Sepal Length in Petal Length
#       Scale range of centered sepal length to
#             range of petal length/12.5
#   Handle width similarly 


incX = scale(grid4D$sl,scale=12.5*irisR[1]/irisR[3])
incY = scale(grid4D$sw,scale=12.5*irisR[2]/irisR[4])
xNew = mat4D[,3]+incX
yNew = mat4D[,4]+incY

xNewR = range(xNew)
xNewR =    1.045*(xNewR-mean(xNewR))+mean(xNewR)
yNewR = range(yNew)
yNewR =    1.045*(yNewR-mean(yNewR))+mean(yNewR)

windows(width=8,height=8.6)

plot(xNewR,yNewR,type='n',
xaxs='i',yaxs='i',las=1,
xlab="Petal Length refined by Sepal Length",
ylab="Petal Width refined by Sepal Width",
main="4D Prediction Domains for Three Iris Species")

tmp = par()$usr
rect(tmp[1],tmp[3],tmp[2],tmp[4],col="#A0A0A0")
points(xNew,yNew,pch=22,col="#B0B0B0",
          bg=predictCaseColor,cex=2.1)
mtext(side=3,line=.3,"Setosa=Green, Versicolor=Turquoise, Virginica=Red")

## End

To understand the encoding consider an example.  

Look at the middle mostly turquoise 4 x 4 matrix of the large
squares surrounded by thick gray lines. The bottom 3 rows of large
squares correspond to smaller petal widths and are classified
as Versicolor. In the top row has some red cells indicating the
Virginica class.   The square large squares on the left 
have two small red square corresponding to smallest class
for sepal length and the smallest two classes for sepal width.

The values used for the prediction at the centers respective squares,
the large square for petal length and width and the small square
for sepal length and width. 

There are many possible applications of nested plots.   

The design give the most resolution (a 10 x 10 matrix of values)
to the two most important variabls and ( a 5 x 5 matrix for values)
for the two least important variables.
      

9. Imputing missing values in data.frames___________________________

This is not discussed here but is of potential use
See 
na.roughfix()
rfImpute


10. Selected Random Forest arguments________________________________

ntree:    The number of trees to grow

mtry:     The number of variables sampled as candidates at each split
          classification default: sqrt(p)
          regression default:  p/3
sampsize: Size() of sample to draw.
          Classication:  if a vector of length # of classes
                         size for the respective classes  

nperm:   The number of permutations in assessing predictor importance

   
11. Partial Dependency Plots________________________________________

Cases with value in an interval for a predictor variable
are assigned to classes based on voting.  The log of the
class voting fraction for a particular class can be compared
against the average log of the class vote fractions for all
classes. When the difference of the two values
is greater than zero, a predictor variable value being
in the interval is partially supporting the class assignment.

Below the mid-range of Petal.Width is supportive of the
versicolor assignment. 

## Run
set.seed(345)
partialPlot(irisRf,iris,Petal.Width, "versicolor")

##End
