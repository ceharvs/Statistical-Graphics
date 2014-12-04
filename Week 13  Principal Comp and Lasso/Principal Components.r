
           Principal Components: Centering and Rotation

The details differ based on the statistical context

Plot a random sample and an ellipsoid of concentration
corresponding to a 90% probability region for a
trivariate normal distribution with given mean 
positive definite covariance matrix 

Sections   1. Generating trivariate normal samples

           2. A 3D data scatterplot with translucent
              ellipoid as a density reference

           3. Producing principal components  

           4. A 3d principal components plot
              with a reference ellipesoid.

           5. Results returned by prcomp()

           6. A computation check using
              matrix multiplication 

           7. Principal component interpretation
              in terms of rotation coefficients 

           8. Picking the number principal components to use

           9. Notes: rank deficient covariance matrices
              and principal component sign changes

          10. 2D scatterplots 

Uses:       Packages:  rgl, (MASS, comes with R) 

1. Generating trivariate normal distribution

First we specify the mean vector, Mean, and
the covariance matrix, Cov. Then we use the
multivariate random normal distribution
sample generator from the MASS library that
comes with R.

## Run_______________

library(MASS)

Mean <- c(4,-2, 5)
Mean

Cov <- matrix(c(3,3.5,0,3.5,10,0,0,0,1), 3,3)
Cov

x <- mvrnorm(1000, Mean, Cov)

## End_________________________

2. A 3D data scatterplot with translucent
   ellipoid as a density reference.   

Next we look a 3D scatterplot. To help with
visualizing the cloud of points we will use a
translucent ellipsiod that contains roughly 90
percent of the observations. 

This is reasonable because the sample is from a
normal distribution. Otherwise we could estimated
the 3D point density, interpolate densities a 3D
grid and the compute and show on 3D surface
for one or more given density levels.  
 
## Run______________________

library(rgl)

open3d()
plot3d(x, box=TRUE)
aspect3d("iso")
xMean <- colMeans(x)
xCov <- var(x)
plot3d( ellipse3d(Cov,centre=Mean, level=.9),
  col="green", alpha=0.5, add = TRUE,
  xlab="x",ylab="y",zlab="z")

## End_____________________

Notes:

1) The data is NOT centered at the origin.
2) The y-axis has the largest range, NOT the x axis.
3) Since the covariance matrix is not a diagonal
   matrix, the long axis of the ellipsoid is not
   aligned with the y-axis. 

The one line principal component function
prcomp() below generates new variables called
principal components. These new variables are
1) Centered at the origin
2) The centered variables are rotated so
   the covariance matrix is diagonal, and
3) The first variable has the largest variance
   The second variable has the second
      largest variance
   Subsequent variables are descending
      variance order.

Note the if we had had set scale arguement to TRUE in
prcomp() this would have passed along to the scale()
function prcomp() uses and 2) above was say the
rotation makes the correlation diagonal.

3. Producing principal components 

We assume are generated variables are in the
same units so chose not to scale the variables. 
 
## Run_____________

xList <- prcomp(x)

## End_____________

4. A 3d principal components plot
   with a reference ellipsoid.

The principal components are in the 
$x component of the list xList above.

## Run_____________________

xPc <- xList$x
xPcMean <- colMeans(xPc)
xPcCov <- var(xPc)
open3d()
plot3d(xPc, box=TRUE)
aspect3d("iso")
plot3d( ellipse3d(xPcCov, centre=xPcMean, level=.9),
  col="cyan", alpha=0.5, add = TRUE)

## End_____________________

5. Results returned by prcomp()

xList$center 
  Contains the means of the input variables
  that were subtracted to center the data

xList$rotation
  Is the rotation used to rotate the
  cases about the origin.

xList$sdev has standard deviations of the
  principal components.  

xList$scale is FALSE in this case 
  since the default uses the 
  covariance matrix. 

xList$x contains the principal
  components as indicated above. 

6. A computation check using
   matrix multiplication 

The computation uses the singular value
decomposition function, svd(), to produce
the rotation matrix. Since we did not
scale the data, svd is based on the
data covariance matrix.  
The covariance for the n cases  x 3 variables
data set is a 3 x 3 matrix.  The rotation
matrix is a 3 x 3 matrix.  

We can post multiply the n x 3 centered variable
data matrix by the 3 x 3 rotation matrix to obtain the 
n x 3 principal components matrix and compare this
against the results of prcomp().   

## Run____________________

xList$center

xList$rot

xCentered <- scale(x,center=xList$center,
                   scale=FALSE)
head(xCentered)
xPCcheck <- xCentered %*% xList$rot

all.equal(xList$x, xPCcheck)

## End______________________

7. Principal component interpretation
   in terms of rotation coefficients 

The scaling and the linear combination
of variables the make up a principal components
provide the basis for interpreting a 
principal component.   

When the variables are scaled the coefficients
of each linear combination are comparable as 
standard deviation units.  we describe them
without reference to units of measure such
as degrees centigrade.

We did not scale our generated variables. 
They don't have units of measure.  We can
thing of the coefficients as weights for 
variables that have difference variances.
    
The rotation matrix for our example is: 

              PC1        PC2         PC3
[1,] -0.3687345672 -0.9245182  0.09644127
[2,] -0.9295343411  0.3668396 -0.03734421
[3,]  0.0008530756  0.1034156  0.99463787

Look at the coefficients in the PC1 column. This
gives the linear combination of the centered 
original variables that make up first principal
component. Roughly 

PC1 <- -.35*X1cen - .93*X2cen + .00*X3cen.

The coefficients for each linear combination are
called loadings in some the principal component
literature.

The second original variable had biggest variance
so not surprisingly its coefficient (-.93) has the
largest magnitude when computing first principal
component. The first original variable with 
coefficient, -.35. also contributes.   
contributes to the first principal component. 
The third original variable with coefficient
(.00) contributes very little.   
 
Remember that 

var( aX+ bY ) = a^2*Var(X) + 
              2*a*b*cov(X,Y) +
                b^2*var(Y)

Since coefficients a and b above are both negative and the covariance
of first two variables is positive (3.5) it makes sense that
that PC1 will have a large variance.  

When nice patterns appear in the coefficients
they can used to provide a rough interpretation
of patterns that appear principal component
plots.

For example in a 5D case a principle
component might have weights close to

(small,  .82, small, -.41, -.41)

This could be interpreted as contrast between the
2nd variable and the average of the 
4th and 5th variables.  

Of course interpretion depends on 
understanding the variables and the
scientific context.

8. Picking the number principal components to use

We typically the largest principal components.
Often there appears a point of diminishing returns.
This can provide a heuristic to pick the number of
components to choose in the absence of other consideration.
Remember, the goal is in part dimension reduce and part
the hope of retaining variaton that is not mostly
noise with respect to the task at hand.   

The screeplot provide one a view the variable
variances.  Another graphic might show the increase
percent of total variance retained.  

## Run_______________

screeplot(xList, las=1)

## End________________

9. Notes: rank deficient covariance matrices
   and principal component sign changes.    

The number of principal components produce
is limited to the rank the data's covariance 
matrix. (The correlation matrix has the
same rank.)   

We could multiple any two principal components
vectors by -1 and they would still be valid
principal components.  This effectively 
multiplies the two columns of the rotation matrix.
This matrix should still have determinant 1. 
   
The reason to mentions this is that two different
principal component algorithms may have corresponding
vectors that differ in sign and both can be correct.

10.  2D scatterplots 

People often make 2D scatterplot with pairs
of principal components.  It is the context
makes them interesting.  

Here we expect to see points from a bivariate
normal distribution with 0 correlation.   
We might change the aspect ratio of the plot
to respect the units and hence convey
the very different variances visually rather
than by relying on the x and y axis
units to tell the different variance story. 
   

## Run____________________________

windows()
plot(xPc[,1],xPc[,2],las=T,
main="Not Very Interesting")

windows()
plot(xPc[,1],xPc[,3],las=T,
main="Not Very Interesting")

windows()
plot(xPc[,2],xPc[,3],las=T,
main="Not Very Interesting")

## End________________________

 

