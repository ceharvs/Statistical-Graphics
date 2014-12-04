File        Principal Components   
By          Daniel B. Carr

Emphasis    Centering, Rotation, and may be reflection

Sections   1. Generating trivariate normal samples
           2. A 3D data scatterplot with translucent
              ellipoid as a density reference
           3. Producing principal components  
           4. A 3-D principal components plot
              with a reference ellipesoid

           5. Results returned by prcomp()
           6. A computational check using
              matrix multiplication 
           7. Principal component interpretation
              in terms of rotation coefficients 
           8. Picking the number principal components to use

           9.  2-D scatterplots 
          10.  A heptathlon example
          10.1 Use identify() to name an outlier
          10.2 Transforming data before input to
               principal components
          10.3 Obtaining principal components
          10.4 Assess the variation in the principal components
          10.5 Relate the first principal component
               to the Judges score

          11. Notes related function arguments
              that are functions  

Due       Plots from 2, 4, 8, 9-first plot, 10.1, 10.5 

Uses:     Packages:  rgl, (MASS, comes with R) 

1. Generating trivariate normal distribution

First we specify the mean vector, Mean, 
and the covariance matrix, Cov. 

Then we use the multivariate random normal
distribution sample generator from the MASS
library that comes with R.

## Run_______________

library(MASS)

Mean <- c(4,-2, 5)
Mean

Cov <- matrix(c(3,3.5,0,3.5,10,0,0,0,1), 3,3)
Cov

set.seed(37)
xyz <- mvrnorm(1000, Mean, Cov)
round(var(xyz),2)
round(Cov - var(xyz),2)

## End_________________________

2. A 3-D data scatterplot with translucent
   ellipoid as a density reference.   

Next we look a 3D scatterplot. To help with
visualizing the cloud of points we will use a
translucent ellipsoid that contains roughly 90
percent of the observations. 

This is reasonable because the sample is from a
normal distribution. Otherwise we could estimate
the 3D point density, interpolate densities on 3-D
grid. The contours of constant density are surfaces
in 3-D. They are sometimes called alpha contours
when alpha fraction of the maximum density.  

Historically a marching cubes algorithm
produced 3-D polygons for surface rendering.  

Note 1:  I am not aware that the desired 3-D contour polygon
generation capability is available in R.  This is more than
22 decades after David Scott's instructive examples in Multivariate
Density Estimation: Theory, Practice, and Visualization (Aug 1992). 
His average shifted histogram approach readily produces density
estimates on a 3-D grid.  I eagerly await his second edition
to be published this and am anticipating 4-D examples.      
 
Note 2:  A constant density shell may be disjoint. 

Note 3: We can't see through many translucent shell surfaces
but David Scott showed that removing slices though the shells
can help us see part of what is inside. 


When the RGL Device appears  
Left click on a corner and drag to enlarge the plot   
Left click in the plot and drag to rotate the contents
Right click in the plot and drag up or down to zoom out or in

## Run______________________

library(rgl)

open3d()
plot3d(xyz, box=TRUE,
 xlab="x", ylab="y", zlab="z")
aspect3d("iso")
xzyMean <- colMeans(xyz)
xCov <- var(xyz)
plot3d( ellipse3d(Cov,centre=Mean, level=.9),
  col="green", alpha=0.5, add = TRUE)

## End_____________________

After modifying the RGL device view make a .png snapshot.

## Run

snapshot3d("Data and 90% Ellipsoid.png" )

## End_____________________

Observe that: 

1) The data is NOT centered at the origin.
2) The y-axis has the largest range, NOT the x axis.
3) Since the covariance matrix is not a diagonal
   matrix, the ellipsoid axis are not aligned the 
   the coordinate axes.

Below the one line principal component function
prcomp() below generates three new variables called
principal components. These new variables are
1) Centered at the origin
2) Rotated so the covariance matrix is diagonal
3) Rotated so 
   The first variable has the largest variance
   The second variable has the next largest variance
   as so on.  
4) There may be reflections

Prcomp() passes the data to scale() function to
center the data.  

If we set the scale argument
in prcomp() to TRUE this is passed to the scale argument
in the scale() function. The variables will then be divided
by their standard deviations after centering and the
principal components rotation makes correlation matrix diagonal.   

3. Producing principal components 

We assume are generated variables are in the
same units so chose not to scale the variables. 
 
## Run_____________

pcList <- prcomp(xyz)

## End_____________

4. A 3-D principal components plot
   with a reference ellipsoid.

The principal components are in the 
$x component of the pcList above.

## Run_____________________

pc <- pcList$x
pcMeans <- colMeans(pc)
round(pcMeans,2)

pcCov <- var(pc)
round(pcCov,2)

open3d()
plot3d(pc, box=TRUE,
xlab="PC1",ylab="PC2",zlab="PC3")
aspect3d("iso")
plot3d( ellipse3d(pcCov, centre=pcMeans, level=.9),
  col="cyan", alpha=0.5, add = TRUE)

## End_____________________

After changing the view make a snapshot,  

## Run

snapshot3d("Principle components and 90% Ellipsoid.png" )

## End_____________________


5. Results returned by prcomp()

pcList$center 
  Contains the means of the input variables
  that were subtracted to center the data

pcList$rotation
  Is the rotation used to rotate the
  case points about the origin.

pcList$sdev has standard deviations of the
  principal components.  

pcList$scale is FALSE in this case 
  since the default uses the 
  covariance matrix. 

pcList$x contains the principal
  components as indicated above. 

6. A computation check using
   matrix multiplication 

The computation uses the singular value
decomposition function, svd(), to produce
the rotation matrix. Since we did not
scale the data, svd is based on the
data covariance matrix.  
The covariance for the n cases by 3 variables
data set is a 3 x 3 matrix.  The rotation
matrix is a 3 x 3 matrix.  

We can post multiply the n x 3 centered variable
data matrix by the 3 x 3 rotation matrix to obtain the 
n x 3 principal components matrix and compare this
against the results of prcomp().   

## Run____________________

rot <- pcList$rot
rot
det(rot)

xyzCentered <- scale(xyz,center=,scale=FALSE)
head(xyzCentered)

pcCheck<- xyzCentered %*% rot

all.equal(pc, pcCheck)

## End______________________

7. Principal component interpretation
   in terms of rotation coefficients 

The scaling and the linear combination
of variables that make up principal components
provide the basis for interpreting 
principal components.   

When the variables are scaled, the coefficients
of each linear combination are comparable as 
standard deviation units.  We describe them
without reference to units of measure such
as degrees centigrade.

We did not scale our generated variables. 
They don't have units of measure.  We can
think of the coefficients as weights for 
variables that have difference variances.
    
The rotation matrix for our example is:

# round(rot,2)

       PC1   PC2   PC3
[1,] -0.36  0.93 -0.05
[2,] -0.93 -0.36  0.00
[3,] -0.01  0.04  1.00

PC1 <- -.36*Xcen - .93*Ycen-.01*Zcen

# round(var(xyz),1)

     [,1] [,2] [,3]
[1,]  3.0  3.5  0.1
[2,]  3.5 10.5  0.1
[3,]  0.1  0.1  1.0
> 

The -.93 weights the Ycen variable most 
since it has the largest variance of 10.5 as
shown abpve.  The -.36 weights the Xcen variable
since it variance of 3.0 is moderate in size.
the -0.1 weight the Zcen variable little since
is variance is only 1. 

As a reminder on the computation of a
variance for a linear combination con
just the Xcen and Ycen part of the
first principal component   

var( a*Xcen+ b*Ycen ) = a^2*Var(Xcen) + 
              2*a*b*cov(Xcen,Ycen) +
                b^2*var(Ycen)

The first principal component has
the largest variance of the principal
components.  Plugging in the numbers
below we can see that even the
covariance term contributes to making
the variance large. We could change
the sign on all the coefficients and
get the same variance.   

var( -.36Xcen -.93Ycen ) = (-.37)^2*3.0 + 
              2*(-.37)*(-.93)*3.5 +
                (-.93)^2*10.5

When a nice pattern appears in the coefficients
they can used to provide a rough interpretation
of patterns that appear in principal component
plots.

For example in a 5D case a principle
component might have weights close to

(small,  .82, small, -.41, -.41)

This could be interpreted as contrast between the
2nd variable and the average of the 
4th and 5th variables.  (A contrast is a 
set of weights that sum to 0.)  

Of course interpretion depends on 
understanding the variables and the
scientific context.

8. Picking the number principal components to use

The number of principal components produced
is limited to the number of linearly  independent
variables in the data set.  This is typically
the same as the rank of the data's covariance or
correlation  matrix. If there are fewer cases
than variables, the number of cases is an upper
bound the rank.     

We typically show the largest principal components.
The hope is that the first few principal components
mostly represent the true structure and that
the last principal components mostly represent noise
or negligible structure.   

The more principal components we represent the higher
the faction of data variabilty that we show. Due to
principal component selection order the amount of
additional variability  represented decreases with
the addition of the next principal component.  One
stopping heuristic is to stop when incremental increase
in variance include gets small. The hope is that
the first few principal components mostly represent
the dominant true structure and that the last principal
components are capturing noise.  

## Run_______________

screeplot(pcList, las=1)

## End________________

While we may select many principal component for 
subsequent modeling, the number chosen to shown using
graphics is often very small.  Often we see a scatterplot
with just two principal components.  

With more principal components we can produce
scatterplot matrices or parallel coordinate plots.  
Scatterplot matrices for three principal components
are not that uncommon.  Of course rotation and
stereo graphics can show 3D points as can rgl.  
However, flatland style remains is popular.     

For maps regions, such as earth grid cells, a principal
component can determine color.  I remember seeing as
many as six univariate maps each representing a
differet principal component.  These were interpreted
based on high magnitude rotation coefficients and contrasts
and the corresponding variable names.   

I have also seen three principal components used to
control the red, green and blue intensities for grid cells. 
Such maps show a lot of variation. However color is an
integral encoding so we ccan't rip apart the a color
into values for three principle components
and interpret this in terms of the orginal variables. 

9.  2-D scatterplots 

When we make 2-D scatterplots with pairs
of principal components we might want to 
make the units per inch the same for the axes.  

Otherwise we have to read the scale and imagine
the different variances. 
   
## Run:  Quick and dirty window scaling__

tmp <- apply(xPc,2,range)
tmp

diff(tmp)
## 22.6 8.1 (6.4)

## Quick and dirty via window scaling 
winX = 7.5

windows(w=winX,h=winX*8.1/22.6)
plot(pc[,1],pc[,2],las=T,
main="Maybe Interesting")

windows()
plot(pc[,1],pc[,2],las=T,
main="Not Very Interesting")

## End

10. A heptathlon example

This one principal component example is interesting because
the linear combination matches the judges score amazinglu
well.   

I adapted this from "A Handbook of Statistical Analysis
Using R" by Everitt and Hothorn. 

## Run

heptathlon = read.csv(file="heptathlon.csv",row.names=1)
heptathlon

# remove the officialscore
hepDat = heptathlon[,-ncol(heptathlon)]

## End 
  
The values for hurdles, run200m and run800m are in seconds
The values are highjump, shot, long jump and javelin are meters

We can look the data with a scatterplot matri  
      
## Run

myPanelSmooth = function(x,y,...)panel.smooth(x,y,col.smooth='red',...)
pairs(hepDat,panel=myPanelSmooth,lwd=3,pch=21,cex=1.5,
     col=rgb(0,.6,1),bg=rgb(0,.6,1),gap=0,las=1)

## End

Note the outlier along the top of the bottom row.  
One athlete has a long hurdle time, a low high jump, a short long jump,
a pretty long 200 meter run time and long time for the 800 meter run
as compared to the other Olympic athletes.   
 
10.1 Use identify() to name an outlier
 
Using identify() and left click in the window by the upper  right 
point to find the outlier name. You and identify a few point for 
fun.  Then right click in the plot and select stop to proceed.   

## Run
plot(hepDat[c(1,7)],pch=16,col='blue',cex=1.2,las=1)
identify(heptathlon[,1],heptathlon[,7],lab=row.names(heptathlon))
# End

10.2 Transforming data before input to principal components 

A good way to simplify intepretation is make all the
positive outcomes have larger values.  Hence the following
script transforms the timed outcomes so the fast times (currently 
small values) become large values.  The transformation below subtracts
the person's time from the longest time.  The person with the shorted
time will have the largest value.
     
## Run

hepDat$hurdles=max(hepDat$hurdles)-hepDat$hurdles
hepDat$run200m=max(hepDat$run200m)-hepDat$run200m
hepDat$run800m=max(hepDat$run800m)-hepDat$run800m

##End

10.3  Obtaining principal components

## Run   

heptPca = prcomp(hepDat,scale=TRUE)   # variables are in different units

heptPca$center
heptPca$scale  # standard deviation of the variables
heptPca$x      # principal components
heptPca$sdev   # the standand deviation of the principal components
round(heptPca$rotation,2)

## End

Look at the rotation columns.  A graphic represention could help see
patterns.  In the first column the magnitude  are roughly the same
except the small value for javelin.  In the second column the javelin
value, -.84, sticks out as having a large magnitude.  
Throwing the javelin has physical requirements are less
compatitible with the training for other events.   

10.4 Assess the variation in the principal components

## Run 
names(heptPca$sdev) = paste('Comp',1:length(heptPca$sdev),sep='')
plot(heptPca)

heptVar = heptPca$sdev**2
100*cumsum(heptVar)/sum(heptVar)
## End

The first principal component accounts for about 64% of the variation.

10.5  Relate the first principal component to the Judges score

## Run

plot(heptathlon$score,-heptPca$x[,1],las=1,pch=21,bg="red",
     xlab="Official Score",ylab="First Principal Component",
     main="Comparing Scores and First Principal Component")
correl = cor(-heptPca$x[,1],heptathlon$score)
xloc = mean(par()$usr[1:2])
text(xloc,4,paste("Correlation =",round(correl,2)),adj=.5,cex=1.1)


## End

Note the above multipled the first principal components by -1
to make the correlation positive
-1 does not change this. 

Note the high agreement of the first principal component as a score and the
Official Score.  This is interesting. Of course omitting one person and the
choice of variable transformations may have had something to do with the
close match.  Still it makes one wonder if the the Official Scoring systems
emerged from the application of principal components to old data sets or if
human subject judgement evolved to weights that maximized variability.  


## End 

11. Notes related function arguments
    that are functions  
The pairs() function has five arguments that
are functions: 
panel = points
upper.panel=panel
lower.panel=panel
diag.panel=NULL
text.panel=textPanel
 
In section 10 above I wanted to replace panel argument points
panel.smooth. However I want to change the color of the
smooth line.  The ... argument in allows value to be 
passed to functions specified as argument. There
was a conflict passing a color argument. Hence I defined
a function my.panel.smooth(that directly specified the
color I wanted and used this.  
 


