#                 Bivariate Density Estimates,
#                Hexagon Binning and Graphics           
#
# Notice: Graphics in Section 3 use windows()
#   rather than ??RStudioGD due to differences
#   in space allocation.  
# 
#   windows() can be replace by
#   quartz() on a Mac and likely 
#   X11() on a linux system

# Topics:       
#
# 1.  Sampling from the multivariate normal family
# 1.1 Arguments of mvrnorm()
# 1.2 Covariance matrix constraints,
#     matrix multiplication and construction
# 1.3 A degenerate bivariate normal example
# 1.4 Generating and combining 3 fixed size samples
# 1.5 Some scatterplots
# 1.6 Generating random sample sizes for a mixture

# 2.  Bivariate density estimates and graphics
# 2.1 Kernel density estimates using the ash package
# 2.2 Plotting points and contours
# 2.3 Perspective surface
# 2.4 Lighting model rendered surface
#     and dynamic rotation

# 3.  Hexagon binning and graphics
# 3.1 The underlying lattice for 
#     hexagon binning
# 3.2 Temporary hbin function and the 
#     default sequential scale graphics style
# 3.3 The lattice and centroid size styles.
# 3.4 The size nested in power of 10 color
#      style
# 3.5 Plotting counts in hexagon cells
# 3.6 Using  hexagon cell ids to compute
#     aggregate statistics for other variables  
     	
# Due: 10 plot: 10 points
# 1.5 plot 
# 2.2 Choose 1 
# 2.3 Second plot
# 2.4 plot   
# 2.5 plot
# 2.6 plot
# 3.2 plot and plot with xbins=40
# 3.4 your choice
# 3.5 plot

# References:     
# 
# Density estimates and ASH
#   David Scott, 1992. Multivariate Density Estimation
#   John Wiley and Sons
#               
# Quantizing
#  John H.Conway and Neil J. A. Sloane. 1993. Sphere Packings, 
#  Lattices and Groups. Second edition, Springer-Verlag, 1993.
# 
# Hexagon binning
#   Carr, D. B.  1991.  "Looking at Large Data Sets Using
#   Binned Data Plots."  Computing and Graphics in Statistics,
#   eds. A. Buja and P. Tukey, Springer-Verlag, New York,
#   pp. 7-39. 
# 
#   Carr et al. 1987. Scatterplot Matrix Techniques For Large N
#   Journal of the American Statistical Association.
#   82(398):424-436.
                 
# Comments:   
#
# Low dimensional binning with regular regions
# can help to address overplotting in scatterplots
# This often overlooked!
# Witness all the overplotted scatterplots! 

# 0. Setup

library(MASS)
library(ggplot2)
library(rgl)
library(ash)
library(hexbin)
source('hbinfunctions.r')

# 1. Sampling from the multivariate normal Family
# 
# Sometimes we sample from theoretical families
# of distributions 
# to compare analytic methods
# to see how methods work when know the answer 
# or to provide examples.
#  
# Here the goal is to show generation of 
# samples based on a mixture of multivariate
# normal distributions. We start by 
# sampling from a multivariate normal distribution
# with different means and covariance matrices.  

# 1.1 Arguments for mvrnorm()
#
# The univariate random normal function
# is rnorm() and arguments are the sample size,
# mean and standard deviation (key work is sd)
#
# The name for multivariate normal distribution
# has added the prefix mv.     
#
# Like rnorm() the first argument mvrnorm() is the
# sample size n which to defaults to 1.  
# 
# The second argument specifies a vector of means
# and the key word mu.The vector length determines the
# the number of variables. 
# 
# The third argument specifies the covariance matrix.
# The key word is Sigma. If there are 3 variables
# this should be a 3 x 3 matrix with variances down
# the diagonal and covariances for pairs of variables off
# the diagonal.  

# A fourth argument with default is empirical=FALSE.
# If set to TRUE R transforms the random
# sample so the sample mean vector and covariance
# matrix match the mvrnorm mean vector and
# covariance matrix. There is still some variation left
# in the sample. Using the default is more common. 

n <- 100
means<- c(2,4)
Cov <- matrix(c(1,-.4,-.4,2),nrow=2) 
Cov 

set.seed(37)
dat <- mvrnorm(n, mu = means, Sigma=Cov)
x <- dat[,1]
y <- dat[,2]
qplot(x, y, 
main="Bivariate Normal Distribution\nWith Negative Covariance")

# 1.2 Covariance matrix constraints,
#     matrix multiplication and construction
# 
# It is trivial to specify a vector of means.  However
# a covariance matrix has to be a symmetric, positive
# definite matrix so it is possible to specify
# an invalid covariance matrix.  
# 
# Symmetric:  
#   Let Cov be a p x p matrix and t() be the 
#   transpose operator.  If t(Cov) = Cov
#   the matrix is symmetric 

Cov
t(Cov)
all.equal(Cov,t(Cov))

# Positive Definite: If the determinant of
# symmetric matrix is positive then it is
# positive definite.

det(Cov) 
det(Cov) > 0 

# A related result is that if v is
# any non-zero numeric column vector of
# length p and Cov a symmetric p x p
# matrix, then have the quadratic form 
# t(v) %*% Cov %*% v is 0 the matrix
# is not positive definite. 
#
# If det(Cov) = 0 the data might
# still have a normal distribution
# but in a lower dimensions.

# 1.3 A degenerate bivariate normal example

x <- rnorm(100)
y <- -x        # The points are on a line in 2D.  
dat <- cbind(x,y)
qplot(x, y)

cor(dat)    # A perfect negative correlation

colMeans(dat)
Cov <- var(dat)
Cov

# Not two dimensional
det(Cov)

# Not two dimensional 
v <- c(1, 1)
t(v) %*% Cov %*% v 

# With matrix multiplication we
# can construct a covariance matrix
# from matrix with standard deviations
# on diagonal and a correlation matrix.

sd <- c(2,3)
sdDiagMat <- diag(sd)
sdDiagMat

Cor <- matrix(c(1, .23, .23, 1), nrow=2)
Cor

Cov <- sdDiagMat %*% Cor %*% sdDiagMat
Cov

det(Cov)  # positive

# 1.4 Generating and combining
#     three fixed size samples

set.seed(137)
# Set 1
n1 <- 1000         # number of cases
m1 <- c(-2, 2)      # means
sd1 <- c(.5, 2)     # standard deviations       
cor1 <- matrix(c(1, .3, .3, 1), ncol=2) # correlation matrix
cov1 <- diag(sd1) %*% cor1 %*% diag(sd1) # covariance
set1 <- mvrnorm(n1, m=m1, Sigma=cov1)

# Set 2
n2 <- 2000
m2 <- c(2, 2)
sd2 <- c(2.5, 1)
cor2 <- matrix(c(1, -.25, -.25, 1), ncol=2)
cov2 <- diag(sd2) %*% cor2 %*% diag(sd2) 
set2 <- mvrnorm(n2, m=m2, Sigma=cov2)

# Set 3
n3 <- 3000
m3 <- c(0, -2)
sd3 <- c(2.5, 1)
cor3 <- matrix(c(1, 0, 0, 1), ncol=2)
cov3 <- diag(sd3) %*% cor3 %*% diag(sd3)
set3 <- mvrnorm(n3, m=m3, Sigma=cov3)

# Combine the data row-wise
DAT <- rbind( set1, set2, set3 )

# 1.5 Some scatterplots

# Plot open dots
qplot(DAT[, 1], DAT[, 2], shape=I(1),size=I(2),
  xlab="X values simulated", ylab="Y values simulated", 
  main="An Overplotted Scatterplot")

# Plot filled dots
qplot(DAT[, 1], DAT[, 2], shape=I(19),size=I(2),
  xlab="X values simulated", ylab="Y values simulated", 
  main="More overplotting with filled dots")

# Here is experiment that tries to convey
# bit more of the data density by overplotting
# and alpha blending dots of decreasing size.
# This is a primitive emulation of  splatting gaussians.
# In computer graphics environment this could
# be implemented with texture mapping. 

ggplot(df,aes(x=x,y=y))+
  geom_point(size=3.5,color="blue",alpha=.1)+
  geom_point(size=3.0,color="blue",alpha=.1)+
  geom_point(size=2.5,color="blue",alpha=.1)+
  geom_point(size=2.0,color="blue",alpha=.1)+
  geom_point(size=1.5,alpha=.1)

# If you experiment a bit changing sizes, colors, or alpha levels
# you may well be able to produce a better looking result.

# When looking at scatterplot we may be interested either
# functional relationships or density features and sometimes
# both. Another assignment addresses graphics showing functional
# relationships.  Here we move on to density estimates,
# contours and surfaces.  Then we return to hexagon
# binning that provides another way to address overplotting 

# 2. Bivariate density estimates and graphical representation.
#
# Kernel density estimtes often provide the foundation for
# graphical representations of data density.  
# 
# 2.1 Kernel density estimates using the ash package 
#
# The average shifted bivariate histogram (ash) approach
# developed by David Scott provides very fast kernel
# density estimates. 
#
# The bin2() arguments are the bivariate data and
# the grid resolution for binning into  rectangles.
bin2d <- bin2(DAT, nbin=c(30, 30))
head(bin2d)

# The ash2() arguments are the result of bin2() and specification of
# how many times to shift the grid when averaging histograms to produce
# the smoothed density estimates. For this we accept the defaults.  

bin2d.sm <- ash2(bin2d)
head(bin2d.sm)

# The bin2d.sm object has list structure that contour function 
# can use as an input argument 

contour(bin2d.sm, nlevels=12, xlab="X Values Simulated: Unitless",
ylab="Y Values Simulated: Unitless", main="Mixture of Correlated Normals")

# In contour() above, nlevels suggests the number of contour levels.
# An option is to specify levels in a vector. Then R ignores the 
# nlevels argument.

# 2.2 Plotting points and contours 
# 
# Text printed on a varied background can be hard to read. 
# It is not too bad if there is singnifica tgray level
# contrast between background and foreground is high.
#
# The first example below uses a light gray, gray(.7)
# for points. This not not contrast were much with
# the white background.
# 
# For the contour lines and text we try full
# intensity blue, rgb(0, 0, 1)
#    
# Remember that rough rgb gray scale equivalents 
# are .3, .6, and .1.
# A full intensity blue is pretty dark.
#   0*.3 + 0*.6 + 1*.1 = .1
# This  provides a reasonable contrast
# to the .7 lightnes point. 
# A full intensity red is still pretty dark
#   1*.3 + 0*.6 + 0*.1 = .3 
# We reduce the red intensity to .5
# to provide darker lines and text. 
# Even darker red looks brown.   

# The  add=T argument in the contour()
# adds the contour to the previous plot.
plot(DAT, col=gray(.75))
contour(bin2d.sm, nlevels=12, xlab="X With No Units",
  ylab="Y With No Units", main="Mixture of Correlated Normals",
  add=T, col=rgb(0,0,1),cex=2,lwd=2,las=1)

plot(DAT, col=gray(.75))
contour(bin2d.sm, nlevels=12, xlab="X With No Units",
  ylab="Y With No Units", main="Mixture of Correlated Normals",
 add=T, col=rgb(.5,0,0),cex=2,lwd=2,las=1)

# 2.3  Plotting filled contours
#
# The following uses a default number of levels and
# colors

filled.contour(bin2d.sm,
  xlab="X Values Simulated: Unitless",
  ylab="Y Values Simulated: Unitless",
  main="Average Shift Histogram Bivariate Density",
  key.title=title("Key\nDensity") )  

# Modifying the contour levels and color ramp palette 
#
# The colorRampPalette() function will accept set of 
# colors and interpolate to provide a longer sequence
# of colors. The first argument is a set of sequential colors
# from Cindy Brewer that I like. Note there are 5 input colors
# and the result as 8 colors to fill in the areas between the 
# 9 levels specified.

YlOrBr <- c("#FFFFD4", "#FED98E", "#FE9929", "#D95F0E", "#993404") 

filled.contour(bin2d.sm, color = colorRampPalette(YlOrBr,
  space = "Lab", bias = 0.8),
  xlab = "X Values Simulated: Unitless",
  ylab = "Y Values Simulated: Unitless",
  levels = seq(0, max(bin2d.sm$z), length=9),
  main = "Average Shift Histogram Bivariate Density",
  key.title = title("Key\nDensity") )  # \n uses a second line

# 2.4 Plotting a perspective view of the density surface
# 
# Note that the light comes from the left 
# Different orientations of the density surface would be helpful
# Some software puts a contour plots beneath a density surface plot.

persp(bin2d.sm, xlab='X: No Units', ylab="Y: No Units",
  zlab="Density", main=" A Single Perspective View Can Hide Features",
  col="lightblue", r=4, phi=40, shade=.2)

# 2.5 A lighting model rendered density surface
#     and dynamic rotation using rgl
# 
# The ash2() function produced the list object we
# called bin2d.sm. The two-way grid of estimated densities
# is in a matrix labeled z in the list.  The x and y coordinates
# for the matrix are vectors labeled x and y. Some graphics functions
# such as persp() can used this list as input.
#
# For the rgl example below we access the x, y and z
# components by explicit name and multiple the density
# matrix by 400 to increase the resolution of  density
# magnitudes. 

names(bin2d.sm)
is.matrix(bin2d.sm$z)

library(rgl)
open3d()
par3d(FOV=0) 
# aspect3d(x=c(1,1,1))
rx <- range(bin2d.sm$x)
ry <- range(bin2d.sm$y)
rz <- range(400*bin2d.sm$z)
decorate3d(rx,ry,rz,aspect=FALSE,
          xlab='xSim',ylab='ySim',zlab='Density')
surface3d(bin2d.sm$x,
          bin2d.sm$y,
          400*bin2d.sm$z, col="#4080FF")
names(bin2d.sm)

# The plot appears in an rgl window
# and not in the R Studio Window
#
# The window may be hidden. Once found 
# you can drag on the window corners
# to enlarge the window.  Left mouse 
# in the window and move the mouse 
# to rotate the plot. Right mouse in the
# window and move it up or down
# zoom in and out.  

# The snapshot3d() function will
# save the plot in a .png file
# for later inclusion in a document.

snapshot3d("myfile.png")

# 2.6 Changing the ash binning grid
# 
# The grid resolution when binning and the grid
# shifting details for averaging histogram densities 
# provide two smoothing parameters.  
# 
# See what happens when using a grid resolution 
# of 40 x 40 rather than 30 x 30.

bin2d <- bin2(DAT, nbin=c(40, 40))
bin2d.sm <- ash2(bin2d)

filled.contour(bin2d.sm, color = colorRampPalette(YlOrBr,
  space = "Lab", bias = 0.8),
  xlab = "X Values Simulated: Unitless",
  ylab = "Y Values Simulated: Unitless",
  levels = seq(0, max(bin2d.sm$z),length=9),
  main = "Average Shift Histogram Bivariate Density\n Higher Resolution",
  key.title = title("Key\nDensity") )  # \n uses a second line
  
# The smaller grid cells bring out more local detail.  The local mode in
# in the upper part of the previous contour now appears to 
# have two local modes. We remember there a mixture of 
# three distributions.  

# 3.  Hexagon binning and graphics
# 
# We may choose to look directly at bin counts or densities. 
# We can do about everything with binned summaries and weights
# that can be done with the original data. Binning can reduce
# storage requirements and speed computation. For example loess
# can run faster using cell centers and weights. The cost is
# loss of higher resolution details.        
#
# Other have established quantizing and stistical the merits
# of hexagon binning in 2-D and advocate truncated octahedron
# binning in 3-D. I have added my opinions about  perceptual
# merits. In term of perception, the cells shapes
# are artifacts of the construction process. Our vision is
# particularly sensitive to horizontal and vertical lines so
# hen viewing square bins oriented horizontally and vertically
# our vision emphasizes the artifacts. This sensitivity detracts
# from seeing other patterns.  Hexagons lie on three sets of
# lines through the lattice points. Truncated octahedrons lie
# 7 sets of lines through lattice points. This and rounder shape
# mitigates the impact of construction artifacts.
#
# Note that the hexagon binning function hbin and graphics used
# in this assignment are a bit different the the binning and graphics
# function in the lattice package.  hexbin function
# and some of the graphics in the hexbin package.  I still plan
# modify my small modification of the graphics to be compatible
# when hexbin() which was based on my older work.  

# 3.1 The underlying lattice for 
#     hexagon binning 
#
# Shifted rectangular lattice points with the right 
# spacing have hexagons as near-neighbor regions

x <- -2:2
sq <- expand.grid(list(x=x, y=x))
fc.sq <- rbind(sq, sq+.5)  	      # face centered squares
fc.sq$y <- sqrt(3)*fc.sq$y   	      # stretch y by the sqrt(3)

windows(width=5, height=sqrt(3)*5)   # also stretch the graphsheet
par(mai=c(.5, .5, .5, .5))
plot(fc.sq$x, fc.sq$y, pch=15, cex=.8, las=1, col="red")
mtext(side=3, line=1.3,
  "Two Shifted Rectangular Lattices and Hexagons Regions", cex=.95)
mtext(side=3, line=.3,
  "Black Dot Is In Two Rectangles But Closer to the Red Center", cex=.95)

nr <- length(fc.sq$x)/2
gblue <- rgb(0,.5,1)
points(fc.sq$x[1:nr], fc.sq$y[1:nr], pch=15, cex=.8, col=gblue)

# stretched squares
px <- c(-1, -2, -2, -1)
py <- sqrt(3)*c(0, 0, -1, -1)
polygon(px, py, density=0, col="#505050")
polygon(px+2, py+sqrt(3), density=0, col="#505050")
polygon(px+.5, py-sqrt(3)/2, density=0, col="#505050")
polygon(px+2.5, py+sqrt(3)/2, density=0, col="#505050")

# near neighbor regions
px <- c(-.5, -.5, 0, .5, .5, 0, NA)
py <- c(-.5, .5, 1, .5, -.5, -1, NA)/sqrt(3)
polygon(px+1, py, col=gblue, density=0)
polygon(px+.5, py+sqrt(3)/2, lwd=3, col="red", density=0)

points(.7, .6, cex=2, pch=19, col='black')

# Fast hexagon binning comments
# 
# Scales the data into lattice coordinates. 
# Squashes y by the sqrt(3) to make 
#     two square lattices.
# For each data point finds closed point of each lattice
# Pick the closest one. 
# For the select lattice point accumulates
# 1) the count
# 2) mean of x's
# 3) mean of y's  
 
# 3.2 Using hbin() and related graphics fuctions
#
# The functions were sourced in the Setup section.
#  
# Below
# xbins gives the number of hexagons bins across the x axis.
# Increasing the number makes the hexagons smaller 

ans <- hbin(DAT[, 1], DAT[, 2], xbins=25)
plot(ans)
# We can see the three cluster

head(ans)
# cell:  
#   The id along with booking information,
#   supports calculation of cell centers.
# count: 
#  The number of points falling in the cell.
# xMean and yMean
#   The average of x and y coordinates 
#   for points falling in the cell.
#   Typically different than the cell center.

class(ans)
# ans is an object of class hbin as well
# as of class data.frame.  
# There hbin function includes is plot function
# for this class
# Hence plot(ans) will produce a plot.  

# Find the cells with the 10 largest counts
ord <- rev(order(ans$count))
tmp <- ans[ord, ]
tmp[1:10, ]

# Be;pw the number of color classes is only a suggestion
windows()
plot(ans, nclass=5, main="Hexagon Binning")

# 3.3 The lattice and centroid size styles
#
# The "lattice" style centers the 
#   hexagon symbols in the hexagon cell.
#
# The "centroid" style moves the symbol
#   closer to the center of mass in the cell
#   but stops when necessary to keep the symbol
#   completely inside the cell.
#   The big symbols cannot move very far.

windows(width=10, height=9)
plot(ans, style="lattice")

windows(width=10, height=9)
plot(ans, style="centroid", col.one="#0080FF", border="#505050")

# 3.4 The size nested color style for
#     for powers of 10 counts
#
# The background color shows the power of 10. 
# The size within the color indicates the leading digit.  
# This appears a little busy.  Possibly some refinements such as
# less contrast could improve the appearance. 
#
# Still this design does show a little of the gradient within each power of ten. 
# 
# Dynamic tools that exploit GPU capabilities and allow investigation
# of densities that ranging over orders of magnitude would be desirable. 

n <- 400000
x <- rnorm(n)
y <- rnorm(n)
bigger <- hbin(x, y, xbins=30)

windows(width=9, height=7)
plot(bigger, style="nested.lat", main="Hexagon Nested Encoding")

windows(width=9, height=7)
plot(bigger, style="nested.cen", main="Hexagon Nested Encoding", border=NA)

# 3.5 Plotting counts in hexagon cells

set.seed(37)
x <- runif(2000)
y <- rweibull(2000, shape=1.5)
bin <- hbin(x,y)

windows()
par(pty='s')
plot(attributes(bin)$xlim, attributes(bin)$ylim,
 type='n',
 xlab="Uniform Distribution on 0 to 1",
 ylab="Weibull: Shape Parameter 1.5",
main="Two Thousand Simulated Values")
hbin.draw (bin, density=0)   # This add hexagons to a plot
#hbin.xy compute the lattice centers
text(hbin.xy(bin), labels=bin$count, adj=.5, cex=.65)

# 3.6 Using hexagon cell ids to compute
#     aggregate statistics for other variables   
#
# In the context of z=f(x,y) and perhaps w= f(x,y) 
# hexagon cell ids can be used as a factor in tapply or aggregate
# to compute statistics for z and w statistics such as
# the means for cases in in each of the hexagon bins.
# 
# Figure 5.5 in Carr 1991 provides example. After the Chernobyl 
# disaster 2000 Cesium-137 and Iodina-131 concentrations were reported
# for locations in the northern US and Canada.  Plot construction
# averaged the concentrations in hexagon regions covering the map.
# The encoding used the two ray directions in each hexagon to encode
# the averages. Ray points pointing down meant 0 parts
# per billion. Ray pointing up represent 37.5 and 75 part per billion
# respectively.  The cesium ray went up on the left and the iodine ray
# went up on the right.  Commonly occurring downward points rays
# were likely background. Upward pointing rays were suggestive of fall
# out from the atmospheric contamination circling the earth. Bivariate
# anomalies were relatively easy to find. 
# 
# To take the next step we could define what we mean by anomalies (or fallout
# suggestive values).  Then we could  highlight such encoding distinct colors
# for rays or thicker rays or both. Colin Ware says encoding that
# pop out are the encoding that we can tune our vision to notice. 
# We can easily choose to spot red lines in a field of black lines.    
# Stephen Kosslyn indicates we can easily spot lines (rays) that are twice
# as thick as other lines (rays).  Color and size are separable encodings.
# WE can used these to encode two different variables.
#
# Sometimes we don't have the expertise to feel comfortable defining
# exactly what we mean when we refer to anomalies. Sometimes the boundary 
# between noise and noise plus signal is not very clear. Maybe we should
# be bold and take the next steps so the audience sees more and thinks more. 
