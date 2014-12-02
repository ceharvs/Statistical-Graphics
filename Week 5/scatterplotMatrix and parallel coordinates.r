# Assignment:  Scatterplot matrix 
#          
# Topics       
# 1. Using pairs
# 2. Modifying a function that is passed as an argument
# 3. Learning from help file examples
# 4. Using a formula and subsetting
# 5. Using the splom function from the lattice library
# 6. Using the parallel function from the lattice library 
# 
# Due  6 points total
#
# Revise the plot in 3.3 (2 points) 
#   Follow some of the suggestions
#   at the end of Section 2.
#   Be sure to put the units of measure
#   in variable labels. 
#   In homework document add a Figure
#   caption with the data location, time period.
#   Cleveland's book as a reference
#   and his reference. 
#    
# 3.4 2(points)
# 5.1 or 5.2 your choice (1 point)
# 6.  (1 point)
#_______________________________________

# 0. Setup

source("scatMatPanelFunctions.r")
library(ash)
library(lattice)

# 1.  Scatterplot matrix with smoothes using pairs
   
ozone <- read.table('ozone.txt',header=TRUE)
head(ozone)

pairs(ozone, panel=panel.smooth, gap=0, las=1, pch=21, bg=rgb(0,.8,1), 
  lwd=3, cex=1.1, main="Ozone Data")

# The above illustrates: 
# 
# Passing a function (here panel.smooth) 
# as a argument to the pairs() function,
#
# Setting the gap between panels to 0. 
# This increases the resolution of the plots!  It addressed Tufte's
# complaint visual illusion of little circles that appear at gaps
# between the corners of the plots. The motivation for the
# gaps is to avoid over plotting of tick mark labels. In this case
# alternating ticks marks and lable from side to side avoids
# the problem.  The cost is having to look from side to side 
# to read the tick labels.    
#       
# Using horizontal labels for all tick labels (las=1)
# 
# Setting the plotting symbol to a circle with an
# outline and a fill color.
#
# Making the smooth line have thicker line width using lwd=3.
 
# The above plot has a flaw.  An unintended consequence
# of using lwd=3 argument to increase the smoothing line thickness
# is that it also increased the thickness of the axis
# lines associated with tick marks. 
# 
# We can look at the pairs documentation.  
# 
# ?pairs
# 
# Note that there is no explicit lwd argument. Rather, 
# lwd=3 is being passed (along with any other unclaimed
# arguments) to a possible chain of functions via 
# the ...  general purpose argument. Hence lwd=3 finds
# it way the tickmark drawing function as well as to
# panel.smooth. 

# The documentation indicates that we can pass
# separate panel drawing functions using the lower.panel,
# upper.panel and diag.panel arguments.  We will do
# so further below 
  
# In the script language design there is merit
# in using the same keyword argument for similar purposes
# in different contexts.This avoids a proliferation of
# key words so decreasesmemory or look up burdens when
# in writing scripts. 
#
# In the ggplot syntax, we uses size to specify the size
# of the line width in the geom_line() and the size of dot in
# a geom_point() function. The separate specification
# for individual functions provides but the context for
# how size is used and the magnitude of size provide
# control
#
# This contrasts with  using cex for point size and lwd for
# line thickness in the base level R graphics and in
# the lattice package.
# 
# The ggplot examples suggest to address the above
# flaw. We set the argument directly in our own
# version of the function.   
      
# 2. Modifying a function that is passed as an argument.
# 
# Some R functions are written as an R script. These
# may be easy to modify.
# 
# panel.smooth
# 
# I copied the function from the console, gave it a new name
# and explicitly set the color and line width in the lines function
# near the bottom of the script. We can also change the span values
# which defaults to 2/3 in the function arguments.   
#
# If I set the span argument in pairs() it is passed through
# to myPanel.smooth and appears to work but produces
# many warning messages. Here we avoid the messages 
# by setting the arguments in the panel function. 

# 2.1 Modified panel function
   
myPanel.smooth <- function (x, y, col = par("col"), bg = NA,
  pch = par("pch"), cex = 1, col.smooth = "red", span = 2/3, iter = 3, ...) 
{
  points(x, y, pch = pch, col = col, bg = bg, cex = cex)
  ok <- is.finite(x) & is.finite(y)
  if (any(ok)) 
    lines(stats::lowess(x[ok], y[ok], f = span, iter = iter), 
    col = "purple", lwd=3, ...)
}

# 2.2 The test plot works but continue to seek 
#     plot improvements   

pairs(ozone, panel=myPanel.smooth, gap=0, las=1, pch=21, bg=rgb(0,.8,1),
  cex=1.1, main="Ozone Data")

# It works! So far so good. However we don't stop redesigning
# at the first improvement.  
#
# Where are the units of measure in the plot?
# What are the units of measure 
# Where was the data collected the location? 
# When was the data collected.  ?
# Are papers written about the data.
#
# Read about the data in Cleveland's
# book Visualizing data in Section 3.10
# on Bivariate Distributions. 
#
# We can consider finer details
# We could capitalize the first letter of the variable names.
# We might make the plots square.  
# We might put the all the tick mark labels on just two sides
#   of the matrix for easier scanning.  
# We might change colors and symbols size
# If points have names we could label some of the points
# ...

# 3. Using other panel functions
# 
# The examples near the bottom of the ?pairs documentation
# define panel functions that also appear in the
# R Graphics Cookbook. (You may latter observe that histogram
# doesn't alway get the range quite right. The histograme gets
# clipped on occation)  
# 
# The scatMatPanelFunction.r script file contains the panel
# functions a perspective density function I produced as an
# experiment. It should be possible to add density contours
# as illustrated in the bivariate density assign.  However
# the fill.coutour function won't work as is because of the
# separate treatment of a legend.  
# 
# I included the panel functions and
# one I made to show a perpective view and put these in the
# sourced file above, ScatMatPanelFunction.r.

# 3.1 Correlations and points with smoothes

pairs(ozone, lower.panel=panel.cor,
  diag.panel=panel.hist, upper.panel=myPanel.smooth, 
  gap=0, las=1, pch=21, bg=rgb(0,.8,1),
  cex=1.1 ,main="Ozone Data")

# The top row tells a story. The ozone high when the
# temperature is high and when the wind speed is low.
# We might not have seen this if we had put the dots
# and smoothes in the lower.panels and the correlation
# in the upper panels. Typically we want to smooth
# the values of the dependent variable and expect it
# to be on the y-axis.  

# I think it is a waste of space put the correlations
# by themselves in graphics panels. It is okay to know
# the correlations of ozone with temperature and 
# wind is high but this doesn't tell us about the 
# bends in the curves.

# In this example the context leads to little interest
# in point density, at least not at first. 
# Later we might fit a ozone surface using wind speed
# and temperature. We could find a contour based on 
# different ozone thresholds and note the number
# of values inside or outside of specific contours.
# This leans toward being a density question. 
#
# Still, our predictions of high ozone are likely
# to be based in part on very recent air pollution
# levels and forecasts of high temperature
# and low wind speed.

# Mostly the reason for showing the bivariate density
# surfaces is illustrate that we can make different 
# panel functions and use them in pairs.  

# 3.2 Perspective density surfaces

pairs(ozone, upper.panel=panel.persp,
  diag.panel=panel.hist, lower.panel=panel.persp, 
  gap=0, las=1, pch=21, bg=rgb(0,.8,1),
  cex=1.1 ,main="Ozone Data")

# 3.3 points in lower and upper panels 

pairs(ozone, panel=points,
      diag.panel=panel.hist, 
      gap=0, las=1, pch=21, bg=rgb(0,.8,1),
      labels=c("Ozone (ppb)","Radiation(lang)", "Temperature (deg F)", "Wind Speed (mph)"),
      cex=1.1 ,main="Ozone Data")

# It is not easy to think about how the densities
# in the perspective panel views in upper panels relate
# to the corresponding scatterplot panels in the
# lower panels.  The panels for the same two variables
# have their axes flipped around the line x = y. 
#
# It is easier to relate the corresponding panels
# in separate,complete scattplot matrix views as
# produced above. Of course the both need to be visible
# and of course it would be easier to study
# juxtaposed paris of plots.

# 3.4 Juxtaposed view
# 
# Consider just two variables x=temperature and
# y=ozone,  Use ggplot() and grid.arrange() from
# an earlier assignment to vertically juxtapose
# a bivariate density perspective plot and a
# scatterplot.  
#  
# The density scales are likely expanded a little
# due to smoothing. There may wll be perspective
# projection alignment issues. The project 
# may hide part of the density.
#
# Is there some rough agreement between the two plots?   
# The reason for showing density contours and 
# perspective views is that we are not very good
# a assessing point density and worse when there
# is lot of overplotting.

# 4. Learning from help documentation examples
# 
# E xamples appear near the end of help files and are 
# often instructive. The following scripts are copied
# from the pairs() documentation.

pairs(iris[1:4], main = "Anderson's Iris Data -- 3 species",
      pch = 21, bg = c("red", "green3", "blue")[unclass(iris$Species)])

# This classic data set was collected by Anderson and used by Fisher to
# formulate the linear discriminant analysis (LDA).  

head(iris)
levels(iris$Species)

# The purpose of discrimiant analysis is use measurements
# of sepal length and width, and petal length and width in order
# to discriminate between three species of iris: setosa, versicolor,
# and virginia. Today LDA remains a part of supervised
# classification methodology. Now there are many more options.     
# 
# We make some minor plots changes by removing the gap between plots,
# increasing the dot size and make the y-axis tick mark labels
# horizontal.

pairs(iris[1:4], main = "Anderson's Iris Data -- 3 species",
      pch = 21, bg = c("red", "green3", "blue")[unclass(iris$Species)],
      gap=0, cex=1.1, las=1)
  
# The bg argument provides three colors to used in filling the
# dots. The unclass() function when applied to a factor
# returns the integers that referred to the three levels
# of the Species factor. In the above context the integers
# become indices to the three colors of the bg vector.  
 
# 5. Using splom() the scatterplot matrix plot function
#    from the lattice package

splom(iris)        # scatterplot matrix function

# Note the presence of axis tick marks labels inside the
# diagonal plots and the sequence being split left to right
# and bottom to top. This is very clever design for saving space
# and focusing attention.  However this can be a little hard to
# follow. When the panels are small there can be
# bad overprinting of text.
   
?splom
       
# The script for the first example in example
# is below.    

# 5.1 A splom (scatterplot matrix) from the lattice library

super.sym <- trellis.par.get("superpose.symbol")
splom(~iris[1:4], groups = Species, data = iris,
  panel = panel.superpose,
  key = list(title = "Three Varieties of Iris",
  columns = 3, 
  points = list(pch = super.sym$pch[1:3],
  col = super.sym$col[1:3]),
  text = list(c("Setosa", "Versicolor", "Virginica"))))
 
# Above the first argument ~ is read as "is modeled by".
#  ~iris[1:4] gets the first four column of the data.frame
#  ~iris[,1:4] makes more sense to me. 
#  groups = Species is the 5th column in the data.frame
#    This categorical variable is the basis
#    for distinguishing the points with color when
#    plotted in superposed panels  

# In the second example give below                                   
#  |Species is read as "given Species" 
#  This time the Species are the basis for
#  juxtaposing panels.   

# 5.2 A conditioned Splom

splom(~iris[1:3]|Species, data = iris, 
  layout=c(2,2), pscales = 0,
  varnames = c("Sepal\nLength", "Sepal\nWidth", "Petal\nLength"),
  page = function(...) {
  ltext(x = seq(.6, .8, length.out = 4), 
  y = seq(.9, .6, length.out = 4), 
  labels = c("Three", "Varieties", "of", "Iris"),
  cex = 2)})

# Note the panels within panels view. Lattice is very 
# powerful! The syntax is takes time to learn. 
# 

#6. A conditioned parallel coordinate plot
#   using lattice conditioning
#
# Parallel coodinates plots provide an alernative
# the scatter plot matrices. 
# Below is another documentation example  

parallelplot(~iris[, 1:4] | Species, iris) 
                    
# The last argument is the data frame. 
# Think of plotting each  case's values for different variables
# as a tiny point on a parallel axis for each 
# the variable and then connecting the case's points by a line.
# 
# The line for different cases may be different colors. 
#
# Only the case points for adjacect axes are directly connect. 
# Variable 1 to variable 2, variable 2 to variable 3 and so on .
# The scatterplot matrix directly features all pairs of variables.  




