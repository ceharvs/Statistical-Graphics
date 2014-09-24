# Singer height dot and distribution plots 			    
# By Daniel B. Carr
# Version: 9/19/2014
 
# Data       
# The lattice package has the singer height
# data used in Cleveland book Visualizing Data. 
           
# R topics     
# ggplot()
# tapply() for computation,
# rlm()  for robust linear model fitting
# facet_wrap() for multiple subplots  
#                     
# Graphics  
#   histograms and density plots,
#   box plots
#   quantile plots and cumulative distribution plots
#   Q-Q plots
#     Variable versus normal
#     Variable versus variable
#     MD plot: Mean and Difference transformation
#     Subset residuals versus composite residuals 
#     reference lines 
   
# Sections:  
# 0. Setup
# 1. Dot plots for voice parts
# 2. Voice.part facet-wrapped histogram
#    and density plots
# 3. Box plots
# 
# 4.  Wrapped quantile and cumulative probability plots
# 4.1 Uses tapply() to construct vectors
#     to put in a data.frame
# 4.2 Produces the quantile and cumulative
#     probability plots
# 4.3 Discussion of quantile
#     labels and cumulative probability
#     options
# 4.4 Produce normal Q_Q plots
# 
# 5. Q-Q plots comparing two data vectors
# 5.1 A single Q-Q plot
# 5.2 The corresponding MD plot (mean and difference)
# 5.3 A Q-Q plot of soprano1 residuals versus
#     pooled residuals
# 
# Due 6 plot 6 points
#
# Section and plot 8 points
# 2.  Histogram plot
# 3.  Row labeled box plots  
# 4.2 The facet_wrap quantile plot  
#     
# 5.1 Q-Q plot Soprano1 versus Bass 2
# 5.2 MD plot: Mean and Difference plot
# 5.3 Q-Q  Soprano 1 residuals versus pooled residuals 

0.  Setup

library(ggplot2)
library(lattice) 
library(MASS) # for robust regression rlm()

# Define a homework ggplot2 theme
# Change the theme if you wish

hwTheme2 <- theme_gray()+
  theme(
  strip.background=element_rect(fill=rgb(.9,.95,1),
    colour=gray(.5), size=.2), 
  panel.border=element_rect(fill=FALSE,colour=gray(.50)),
  axis.text=element_text(colour="black"),
  axis.ticks=element_blank()) # no ticks

# 1.  Dot plots for the singer height data

head(singer)
tail(singer)

# We start with qplot() function 
# which stands for quick plot. 
# This function has some arguments
# like the R base level plot() function
# some ggplot() features.  
#
# The qplot() syntax is simpler at times
# for example by not needing the aes()
# function. However, focus ggplot to 
# reduce the burden of syntax switching.  

qplot(data=singer,x=height,y=voice.part,
  main="Singer Data Dot Plot",
  xlab="Heights in Inches: Rounded to Integers?",
  ylab="Voice Part") + hwTheme2

# Are there ties?
with(singer,table(voice.part, height))

# Can we show tied values with dot plots? 
ggplot(singer, aes(x=voice.part, y=height))+
  geom_dotplot(binaxis="y", method="dotdensity",
     binwidth=.22, dotsize=.5, stackdir="up") + 
  labs(y="Height in inches: Likely Rounded",
      x="Voice Part",
      title="Singer Stacked Dot Plot")+
  coord_flip() + hwTheme2

# Yes, but the specification is a bit complex.
#
# The general specification patterns are
# not hard learn   
#
# We provide a data frame.  
#   aes() associates aesthetics such as 
#     x and y coordinates
#     symbol shape, size and color 
#     with data.frame variables or constant.  
#   geom_ functions name the geometric rendering 
#     arguments include
#       parameters for rendering and computation
#       a method for computing needed values
#         often there is default method and 
#         choices include prividing you own.  
#  layout functions
#    facet_wrap, facet=grid
#    coord_flip
#  labeling functions   
#    labs, ggtitle

# Focus attention on details of commonly used geoms such 
# as geom_point() and geom_line(). Access details
# for little used functions.    

?geom_dotplot

# Might specifying histograms be easier?

# 2. Facet_wrap histograms and density plots

# We can use facet_wrap() to produce a plot
# containing rows of subplots. Here we limit
# the subplots (sometimes call panels or facets)
# to two per row so the 8 voice.part plots
# appear in four rows of two columns each.

# Following a left to right then top to bottom reading
# convention it seems natural to put Soprano 1 and Soprano 2
# subplots in the first row. The order of the voice.part 
# factor levels Bass 2 first. will put Soprano 1 last because
# is the last level of voice part.  
#     
# Below we create our own copy of singer
# data.frame called singer2.  Then we create a factor
# called partOrder that reverses the levels to put
# Soprano 1 first. We used this factor for
# our fact-wrap plots. 
#
# Note that the row order of the data frame has not changed.
# Rather the partOrder levels and indices are different.  
# Occasionally the data frame row order is important such as
# when using geom_path().
  
singer2 <- singer
singer2$partOrder <- with(data=singer,
  factor(voice.part,levels=rev(levels(voice.part))))

ggplot(data=singer2, aes(x=height))+
  geom_histogram(binwidth=1,fill="green",color="black")+
  facet_wrap(~partOrder,ncol=2)+
  labs(x="Count",y="Height in Inches",
  title="Histograms") + hwTheme2

# We can also produce density plots

ggplot(data=singer2, aes(x=height))+
  geom_density(fill=rgb(0,.8,1),color="black")+
  facet_wrap(~partOrder,ncol=2)+hwTheme2

# Omitting the labels draws attention to the 
# fact that part specification burden in
# involves the important task of provide labels.

# 3. Box plots

# There are variations on box plot construction.
# The basic elements used here are indicated below.
# 
# Median: shown as a thick line (or longer line or dot)
# 1st and 3rd quartiles:  show as end of a box
# Interquartile range (IQR):  3rd-1st Quartile
# Upper fence:  3rd quartile + 1.5*IQR (not shown)
# Upper adjacent value: largest value <= Upper fence
# Upper outliers:  values > upper fence
# Lower element are analogous to upper elements

ggplot(data=singer, aes(x=voice.part,y=height)) +
  geom_boxplot(width=.6,col="blue",outlier.colour='red',
     lwd=.1)+
  coord_flip()+ hwTheme2

# The red dot looks small.  Can it be larger?  
?geom_boxplot

ggplot(data=singer, aes(x=voice.part,y=height)) +
  geom_boxplot(width=.6,col="blue",
  outlier.colour='red',outlier.size=3.5, lwd=.1)+
  coord_flip() + hwTheme2

# The boxplot construction is easy to understand when
# quartiles are familiar.   

# 4. Wrapped quantile and cumulative probability plots
# 
# For each voice part we pair heights sorted in
# ascending order (quantiles) with 
# ascending order cumulative probabilities
# to produce quantile or cumulative probability plots.

# The tapply() function automates the processing by
# applying a function to the heights of each voice
# part and returning all the results in a list. 
#
# tapply() has three basic arguments,
#   a vector of data, 
#   a factor or list of factor indicated
#     class membership, and 
#   a function to apply to the data  
#     in individual classes 
#   an optional 4th argument will pass
#     through to the function  

# Below we apply the mean() function
# to the height data for each voice part.

meanHeightVector <- with(singer2,
  tapply(height, partOrder, mean) )
meanHeightVector
round(meanHeightVector,1)

# The result is list of named single
# value vectors. R can return this
# as vector with names and does so.
# 
# If any the vectors of the list were
# longer than 1 the returned result would
# remain a list. In the next example.
# the sort function returns a vector
# for each voice part and these
# differ in length.

heightList <- with(singer2,
  tapply(height,partOrder, sort))  
heightList

# We can collapse this list of vectors,
# heightList, into a single vector using
# the function unlist().  

heightQ <- unlist(heightList)
head(heightQ,40)

# Note that R constructed the vector 
# element names by pasting together
# factor level character string and 
# the index for the sorted vector. 
# The 37th value is the first from
# Soprano 1. 

# We can obtain the corresponding cumulative
# probabilities by againg using tapply()
# with the same class factor.
#
# This example shows using the 4th
# argument. We will pass the argument a=.5
# to points(). This makes ppoints use
# Cleveland's (i-.5)/n function for 
# all sample sizes. 

probList <-  with(singer2,
  tapply(height, partOrder, ppoints, a=.5))
heightP <- unlist(probList)

# Next we include our two vectors in the
# singer2 data.frame. 

singer2$heightQ <- heightQ
singer2$heightP <- heightP

# Note that once we had the partOrder
# and heightQ variables we no longer
# need the original voice.part and
# height variables.   

# 4.2 Produce the quantile and cumulative
#     probability plots

# Quantile plot
ggplot(singer2, aes(x=heightP, y=heightQ)) +
facet_wrap(~partOrder,ncol=2)+
geom_line() + geom_point(shape=21,fill='lightgreen')+ 
ggtitle("Singer Height Quantile Plot\n") + 
labs(x= "Cumulative Probability",y="Height in Inches\n")+
hwTheme2

# Change coordinate roles for
# a cumulative distribution plot

ggplot(singer2, aes(x=heightP, y=heightQ)) +
facet_wrap(~partOrder,ncol=2)+
geom_line() + geom_point(shape=21,fill='lightgreen')+ 
ggtitle("Singer Cumulative Probability Plot\n") + 
labs(y= "Cumulative Probability\n",x="Height in Inches")+
hwTheme2

# 4.3 Discussion quantile names
#     and cumulative probabilities
#     options 
#
# The .10 quantile is the 10th percentile. 
# The .25 quantile is the 1st quartile
# The .50 quantile is the median
# The .75 quantile is the 3rd quartile
# The .90 quantile is the 90th percentile
#
# In the book Visualizing Data, Cleveland
# refers to the values returned by the
# the formula (i-.5)/n for i= 1,...,n
# as f values rather than as cumulative
# probabilities, perhaps to distinguish
# these from the classic empirical distribution
# cumulative probabilities.   
#  
# A more general formula that returns values
# in the interval (0 1) is
# (i-a)/(n+1-2a) for 0 <=a < 1 for i=1,...,n
# If a=1 the interval is [0 1].  

# If we knew the sample was from a 
# normal distribution a=.375 is better choice.
# However the we could estimate the mean
# and standard deviation and use pnorm().
# When exploring data we don't know the
# distribution, so we do something reasonable
# such as pick a=.5. 
#
# Some have provided reasons for choosing
# a=1. Then for formula is 
# (i-1)/(n-1) for i=1,...,n
# An implication of this choice is
# that based on  random sample, x, 
# there is no probability that a future
# sample from the same distribution
# would produce a value outside
# [min(x) , max(x)].   
#        
# Often our data is not based on random samples
# from one or more populations. In exploratory
# data analysis the emphasis is on descriptive
# statistics rather than inference statistics
# The computational methods may be the same.        

# 4.4 Produce Normal Q_Q plots
#
# We have cumulative probabilities
# so we can used qnorm() add a column to singer2
# with the standard normal quantiles.

normQ<- with(singer2,
  tapply(heightP,partOrder, qnorm))  
singer2$normQ <- unlist(normQ)

tmp <- with(singer2,

ggplot(singer2,aes(x=normQ,y=heightQ)) +
  facet_wrap(~partOrder,ncol=2)+
  geom_point(shape=21,fill='lightgreen')+ 
  ggtitle("Normal Q-Q Plot\n") + 
  labs(y= "Height in Inches\n",x="Standard Normal Quantiles")+
  hwTheme2

# We can compare this to Cleveland's Figure 2.11 on page 32.
# Our version lacks the robust fit straight lines. 
# Section 5 show a line in the  single plot
# context. Neither includes pointwise confidence
# intervals that can be helpful to assessing departures
# from the normal distribution.  

# 5. Q-Q plots comparing two data vectors
# 
# The matrix of pairwise Q-Q plots in Clevelands Figure 2.5
# can be produced used lattice graphics but involves
# knowing more about lattice. It likely can be done in
# ggplot2. Its straight forward with R's base
# level graphics and my panel layout functions. For now
# we focus on individual plots. 

# 5.1 A single Q-Q plot
# 
# We compare Soprano 1 and Bass 2 heights
# They have 36 and 26 values respectively.
# 
# We could compute our own quantile pairs
# for plotting as indicated below. 

x <- singer$height[singer$voice.part=='Soprano 1']
y <- singer$height[singer$voice.part=='Bass 2']

probs <- ppoints(min(length(x), length(y)), a=1)
xQuants <- quantile(x, probs, type=7)
yQuants <- quantile(y, probs, type=7)

cbind(x,y)

# The above script computes cumulative probabilities
# from the shorter vector.  The quantile
# function generates its own cumulative probabilities
# based on the length of the data. 
# It sorts the data to obtain quantiles.
# It interpolates obtain quantiles for the 
# requests cumulative probabilities in the
# function argument. This is the probs vector above. 
#
# For the shorter vector the internal and request
# cumulative probabilities match so the function
# returns the sorted values as quantiles. 
# 
# Interpolation produces the requesed quantiles
# for the longer vector.     

# The R qqplot() function does this more
# expeditiously and will produce a Q-Q plot.

qqplot(x,y)

# To shorten the script we borrow the qqplot
# quantils. To improve the plot we
# make our own.    

# First we compare the qqplot quantiles 
# with those above. 

qqDat <- qqplot(x,y,plot=FALSE)

all(xQuants==qqDat$x)
all(yQuants==qqDat$y)

# For our graphics we will
#
# 1 uses the range for all the singer
#   heights for the x and y scales.
#   This the foundation for comparing
#   Q-Q plots for all pairs of 
#   voice parts.
# 2 show grid lines
# 3 show the equality line x=y
# 4 show a robust fit line to the q-q points
# 5 show the points
   
x <- singer$height[singer$voice.part=="Bass 2"]
y <- singer$height[singer$voice.part=="Soprano 1"]
qqplot(x,y)
     
# We could convert the script below into an R function
# that hides the details. 

# Get the variables
x <- singer$height[ singer$voice.part=="Bass 2" ]
y <- singer$height[ singer$voice.part=="Soprano 1" ]

# Find the global data range
rx <- range(singer$height) # height range of all parts

# Get the qq pairs and make a data frame
qqDat<- qqplot(x,y,plot=FALSE)
qx <- qqDat$x
qy <- qqDat$y
df <- data.frame(qx,qy)

# fit a robust regression line
rFit <- rlm(qy~qx)$coef # robust regression line

ggplot(df,aes(x=qx,y=qy))+
  geom_abline(intercept=0,slope=1,col="black")+
  geom_abline(int=rFit[1],sl=rFit[2],
     size=1.5,col=rgb(0,.8,0))+
  geom_point(size=3.2,col="blue")+
  xlim(rx)+ylim(rx)+
  labs(x="Bass 2 Height",y="Soprano 1 Height",
    title="Q-Q Plot of Singer Height in Inches")+
  hwTheme2

# The scalable vector graphics plot should be square
# so that black x=y line is at 45 degrees.

# 5.2 The corresponding MD plot (mean and difference plot)

# Below we also use a robust line to bring out another
# pattern

qMean <- (qx + qy)/2
qDiff <- qx - qy
df <- data.frame(qMean,qDiff)
rFit <- rlm(qDiff~qMean)$coef # robust regression line

ggplot(df,aes(x=qMean,y=qDiff))+
  geom_hline(yint=mean(qDiff),size=1.2)+
  geom_abline( int=rFit[1],sl=rFit[2],
       size=1,col=rgb(0,.8,0))+
  geom_point(col="blue",size=3.2)+
  labs(x="Mean of Heights in Inches",
       y="Bass2 - Soprano1 in Inches",
  title="MD plot of Paired Soprano1 and Bass2 Quantiles")+
  hwTheme2
  
# The horizonal line shows that on the average
# Bass2s are roughly 7 inches taller the
# Soprano1s.  
# The robust fit line calls attention to the pattern
# the difference gets larger as average of taller men
# and women gets larger.  
  
# 5.3 A Q-Q Plot of soprano1 residuals versus
#     pooled residuals
#
# The approach of pooling residuals is powerful
# and used for inference in analysis of variation
# and regression when the 0 centered residual
# distributions have roughly the same standard deviations 
#
# Under the same conditions data descriptions can be
# simplify.  
#
# We could use functions like oneway() analysis of
# variance to get means and residuals for the singer
# data. Here we write our own residual producing function for 
# tapply() to use.  

# Remove the voice.part means to get residuals
myResiduals <- function(x) x - mean(x)
resList <- with(singer2, 
  tapply(height, partOrder, myResiduals))

resAll <- unlist(resList)
resS1 <- resAll[singer2$voice.part=="Soprano 1"]
rx <- range(resAll)

# Get the paired quantiles
qqDat<- qqplot(resAll,resS1,plot=FALSE)
qAll <- qqDat$x
qS1 <- qqDat$y
df=data.frame(qAll,qS1)
# robust regression line for paired quantiles
rFit <- rlm(qS1~qAll)$coef

ggplot(df,aes(x=qAll,y=qS1))+
  geom_abline( int=0,sl=1,
      size=1,col="black")+
  geom_abline(int=rFit[1],slope=rFit[2],
      size=1.2,col=rgb(0,.7,0))+
  geom_point(col="blue",size=3.2)+
  labs(x="Pooled Residuals in Inches",
       y="Soprano1 Residuals in Inches",
       title="Q-Q Plot")+
  xlim(rx)+ylim(rx)+
  hwTheme2

# Make the plot square plot and compare it to 
# to the soprano 1 panel in Cleveland's book.  
# There are a lot of ties but the straight line fit
# is reasonably good. The smaller slope of the green
# line suggests a somewhat smaller standard deviation
# for the Soprano1 residuals than the composite of all 
# residuals.

sd(qS1)
sd(qAll)

# This is not very surprising.  Fairly often
# increases in the mean are  accompaned by
# increases in the standard deviation (and variance).  

# 6. Wrap up comments
# 
# Cleveland indicates that we can provide a good description
# of the singer data using the voice part means and the standard
# deviation of the pooled residuals from fitting the means.
# There is some variation in standand deviation
# from voice part to voice part as indicated by the smaller
# Soprono 1 standard deviation but this is not excessive.
#
# If in fact all the voice part residuals are from the same 
# distribution we would expect the pooled residuals to provide
# a better estimate of the standard deviation. How much 
# strength can we borrow from somewhat similar data? This
# is an old question that modern shrinkage methods strive
# to answer.   
#
# 
# Cleveland's closes sections with residual and fit plots
# to provides a visual indication of how much the total
# variation is associated with the fit and how much is left
# in the # residuals. An R-squared statistic can
# provide a quick numeric summary while a plot can 
# provide a visual impression about how much noise is left.   
# 
# In other situations data sets being
#    compared may be
# 1) bell shaped but have substantially different
#    standard deviations. Then a single standard deviation
#    would provide an inadequate description. 
# 2) symmetric but not bell shaped.
#    While the means still would indicate distribution locations,
#    the shapes need to be described. This might address 
#    the range, tail thickness and other features.
# 3) The distributions may be skewed.  Then including the median
#    and something about the tails becomes important. 
# 4) The distribution may be multimodal.    
# 
# At some point it's easier to look at density plot than 
# study numerous numeric descriptive statistics.  Still,
# statistical graphics and word together can be more effect
# for communication to ourselves and others than either alone. 
# 
# I suspect that numerous numeric descriptive statistics 
# for a distribution can lead to a quick shifting of
# attention elsewhere. Simple appearence and parsimony
# are important.      
# 
# Further into the Visualizing Data Cleveland shows
# that sometimes data transforms can made distributions
# easier to describe and compare.
# 
# He also addresses distributions that don't have such
# parimonious descriptions and clean comparisons.
   

