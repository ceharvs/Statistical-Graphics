#                             Univariate Distribution Graphics
                                    
# Sections
                             
# 1.  Normal family of distributions    
setwd("C:/Users/chris_000/Documents/school/Statistical-Graphics/Week 2")

# 2.  Exponential family of distributions
# 2.1 R exponential distribution functions
# 2.2 Compare two exponential density plots                              

# 3.  Gamma family of distributions
# 3.1 Comparing densities with different rates
# 3.2 Comparing densities with different shapes

# 4.  Density, Box plot and Q-Q plots from random samples (or data)
# 4.1 Random samples from gamma distributions
# 4.2 Explortory Data Analysis plots
    
# 5. Sample cumulative probabilities and quantiles
# 5.1 Cumulative probabilities for a sample.
# 5.2 The empirical cumulative distribution
 
# 6.  Approximating quantiles and Q - Q plots
# 6.1 The quantile function and order statisics
# 6.2 A constructed Q-Q plot from two samples
# 6.3 The R qqplot() 
# 6.4 Adding 1st and 3rd quartile fit reference line
# 6.5 Brief comments on interpretation  


# Due___________________________________
# 1.  The standard normal Quantile plot
# 2.2 Juxtaposed Density plots
# 3.2 Juxtaposed Density plots
# 4.2 One Eda plot, your choice
# 6.4 Q-Q plot with reference line

0. Setup

# cd to the folder with the files
# Install package: car                             

source('Class Distribution Functions.r')
source('panelFunctions.r')
library(car)
library(ggplot2)
library(gridExtra)
hwTheme <- theme_gray()+
  theme(
    strip.background=element_rect(fill=rgb(.9,.95,1),
        color=gray(.5), size=.4),
    strip.text=element_text(size=rel(1.05)),
    panel.border=element_rect(fill=FALSE,colour=gray(.50)),
    axis.text=element_text(colour="black"),
    panel.grid.minor = element_blank())

#  1. Normal family examples
#
# The standard normal distribution has 
# mean=0 and standard deviation 1

p <- ppoints(300)  # generic cumulative probabilities
q <- qnorm(p)      # quantiles for cumulative  probs 
d <- dnorm(q)      # densities for quantiles
# Note qnorm() and pnorm() inverse functions
# p = pnorm(qnorm(p)) and q = qnorm(pnorm(q))                           

df <- data.frame(p,q,d) # a data frame.                               

# Interpolated cumulative probability plot
# This connect the 300 points with lines.                             
ggplot(df,aes(x=p, y=q)) + 
  geom_line(col="blue" , size = 1.2)+ hwTheme + 
  labs(x='Quantiles',y='Cumulative Probabiliies',
  title="Standard Normal\nCumulative Probability Plot")
                               
ggplot(df,aes(x=q, y=p)) + 
  geom_line(col="blue" , size = 1.2)+ hwTheme + 
  labs(x='Quantiles',y='Cumulative Probabiliies',
  title="Standard Normal\nQuantile Plot")
                             
# The two plots switch the x and y coordinates
# The y axis determins the plot name. 
# I like to see density plots                             
                             
# Side by side density plots for two normal
# distributions                           
                             
# Generic labels for more than one plot                             
axisLabs <- labs(x='Quantiles',y='Density')                             

# Standard normal density
#
# We can name plots to save them
# Providing the name will produce the plot                                                
plot1 = ggplot(df,aes(x=q,y=d)) +
  geom_area(color="blue",fill=rgb(0,.5,1),alpha=.2)+
  axisLabs + hwTheme +
  ggtitle('Normal Density')
plot1                             

# Normal density mean 100 standard deviation 10 
q <- qnorm(p, mean = 100, sd = 16) 
d <- dnorm(q, mean = 100, sd = 16)
df <- data.frame(p, q, d)

plot2 = ggplot(df, aes(x=q, y=d)) + hwTheme +
  geom_area(color="blue",fill=rgb(0,.5,1),alpha=.2)+
  axisLabs +
  ggtitle('Normal Density Mean=100, SD=16')                             
plot2
                             
# We can juxtaposed separate plots
grid.arrange(plot1, plot2, ncol=2)                              
                              
# The plot shapes are identical.  
# The grid line labels indicate that both
# x and y scales differ.
#
# To construct plot2 we could have multiplied
# the standard normal quantiles by 16 
# to change the scale and
# added 100 to shift distribution center.    
#
# A distribution density must
# integrate to 1 so we would divide the 
# the standard normal densities by 16 to
# adjust for stretching scale by 16.
#                     
# Because the x and plot scale difference
# only by linear transfromation the linear
# tranformation of the two plots to put them                   
# into two screen windows of the same size
# makes the their curves appear identical.
# Other than the title, it is grid
# lines labels that tell the story of the
# curves being very different! 
# 
# The Grammar of Graphics by Leland Wilkinson
# refers to the scales and legends as guides
# to plot interpretation. To interpret a plot,
# take note of the scales (and the units of
# measure when provided.) They may be the only
# plot elements telling the story.

# 2. Exponential family of distributions
# 
# We can think of the exponential family as characterizing
# the waiting time to one Poisson event such as radioisotope
# disintegration. 
#    Rather the time, other measures can provide
#    the context for Poisson events. The distance
#    along a road to Poisson events such as crime
#    locations or pothots examples.
#                             
# This family has one parameter that is often written
# using the Greek letter lambda and the f(x) = {e}^{- lambda x}
# for x >=0. See page 186 in R for Everyone.
#
# Think of lambda as the number of Poisson events per unit time.    
# The mean of the exponential distribution with rate lambda is 1/lambda .  
# If the Poisson rate is 1 event per day the expected waiting
# for an event is 1 day.  If the Poisson rate is 2 events per day the
# expected waiting time is 1/2 day.
# 
# * You can skip this paragraph.  
#   Texts pararameterize some families of distribution in different
#   ways. This can to lead confusion.  Distinguishing between
#   rate and scale parameters can be helpful. The parameter lambda is
#   called rate parameter because it multiplies x in the exponent of e.
#   A scale parameter divides x in the exponent of e.  This
#   understanding is particularly relevant to the gamma distribution
#   where R supports providing either a rate or the scale parameter. 
          
# 2.1 R exponential distribution functions 
# 
# 
# Usage  
# 
# dexp(x, rate = 1, log = FALSE)
# pexp(q, rate = 1, lower.tail = TRUE, log.p = FALSE)
# qexp(p, rate = 1, lower.tail = TRUE, log.p = FALSE)
# rexp(n, rate = 1)
# 
# The default rate is 1 as shown above. 
# 
# See page 185 in R for Everyone for list or distributions
# and their function                             

# 2.2  Compare two exponential density plots
                             
p <- ppoints(300) # 300  computes 300 cumulative probabilities
q <- qexp(p)      # Find quantiles from cum. probs. 
d <- dexp(q)      # Find densities from quantiles

df <- data.frame(p,q,d)
plot3 = ggplot(df,aes(x=q, y=d)) + 
  geom_area(color="blue",fill=rgb(0,.5,1),alpha=.2)+
  axisLabs + hwTheme +
  ggtitle("Exponential Distribution: Rate = 1\nMean=1")                            
plot3
                             
# Rate 2, expected waiting time = 1/2 
q <- qexp(p,rate=2) 
d <- dexp(q,rate=2) 
df <- data.frame(p,q,d)
plot4 = ggplot(df,aes(x=q, y=d)) + 
  geom_area(color="blue",fill=rgb(0,.5,1),alpha=.2)+
  axisLabs + hwTheme +
  ggtitle("Exponential Distribution: Rate=2\nMean=1/2")                            
grid.arrange(plot3, plot4, ncol=2) 
                             
# Again the scale change for a density plot
# leads to the curve appearing to being the same.
# How so the two x-axes differ?
# How do the two y-axes differ?
                            
# 3. Gamma family of distributions
# 
# This family has two parameters rate and
# shape.  It contains the exponential family 
# 
# rate = 2:   Poisson mean: 2 events per unit time 
# shape = 3:  Waiting time to 3rd Poisson event  
# shape = 1:  Gives the exponential family.
#
# The shape parameter does not
# have to be an integer.                            
# When shape parameter is an integer
# the distribution is sometimes
# called the Erlang distribution
             
# 3.1 Comparing densities with different rates  

p <- ppoints(300)
q <- qgamma(p,rate=1,shape=3) 
d <- dgamma(q,rate=1,shape=3)
df <- data.frame(p,q,d)
plot5 = ggplot(df,aes(x=q, y=d)) + 
geom_area(color="blue",fill=rgb(0,.5,1),alpha=.2)+
axisLabs + hwTheme +
ggtitle("Gamma Distribution: Rate = 1, Shape=3")                            

q <- qgamma(p,rate=2,shape=3) 
d <- dgamma(q,rate=2,shape=3)
df <- data.frame(p,q,d)
plot6 = ggplot(df,aes(x=q, y=d)) + 
  geom_area(color="blue",fill=rgb(0,.5,1),alpha=.2)+
  axisLabs + hwTheme +
  ggtitle("Gamma Distribution: Rate = 2, Shape=3")                                                      
grid.arrange(plot5, plot6, ncol=2) 
                                                                                                                                          
# 3.2 Densities with with different shapes  
                             
p <- ppoints(300)
q <- qgamma(p,rate=2,shape=3) 
d <- dgamma(q,rate=2,shape=3)
df <- data.frame(p,q,d)
plot7 = ggplot(df,aes(x=q, y=d)) + 
  geom_area(color="blue",fill=rgb(0,.5,1),alpha=.2)+
  axisLabs + hwTheme +
  ggtitle("Gamma Distribution: Rate = 2, Shape=3")                            
                             
q <- qgamma(p,rate=2,shape=30)
d <- dgamma(q,rate=2,shape=30)
df <- data.frame(p,q,d)
plot8 = ggplot(df,aes(x=q, y=d)) + 
geom_area(color="blue",fill=rgb(0,.5,1),alpha=.2)+
axisLabs + hwTheme +
ggtitle("Gamma Distribution: Rate = 2, Shape=30")                                                      
grid.arrange(plot7, plot8, ncol=2) 
                             
# When the shape parameter is 30 distribution
# looks similar to a normal distribution.
# Is there a reason?                               
# When the shape paramter is 30
# the distribution is the sum of
# 30 waiting times: the  
# waiting time to the 1st Poisson event
# and the 29 waiting times to success next
# events. The Central Limit Thereom
# says the sum of n independent
# identically random variables converges
# to normal distribution as n gets large,
# provided the random variables have a finite
# variance.
                             
# Side Note: The normal distribution can provide a
# reasonble approximation for n = 30 if the
# random variable distribution does not
# have thick tail. The exponential distribution
# right tail is thicker than that of the
# normal distribution so larger shape
# parameter would further reduce
# the lack of symmetry and yield a more
# more bell-shape curve.
                                                                          
# 4. Density, Box plot and Q-Q plots from data
# 
# Here we start with a function, classEda(), that produces
# all three plots. Then we consider the construction
# of Q-Q plots. Next week's lecture assignment
# box plot construct and say more Q-Q plots interpretation
#     
# 4.1 Using random samples from the gamma distribution
# 
# R distribution functions for generating
# random number all have the prefix r.
# See page 185 in R for Everyone
# The first argument is the sample size.  
# Remaining arguments are parameters
# specifying the specific distribution in the
# family. 

# 4.2 Explortory Data Analysis plots
                             
set.seed(131)  # Lets us get the same sample later
x <- rgamma(100, rate=2, shape = 2)                             
classEda(x, varLab1= "Gamma Sample Size=100",
            varLab2= "Shape = 2, Rate = 2",
            varUnits="Seconds to 2nd Isotope Disintegration")             

set.seed(137)
x <- rgamma(200, shape = 30, rate = 5)
classEda(x, varLab1= "Gamma Sample Size=200",
            varLab2= "Shape=30, Rate = 5 Pounds Per Month",
            varUnits="Diet Plan: Months to Lose 30 Pounds")           

#5. Sample cumulative probabilities and quantiles
# 
# 5.1 Cumulative probabilities for a sample.
# 
# We use the function ppoints (n) where
# n is the sample to product cumulative
# probability.  For n > 10 this is the
# function cited in Cleveland. 
# it is (i-.5)/n where n is the 
# sample size and i stands for integers 1,2, ..., n.
# 
# For n <= 10 it is (i-.375)/(n+1-2*.375).  
# This is better for the normal distribution
                             
set.seed(151)
x <- rgamma(100, shape=3, rate = 2)
prob <- ppoints(x) 
prob 
quant <- sort(x) # The sorted values are quantiles
df <- data.frame(quant,prob)

ggplot(df,aes(x=quant,y=prob))+
  geom_line(col=rgb(0,.5,1)) + 
  geom_point(size=3,col="blue")+
  hwTheme +
  labs(x="Quantiles",y="Cumulative Probability",
      title="Sample size=100,Gamma Rate=2, Shape=30")                               

# 5.2 The Empirical Cumulative Distribution Function
# 
# This classic from has probability jumps of
# 1/n at the n quantiles of the sample. This
# is good for proving asymptotic theorems about
# distributions.  
#                             
# The distribution is continuous from the right
# at the jumps but not from the left.  
# p = F(X<=x)
# The distribution jumps 1/n at exactly the
# sort x values and stays flat until the next sorted value.  
# 
# In R we can write our own functions.  We could
# write functions to make the production of density plots                             
# involve less typing     
                          
# Below the instuctor write classEcdf() function
# only require the random sample values 
                             
classEcdf(x)
                             
# 6. Approximating quantiles and Q - Q plots
# 
# We have seen how easy it is to get
# quantiles from a theoretical distribution
# given cumulative probabilities.  The task here
# address obtaining approximate 
# data quantiles for given cumulative probabilities.   

# 6.1  The quantile function and order statistics
# 
# Given a sample in a data vector, say x, and a desired
# vector of cumulative probabilities, such as p = c(.25,.50,.75)
# the quantile() function will sort x values to obtain
# order statistics and interpolate between these
# as needed obtain quantile approximations to 
# match the given cumulative probabilies.  


p <- c(0, .25, .50, .75, 1)
quantile(x,p)
min(x)
median(x)  
max(x)
IQR(x)  # interquartile range

6.2 Constructing a Q-Q plot from two samples

set.seed(4211)
set1 <- rgamma( 70, shape=3, rate=2)
set2 <- rgamma(100, shape=3, rate=4)

commonProbs <- seq(0,1,length=50)

set1quant <- quantile(set1, commonProbs, type = 5)
set2quant <- quantile(set2, commonProbs, type = 5)

df <- data.frame(set1quant,set2quant)
ggplot(df,aes(x=set1quant,y=set2quant))+
 geom_line(size=1,col=rgb(0,.5,1))+
 geom_point(size=3,col="blue")+
 labs(x="Rate 2 quantiles",
      y= "Rate 4 quantiles",
      title= "Q-Q plot")+hwTheme

# The quantile function 9 ways to compute quantiles 
# Type 5 corresponds to Cleveland's approach. 
# If you omit the type argumen you get R
# default which type 7. Either
# fine in his class.
                             
# 6.3 The R qqplot() 
# 
# Producing Q-Q plots to compare two set data is easy
# with the old qqplot() function. 
                             
qqplot(set1,set2, type='o',las=1)
                             
# 6.4 Adding 1st and 3rd quartile fit reference line
#
# A package may have a function that does this. 
# Here produce our own plot with grid lines
# and a reference line. It goes through
# the paired first and third quantiles.                               

df <- as.data.frame(qqplot(set1,set2,plot=FALSE)) # same the estimated quantiles

p <- c(.25, .75)  # 1st and 3rd Quartiles
xQuartiles <- quantile(set1,p)
yQuartiles <- quantile(set2,p)
# Fit a linear model to get the intercept and slope                             
ans <- lm(yQuartiles~xQuartiles)$coef                             
                           
ggplot(df,aes(x=x,y=y))+ 
  geom_abline(int=ans[1],slope=ans[2],col='blue')+
  geom_point(size=2.5,col="red")+hwTheme+
  labs(x="Set1 Quantile",y="Set2 Quantiles",
       title="Q-Q Plot")   

# 6.5 Brief comments on interpretation                             
# The straight line fits unusually well.  This suggest the
# distributions have the same shape and differ only in
# location (median or mean) and
# scale (interquartile range or standard deviation)
# 
# When line fits well the slope estimates ratio of two
# standard deviation. 

# Estimated slope and ratio of standard deviations                             
ans[2]
sd(set2)/sd(set1)

# Quick distribution summaries                             
summary(set1)                            
summary(set2) 
                             
# More commonly a Q-Q the data set's quantiles 
# to the a normal distribution quantiles
# The next class will discussed this. 
 


 
