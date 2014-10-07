#                              Smoothes y=f(x) 
# 
# Motivation for smoothing and a description of loess smoothes
# appears in Cleveland's Visualizing Data Chapter 3 pages 87 - 100                               
#                              
# This assignment currently feature loess smoothing and polynomial fits.  
# The spline package for natural splines smooths is not available
# for the current version of R

# Due: plots for 2,3,4,6 and 7 for 5 point 

# The hamster data sources are 
# 
# J.M. Chambers, W. S. Cleveland, B. Kleiner, P.A Tukey, 1983.
# Graphical Methods for Data Analysis, Wadsworth Brooks Cole,
# Pacific Grove CA.
#   
# Lyman et al. 1981.  Hibernation and longevity in the Turkish hamster.
# Science 212, p668-670.

1. Setup

library(ggplot2)
# library(splines)
hamster <- read.csv(file='hamster.csv',header=T)
head(hamster)

hwTheme <- theme_gray()+
  theme(
    strip.background=element_rect(fill=rgb(.9,.95,1),
                                  color=gray(.5), size=.4),
    strip.text=element_text(size=rel(1.05)),
    panel.border=element_rect(fill=FALSE,colour=gray(.50)),
    axis.text=element_text(colour="black"),
    panel.grid.minor = element_blank())

# 2. Loess smoothes with different spans

qplot(hibPercent,ageDays, data=hamster)+
  geom_smooth(method="loess", span=.25, se=FALSE, size=4, col="green")+
  geom_smooth(method="loess", span=.5, se=FALSE, size=2.5, col=rgb(0,.5,1))+
  geom_smooth(method="loess", span=.75, se=FALSE, size=1, col="black")+
  geom_point(color="red")+ 
  hwTheme+
  labs(x="Percent of Life in Hibernation",
      y="Age in Days",
      title="Hamster Data\nLoess Smoothes, Different Spans")

# 3. Loess smoothes with fix spans,
#    locally quadratic versus locallylinear

qplot(hibPercent,ageDays, data=hamster)+
  geom_smooth(method="loess", span=.2, degree=2, se=FALSE, size=4, col="green")+
  geom_smooth(method="loess", span=.2, degree=1,se=FALSE, size=2.5, col=rgb(0,.5,1))+
  geom_point(color="red")+
  hwTheme+
  labs(x="Percent of Life in Hibernation",
      y="Age in Days",
      title="Hamster Data\nLoess Linear Versus Quadratic, Span=.2")

# The general pattern is monotonic increasing or monotonic decrease
# a local linear fit (degree 1) is better
# If there peaks and valleys a local linear fit will tend to lop off peaks
# and fill in valleys. A local quadratic fit, the default, is better.
# Of course a big span can keep a local quadratic fit from tracking
# local patterns.

# 4. Fitting polynomial of different degrees

qplot(hibPercent,ageDays, data=hamster)+
  geom_smooth(method="lm", formula=y~poly(x,5),se=FALSE, size=4, col="green")+
  geom_smooth(method="lm", formula=y~poly(x,3),se=FALSE, size=2.5, col=rgb(0,.5,1))+
  geom_smooth(method="lm", formula=y~poly(x,2),se=FALSE, size=1, col="black")+
  geom_point(color="red")+
  hwTheme+
  labs(x="Percent of Life in Hibernation",
      y="Age in Days",
      title="Hamster Data\nDifferent Polynomial Fits")
 
# # 5. Natural splines Skip: package not available for R version 3.1.1 
# qplot(hibPercent,ageDays, data=hamster)+
#   geom_smooth(method="lm", formula=y ~ ns(x, 5),se=FALSE, size=4, col="green")+
#   geom_smooth(method="lm", formula=y~  ns(x,3),se=FALSE, size=2.5, col=rgb(0,.5,1))+
#   geom_smooth(method="lm", formula=y~ ns(x,2),se=FALSE, size=1, col="black")+
#   geom_point(color="red")+
#   hwTheme + 
#   labs(x="Percent of Life in Hibernation",
#       y="Age in Days",
#       title="Hamster Data\nDifferent Natural Splines")

# 6. Linear model and with standard errors

qplot(hibPercent,ageDays, data=hamster)+
  geom_smooth(method="lm", formula=y ~ x, size=1.5, col="blue")+
  geom_point(color="red")+
  hwTheme + 
  labs(x="Percent of Life in Hibernation",
       y="Age in Days",
       title="Hamster Data\nLinear Fit and Standard Errors")
       
# 7. Upper and lower smoothes
#    
# A smooth splits the data into upper and lower sets.
# These sets can be smoothed with the results attached
# to the original smooth.  This tends to bracket the
# inner half of the data and bounds the central smooth.
# 
# These three curves provide a nice caricature to put
# in a scatterplot matrix. It provides another way
# to scan for a tight functional relationship.  
# 
# Sometimes we also want to look at x=f(y).  
# We can show both without messy over plotting
# if we used both upper and lower triangles of a
# scatterplot matrix.  

head(hamster)
x <- hamster$hibPercent
y <- hamster$ageDays

ans <- loess(y~x)       # Read "y is modeled by x"
# see the components in ans
names(ans)

# get the residuals from the regression objects
res <- residuals(ans)

high <- res>0

set1.x <- x[high]
set1.y <- res[high]

set2.x <- x[!high]
set2.y <- res[!high]

# smooth the upper and lower residuals
ans.high <- loess(set1.y~set1.x)
ans.low  <- loess(set2.y~set2.x)

# add on the middle smooth
yhat <- predict(ans)
upper <- predict(ans.high)+yhat[high]
lower <-  predict(ans.low)+yhat[!high]

qplot(hibPercent,ageDays, data=hamster)+
  geom_line(aes(x=set1.x, y=upper),size=1.2,col=rgb(0,.5,1))+
  geom_line(aes(x=set2.x, y=lower),size=1.2,col=rgb(0,.5,1))+
  geom_line(aes(x=x,y=yhat),size=1.4,col="red")+  
  geom_point(color="black")+
  hwTheme + 
  labs(x="Percent of Life in Hibernation",
       y="Age in Days",
       title="Hamster Data\nLower, Center, and Upper Smoothes")
 
# Construction of a similar plot  
#
# One can partition the data along the x-axis. 
# x group find the mean of x values the estimate
# first and 3rd quartiles the y values. These x and values
# for the different groups can be used to obtain
# another form of upper and lower smoothes.  
#
# Cleveland uses of creating x groups under the
# label slicing in section 3.7 on Slicing. He focues
# on the spred of residual from the center smooth.  
#
# Given x groups, box plots of y values may also call
# attention to thelocal variation in the
# vertical spead of values.  

#