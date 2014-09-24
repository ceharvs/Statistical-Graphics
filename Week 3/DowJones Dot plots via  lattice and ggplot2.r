		Superposed and Justaposed Dot Plots
              Using the lattice and ggplot2 packages 
    
# Sections
#
# 1. Date frames, read.csv() and data frame subscripts 
# 2. Character strings and factors 
# 3. A lattice dotplot() 
# 4. A ggplot() dotplot
# 5. Superposed dot plots with two variable per case
#    and a legend
# 6. Refining ggplot() defaults
# 7. Juxtaposed dot plots
            
# Due: 6 plots and 2 question answers in word or pdf file.
# Points: 8 
# 
# From 5. the plot from the lattice dotplot()
# From 6. The refined ggplot()
# From 7. The two plots.  
# Like 7. Produce two more plots like using
#         the older data in the "DowJones2012Aug28.csv". 
#         Change the date in the labels
#         Change the symbol colors in the plots.  
#         Change the rgb(1,0,0) (or red) to "orange"
             and rgb(0,.5,1) to "blue". 
#         Make the orange symbols smaller.  
# Questions
#   a) Do you prefer the black or gray text in ggplot?  
#   b) What a reasonable specification for orange in
#      term red green and blue intensities on a 0 to 1 scale?
      
            
# 0. Setup
#    Install the ggplot package
#    The working directory must contain
#    two Dow Jones csv files
#
#    References such as p226 are to the
#    R Graphics Cookbook
    
# 1. Data frames, read.csv() and data frame subscripts
# 
# We make frequent use of data frames in this class.
# 
# Reading a row and column data file into R
# typically produces a data frame. For example
# R's read.csv() function will read a file with a 
# cases by variables data set stored as  
# rows and columns in a comma separated value (csv)
# format and produce a data frame.
# The csv file format is common and can be written
# by a wided variety of software products such as Excel.  
setwd("C:/Users/chris_000/Documents/school/Statistical-Graphics/Week 3")
		
DowJones <- read.csv(file="DowJones2014Jan18.csv")
DowJones
head(DowJones)  # the first data row "3M Co"
tail(DowJones)  # the last row is "Walt Disney" 

# Data frames have row and column names. 
# The read.csv() default argument, header=TRUE,
# treats the first row in the file as column
# names. The function uses the counting integers
# as the default row names. Technically, R 
# sorts the row names as character strings.  
 
# The example below show choosing a column
# in the data file to use as row names.   
# For a data frame R requres unique names
# because R provides access to data frame
# rows and columns by name.
# R uses paired brackets, [], to specify
# indices and a comma to distinguish row
# indices from a column indices.
#    
# Side note: What Lander's book calls index
# vectors I have called these subscript vectors
# from many years. When creating "subscript" 
# vector that I only want to use with the
# next few script lines often named
# the vector subs to designate its use. 
# 
# A single index can be specific as a constant
# as shown below or as names vector of length
# one.    
       
DowJonesTmp <- read.csv(file="DowJones2014Jan18.csv",
  row.names=1)

head(DowJonesTmp)  # the first data row "3M Co"
DowJonesTmp["Boeing",]
DowJonesTmp[,"Weekly"]
rm(DowJonesTmp)
 
# The script below produces graphics based on the 
# DowJones data frame, so the script above removes
# the DowJonesTmp data frame that is not needed from
# the R workspace with the rm() function.
# 
# In R there may be many ways to manipulate data
# and produce the desired graphics. For clarity
# purposes along the way it can be helpful use
# data frames with meaningful row names.  
# 
# To avoid problems in accessing data R requires
# that the data frame row and column names be unique.
#      
# More complex data structures stored in data frames
# often have integers as rownames because integers 
# can be both unique and useful in some ways.
# More meaningful but replicated name may 
# may be in a column of the data frame  
   
# R matrices also provide row and
# and column access using names.
# For matrices unique row and column
# names are not required at creation time
# However R only provide name only accesses
# to the first occurence of repeated names  
# 
# Below there are two rows with the name
# Mat. If we want all occurrences
# we have to use explicit integer or logical
# subscript vectors.

mat <- matrix(1:12, nrow=4)
rownames(mat) <- c("James","Mat","Cloe","Mat")
colnames(mat) <- c("var1","var2","var2")
mat
    
mat["Mat",] # Only accesses the first occurrence
    
subs <- c("Mat","Mat")
mat[subs,]  # Accesses the first occurrence twice

subs <- rep("Mat",6) # 
mat[subs,]  # Returns the first occurrence
            # for each subscript
    
subs <- c(2,4)
mat[subs,]   # Accesses both occurences 

subs <- c(FALSE, TRUE, FALSE, TRUE)
mat[subs,]   # Accesses both occurences

		
mat[,"var2"] # only accesses the first ocurrence
		         # The single column matrix result 
		         # degenerates into a integer vector
		
is.matrix(mat[,"var2"])    
class(mat)
class(mat[,"var2"])
     
# Often we can convert a matrix into 
# directly data.frame. However this will not
# work for a matrix with duplicated
# row or column names.
#
# THe as.data.frame() function below
# creates an object called df. In this
# unusual example the error message is
# not immediate but appears when 
# we try to access and print the object.

df <- as.data.frame(mat)
df

# 2. Character strings and factors
# 
# When R reads file with rows and columns to
# produce a data.frame the default converts
# a column comprised of character strings
# into a factor.  We often use factors
# to represent categorical variables.  
# R makes use of factors for both plot
# construction and modeling. Learning
# about factors early on is helful. 
# 
# The script section below motivates the construction
# of a factor. With a short vector of unique character
# strings we can use a long vector of subscripts to
# create a long vector of characters strings that
# has multiple replicates.  
		
size <- c("Small","Medium","Large") # 3 items
size		
  # set the random sample seed to enable replication
set.seed(37)  
  # Randomly sample 50 values from  1, 2, and 3
subs <- sample(1:3,50,replace=TRUE) 
subs		
fullStringVector <- size[subs] # 50 character strings
fullStringVector
		
# Create a factor
sizeFactor <- factor(size[subs], levels=size)
levels(sizeFactor) # access its levels
as.integer(sizeFactor) # access its subscripts

# convert a factor to character string vector 
as.character(sizeFactor) 
		  
# The order of factor levels determining
# table cell ordering and multiple aspects
# of plot constructiona. These include
# the  order of bars in bar plots, 
# text in dot plots, symbols in a legend 
# subplots (or facets) in plot.
    
table(sizeFactor)
plot(sizeFactor)    
    
# Note that the left to right order
# of the bar categoris matches the order
# in the levels vector. 
# 
# The counting numbers for the levels
# provide plotting coordinates.  Implicit
# x coordinates in the plot are 1, 2, and 3.
#
# Graphics examples below change the 
# order of factor levels to change
# plot appearance. 

# When we don't provide levels when
# a factor is created, the factor() 
# function obtains the unique character
# strings and sort them alphabetically
# to use as levels. # Often this is not
# the order we want.  
    
sizeFactorB <- factor(size[subs])    
levels(sizeFactorB) 
plot(sizeFactorB)    
# Now Large appears first.    

# For analysis of variance or regression
# R will convert factors into columns of
# the model matrix. There is an example
# below but we address the topic later.     
y <- rnorm(50)    
model.matrix(y~sizeFactor)
    
# 3. A lattice dotplot()

# In script below the library() function
# provides access to the lattice function
# and data objects. 
# Access continues during the session.
	
library(lattice)
dotplot(Company~Yearly, 
  type='o',   # overplots points and lines
	data=DowJones, xlab="Yearly Percent Change")

# In a graphics context we read the symbol "~" as
# "is plotted against".  Explicitly Company names are to
# be plotted using y axis coordinates against their
# Yearly values using x-axis coordinates.    
# 
# The type='o' argument pair is key word and
# values pair.  The "o" cause the overploting
# of points and the connecting
# consecutive points with lines. 
# 
# The data = Dowjones argument pair means look in the
# data frame for the variables specified.  
    
# We can control several plot features. such
# as symbol size and color, and labeling. 
		
dotplot(Company~Yearly,
  type='p', # plot points
  col="red",
  cex=1.4,  # size 
	data=DowJones,
  main="30 Dow Jones Industrials",
  xlab="Yearly Percent Change")

# The "3M Co" appears as the first level of the factor.  
# It has a y coordinate of 1 and appears at the bottom
# of the plot. Walt Disney is the last item in
# the factor levels. It as a y coordinate of 30
# and appears at the top.  
		
# Side note: Graphics versus table conventions
# 		
# 	The graphics convention is based on standard
# 	Cartesian coordinates. The y-axis values 
# 	increases going upward. This differs from table
# 	convention that puts the first row at the top.
# 		
# 	Realizing there are different conventions can
# 	help us shift our perspective. This table versus
#   graph convention conflict will appear in other
#   contexts, such as the ordering of y-axis variables
#   when producing a scatterplot matrix.  The problem
#   of differing conventions seems here to stay. 
#   We can at least be aware the our choice of 
#   convention being, may be bothersome to some
#   people.  

# One way to redesign the plot is to  put the
# companies in decreasing order based on their
# yearly percent change.  We also experiment
# by putting source and data in the title
# and by changing the point color to purple. 

# The reorder()  function basically reorders
# the levels of the first argument based
# on the values of the variable appearing
# in the second argment. 
#
# Below, the with() function provides the DowJones
# data frame as the first context for finding
# variables usedin the second argument.
#    
# Below, note that the last level is Boeing
# and that Boeing appears at the top of the 
# subsequent plot. 

levels(with(DowJones,reorder(Company,Yearly)))    
    
dotplot(reorder(Company,Yearly)~Yearly,
  type='p', # plot points
	col="purple",
	cex=1.4,  # size 
	data=DowJones,
	main="30 Dow Jones Industrials\nWashington Post: Jan 18, 2014")
  
# 4. A ggplot() dotplot

library(ggplot2)
ggplot(DowJones, 
  aes(x=Yearly, y = reorder(Company, Yearly) )) +
    geom_point(cex=4,col="blue") +
    xlab("Yearly Percent Change") +
    ylab("")+ 
    ggtitle("Dow Jones Industrials\nWashington Post: Jan 18, 2014") 

# Above ggplot() will first look for variables in the
# DowJones data frame
#
# The aes() function specifies the aesthetic roles 
# variables are to serve in the graphics. Aesthetic roles
# include plot x and y position, symbol color, size, shape
# and subplot order. 

# We can include more lines of plot specificions using 
# the symbol +
# We leave discussion of functions such as geom_point
# to a later class.   

# 5. Superposed dots plots with two variables per case
#    and a legend
#  
# Some graphics tasks such as producing superposed dot plot
# using lattice or ggplot2 packages invovle the 
# constuction of special data frames, to deal with 
# multiple values per case that we want to plot values
# using different colors,  sizes or shapes. 
#
# Here we creae a data frame that   
# 1) stacks Weekly and Yearly values in the same column
# 2) uses each company name twice to label is two values
# 3) creates a factor whose values tell which variable is which.   
# The two rows for the same company won't necessarily appear together.      
#   
#    Company   PercentChange  VariableType
#  1  3M Co         .8             Weekly
# 31  3M Co       40.0             Yearly
#  ...
# 
# There are functions such as melt() to help us
# restructure data frames. If curious see section 15.19
# p364 in the R Graphics Cookbook.   
#  
# The script below directly constructs the columns
# needed to create a data frame called DowJonesStack.
 
nam = as.character(DowJones$Company)
levels <- levels(reorder(DowJones$Company,DowJones$Yearly))

DowJonesStack <- data.frame(
  CompanyYearly= factor(c(nam, nam),levels=levels), 
  PercentChange = c(DowJones$Weekly,DowJones$Yearly),
  Period = c(rep("Weekly",30),rep("Yearly",30))
 )
head(DowJonesStack)
tail(DowJonesStack)
 
# Both lattice and ggplot2 support plots that encoded factor
# levels using symbols that can have different color, size,
# and shape.  We provide a plot key or legend to
# communicate the encoding.
# 
# In the example below the point shape codes are
# 16 for filled circles and 17 for filled triangles.  
# We can control the size and color. Other codes would
# also let us control the symbol outline colors.  See p79  

my.key <- 
  list(space="right",
    text=list(levels(DowJonesStack$Period)),
    points=list(pch=c(16,17),
    cex=c(1.3,1.1),
    col=c("red",rgb(0,.5,1))))

dotplot(CompanyYearly~ PercentChange, 
  groups=Period,
  data=DowJonesStack, 
  pch=c(16,17),
  cex=c(1.4,1.2),
  col=c(rgb(1,0,0),rgb(0,.5,1)),
  key=my.key,
  main="30 Dow Jones Companies: Jan. 18, 2014")

# Now for ggplot version. The legend is automatic!

ggplot(DowJonesStack, 
  aes(x=PercentChange, y=CompanyYearly,
  colour=Period, shape=Period))+
  geom_point(size=3) +
  ylab("")+xlab("\nPercent Change")+
  ggtitle("30 Dow Jones Companies: Jan. 18, 2014\n")+
  scale_colour_manual(values=c('red',rgb(0,.5,1))) 
 
# I prefer ggplot2 over lattice when it comes to 
# legend production. For somes tasks I prefer lattice
# graphics.  Both packages can produce a wide
# range of grpahics and both have substantial
# learning curves. The learning can take place
# over time. I explain more script details in the 
# over time.  
     
# 6. Refining ggplot() defaults
#
# We can always consider redesigns. I like a border
# around the plot and want all of the text black.
# I generally prefer to omitting minor grid lines and
# want very light but colorful background for subplot
# labels.
#    
# Below I bundle my preferred specification for
# homework in a hwTheme object that can be added
# to a ggplot using +.  You may have develop
# and use other preferences.

   
hwTheme <- theme_gray()+
	theme(
	strip.background=element_rect(fill=rgb(.9,.95,1),
		  color=gray(.5), size=.4),
  strip.text=element_text(size=rel(1.05)),
	panel.border=element_rect(fill=FALSE,colour=gray(.50)),
		  axis.text=element_text(colour="black"),
	panel.grid.minor = element_blank())
    		
ggplot(DowJonesStack, 
	aes(x=PercentChange, y=CompanyYearly,
  color=Period, shape=Period)) +  
	geom_point(size=4) +
	ylab("")+xlab("\nPercent Change")+
	ggtitle("30 Dow Jones Companies: Jan. 18, 2014\n")+
	scale_colour_manual(values=c('red',rgb(0,.5,1))) +
  hwTheme

 
# Sometimes I remove superfluous tick marks the are
# superfluous n the presence of grid line.  Ideally 
# we could then move the grid line labels closer to  
# grid lines to support better perceptual grouping.
# However ggplot() does not give back the tick space.
		
        
# 7. Juxtaposed dot plots 

# Lattice graphics support conditioned panels
# based 3 or more factors.  There can be multiple
# strip labels at the top the indicate the
# particular levels of each of the multiple factors
# A plot can cover multiple pags.  
# 
# Below the vertical bar "|" can be read "conditioned on".  
# The example is particular simple in that there is only 
# on factor with two levels.

dotplot(CompanyYearly ~ PercentChange|Period, 
        groups=Period,
        layout=c(2,1), # 2 columns and 1 row
        data=DowJonesStack,
        scales= list(alternating=1),
        pch=c(16,17),
        cex=c(1.4,1.2),
        col=c(rgb(1,0,0),rgb(0,.5,1)),
        main="30 Dow Jones Companies: Jan. 18, 2014")

# ggplot() uses fact_grid() and fact_wrap() to produce
# subplots that lattice would call panels. The fact_grid()
# function support layouts with only one or two factors. In the
# notation, facet_grid(.~Period) appearing below specifies
# the subplots appear in different columns.  Reversing
# the order to (Period~.) would make the subplots appear
# as different rows.  

ggplot(DowJonesStack, 
	aes(x=PercentChange, y=CompanyYearly,
  color=Period, shape=Period)) +
	facet_grid(. ~ Period)  +  # p243
	geom_point(size=4) +
	ylab("")+xlab("\nPercent Change")+
	ggtitle("30 Dow Jones Companies: Jan. 18, 2014\n")+
  scale_colour_manual(values=c(rgb(1,0,0),rgb(0,.5,1)))+
  hwTheme+
  theme(legend.position="none") # Removes the legend:p226 
   

DowJones2 <- read.csv(file="DowJones2012Jan28.csv")
nam = as.character(DowJones2$Company)
levels <- levels(reorder(DowJones2$Company,DowJones2$Yearly))
		
DowJonesStack2 <- data.frame(
		  CompanyYearly= factor(c(nam, nam),levels=levels), 
		  PercentChange = c(DowJones2$Weekly,DowJones2$Yearly),
		  Period = c(rep("Weekly",30),rep("Yearly",30))
		)

dotplot(CompanyYearly ~ PercentChange|Period, 
		        groups=Period,
		        layout=c(2,1), # 2 columns and 1 row
		        data=DowJonesStack2,
		        scales= list(alternating=1),
		        pch=c(16,17),
		        cex=c(1,1.2),
		        col=c("orange","blue"),
		        main="30 Dow Jones Companies: Jan. 28, 2012")
    
ggplot(DowJonesStack2, 
		       aes(x=PercentChange, y=CompanyYearly,
		           color=Period, shape=Period)) +
		  facet_grid(. ~ Period)  +  # p243
		  geom_point(aes(size=4)) +
		  ylab("")+xlab("\nPercent Change")+
		  ggtitle("30 Dow Jones Companies: Jan. 28, 2012\n")+
		  scale_colour_manual(values=c(rgb(1, 0.5, 0),"blue"))+
		  hwTheme+
		  theme(legend.position="none") # Removes the legend:p226 