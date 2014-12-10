# File      Confident Class Map Examples.r
# By        Daniel B. Carr
# Copyright 2010, 2011, 2012, 2013, 2014
#           Class use permitted

setwd("C:/Users/ceharvey/Documents/Personal/school/Statistical-Graphics/Week 8/Confident Class Maps")
Sections

1.  Showing states in oneway tables (lists) and maps
1.1 Tables
1.2 Maps
1.3 Discussion

2.  Two-way crossed factors: lists and map

3.  Three-way crossed factors list and maps

4.  State comparisons versus Virginia
4.l Two-way crossed Map
4.2 Three-way crossed Map
4.3 Three-way crossed map with
    alternate color bar label
  
# Due  6 points
#
#  Section 1.2 Your preferred plot
#  Section 2  Plot and one more
#     where you have select
#     one or two of the other
#     comparison columns in the data frame
#     2 points

#  Section 3: Preferred plot from 3
#     Indicate why you preferred it
#     or if picked one but did not
#     have a preference 
#     2 points   
#
#  Section 4: one plot, you choice 

#  Additional comments welcomes on the
#  design, labels color choice, etc. 

# Data       NP_NAEP_MergedDat.csv
#            VA_NAEP_MergedDat.csv

# 0. Setup

library(micromapST) # has state boundary files
source("confidentClass1WayMap.r")
source("confidentClass2WayMap.r")
source("confidentClass3WayMap.r")
source("confidentClass3WayMapDesign2.r")
source("panelFunctions.r")

1. Showing states in oneway lists (tables) and maps 

## Run

df<- read.csv("NP_NAEP_MergedDat.csv", row.names=1)
head(df)

## End______________________

There are four categorical variables in the data set.
Each partitions the states into three classes.
 
The common approach to categorical variables focuses
attention on one variable at the time. For each variable
we can look at list of the states for each of the classes.

Here we have three classes denoted "Below", "Similar To"
and "Above" that were encoded as 1, 2, and 3 respectively.
The script below used used the text labels.    

1.1 Tables

## Run__________________  

labels <- c("Below","Similar To","Above")

tabMath4 <- tapply(rownames(df),list(df$Math09G4_Comp),c)
names(tabMath4)=labels
tabMath4

tabMath8 <- tapply(rownames(df),list(df$Math09G8_Comp),c)
names(tabMath8)=labels
tabMath8

tabRead4 <- tapply(rownames(df),list(df$Read09G4_Comp),c)
names(tabRead4)=labels
tabRead4

tabRead8 <- tapply(rownames(df),list(df$Read09G8_Comp),c)
names(tabRead8)=labels
tabRead8

## End__________________________

1.2 Maps

The list of state postal codes even if put is table
with 3 cells does not make it easy to see spatial
patterns in our minds eye. Providing the
full states names would help those not very familiar
with the postal codes. Only those also familar with
state locations might gain a rough idea about
spatial patterns from reading the names in the
the three class cells of a table.  Representing
the states in each of the three classes using
three maps provides a direct way to see spatial patterns!

## Run

windows(width=7.5, height=3)
confidentClass1WayMap(df,
  var=1,
  colLab="Mathematics Grade 4",
  colorFill=rgb(.73,.48,1),
  title="2009 State Average NAEP Scores As Compared to National Public")

windows(width=7.5, height=3)
confidentClass1WayMap(df,
  var=3,
  colLab="Mathematics Grade 8",
  colorFill=rgb(.73,.48,1),
  title="2009 State Average NAEP Scores As Compared to National Public")

## End_________________

# 1.3 Discusion

In addition to looking for patterns in individual plots
we can juxtapose the two plots one above the other
and look for differences in the corresponding panels. 
Juxtapose the two windows on your computer.   

The relatively empty Similar To Grade 8 panel indicates more
separation in state averages from the National Public
average than for 4th grade students.  For 8th graders in 2009
the averages for several more states, including Texas,
were above the National Public average. Two states,
Connecticut and Florida were added to the
8th Grade Below map. The 4th grade average for Florida was
in the Above class. There may be an interesting story here
about way the two student populations from the same state
performed so differently.          

We can learn from such margin plots, but they can be far
from the whole story and can be misleading at times.  

Long ago, experience and advances in experimental 
design led researchers away from one variable at a time research.
Now many scientific studies involve two or as sometimes categorical
factors are crossed or partly crossed in factional factorial designs.
 
Unfortunate the visual analytics in popular press
continue emphasize one variable at time thinking and
avoid mading efforts to educate the audience in more
sophisticated ways of thinking.  Software such as Tableau
can provide sophisticated web graphs, so change may be
in progress.  Still, I look at books such as Information
Dashboard Design (2006) by Stephew Few and see mostly one-way
graphics.  Line charts are typical one-factor indexed times series.
Pie chart times series are also one-factor indexed time series. 

A familar two-way crossed graphic is the 3-D perspective two-way barchart.
This typicaly shows one value for each cell in the two-way
cross design.  Design issues include occlusion of bars and
difficulty in reading values against a scale in the
3-D perspective setting.  
 
2.  Two-way crossed factors: lists and maps

The results of tapply will print nicely
when the function returns a single value for
each cell or cell vectors are all the same
length so the storaage can be in a array.    

For the variable length state abbreviations
the results are stored but the printing
only indicates the type and length of
of the cell vector when it is longer the
length 1.  We can extract the vector values
from the 3 x 3 table cells with a little work.    

## Run

tabMath4Math8 <- tapply(rownames(df),
  list(Grade4=df$Math09G4_Comp,
       Grade8=df$Math09G8_Comp),c)
rownames(tabMath4Math8)=labels
colnames(tabMath4Math8)=labels
tabMath4Math8

tabMath4Math8[[1]]  #Grade4=Below      Grade8=Below
tabMath4Math8[[2]]  #Grade4=Similar To Grade8=Below
tabMath4Math8[[3]]  #Grade4=Above      Grade8=Below

tabMath4Math8[[4]]  #Grade4=Below      Grade8=Similar To
tabMath4Math8[[5]]  #Grade4=Similar To Grade8=Similar To
tabMath4Math8[[6]]  #Grade4=Above      Grade8=Similar To

tabMath4Math8[[7]]  #Grade4=Below      Grade8=Above
tabMath4Math8[[8]]  #Grade4=Similar To Grade8=Above
tabMath4Math8[[9]]  #Grade4=Above      Grade8=Above

## End

Instead we just sort the data and fix the labeling.
This generalizes to higher way cross product tables and
nicely omits the empty cells.   

## Run

ord <- order(df[,1],df[,3])
tmp <- df[ord,c(1,3)]
colnames(tmp)<- c("MG4","MG8")
tmp$MG4 = factor(labels[tmp[,1]],levels=labels)
tmp$MG8 = factor(labels[tmp[,2]],levels=labels)
tmp

## End

To plot the state postal codes in two way table
with variables length rows is not hard using
my panelLayout functions. Page 93 in the
Visualizing Data Pattern with Micromaps 
shows an example.  A two-way map is
on then next page.

## Run

windows(width=7.5, height=6)
confidentClass2WayMap(df,
  rowVar=5,colVar=1,
  rowLab="Reading Grade 4",
  colLab="Mathematics Grade 4",
  backFill=rgb(1,1,.85),
  colorFill=rgb(.7,.35,1),
  title="2009 State Average NAEP Scores As Compared to National Public")

## End

3. Three-way crossed factors list and maps

We still have not used color to represent a variable
We could use color to encode the state average score
in the a oneway map.

The two averages for each state in the two way table to
not lend themselves to color encoding.  However, we can
use color to encode state class membership for third variable.
Below the choice is the 8th Grade reading score for the same
year. This is the basically the same student population than
took the 8th grade mathematics test. The 8th graders are
obviously both are distinct from those taking the 4th grade
tests.  The 3-way comparisons are not as nicely similar as
one may like but they are valid state descriptors and
informative.  

With 4 tests in the data file there are 4
different three-way comparisons we could pick.  Taking the
next step up to a 3 x 3 x 3 x 3 = 81 cell design gets
a bit cognitive taking.  The 3 x 3 x 3= 27 cell design
works pretty well when we use color for on factor because
it collapse to a 3 x 3 layout with color.    

## Run
   
ord <- order(df[,1],df[,3],df[,7])
tmp <- df[ord,c(1,3)]
colnames(tmp)<- c("MG4","MG8")
tmp$MG4 = factor(labels[tmp[,1]],levels=labels)
tmp$MG8 = factor(labels[tmp[,2]],levels=labels)
tmp$RG9 = factor(labels[tmp[,2]],levels=labels)
tmp

## End 

windows(width=7.5, height=6)
confidentClass3WayMap(df,
   rowVar=3,colVar=1,colorVar=7,
   rowLab="Mathematics Grade 8",
   colLab="Mathematics Grade 4",
   colorLab="Reading Grade 8",
   title="2009 State Average NAEP Scores As Compared to National Public")

windows(width=7.5, height=6)
confidentClass3WayMapDesign2(df,
   rowVar=3,colVar=1,colorVar=7,
   rowLab="Mathematics Grade 8",
   colLab="Mathematics Grade 4",
   colorLab="Reading Grade 8:",
   title="2009 State Average NAEP Scores As Compared to National Public")
 
# 4. State comparisons versus Virginia

# Read file
dFrame<- read.csv("VA_NAEP_MergedDat.csv",row.names=1)
head(dFrame)

# 4.1 Two-way Map

# Set refRegion="VA"
windows(width=7.5, height=6)
confidentClass2WayMap(dFrame,refRegion="VA",
  rowVar=3,colVar=1,
  rowLab="Mathematics Grade 8",
  colLab="Mathematics Grade 4",
  colorFill=rgb(0,.6,1),
  backFill=gray(.86),
  backLine=gray(1),
  title="2009 State Average NAEP Scores As Compared to Virginia")

# 4.2 Three-way List of States and Map

windows(width=7.5, height=6)
confidentClass3WayMap(dFrame,refRegion="VA",
  rowVar=3,colVar=1,colorVar=7,
  rowLab="Mathematics Grade 8",
  colLab="Mathematics Grade 4",
  colorLab="Reading Grade 8",
  title="2009 State Average NAEP Scores As Compared to Virginia")

# 4.3 Three-way Map: Alternative Color Bar Label

windows(width=7.5, height=6)
confidentClass3WayMapDesign2(dFrame,refRegion="VA",
  rowVar=3,colVar=1,colorVar=7,
  rowLab="Mathematics Grade 8",
  colLab="Mathematics Grade 4",
  colorLab="Reading Grade 8",
  title="2009 State Average NAEP Scores As Compared to Virginia")

