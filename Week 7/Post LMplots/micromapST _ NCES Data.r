               Preparing a US State Data to produce Micromaps
                        With example Linked Micromaps
                        

Due:  Plots from 4, 5 and 6
      Similar plots using different data from the NCES.
      Use bars instead of dots in the plain dot plot example. 
      See the file Access Dept of Education Data 2014.pdf
        

1. Introduction
 
MicromapST requires that the input data.frame have
all 50 states and the District of Columbia present.

The row.names must have appropriate State ids
The State ids can full names, the two letter postal codes or the two digit fips codes.

The NCES data files have state names in column and non state rows such 
as the National Public and DoDEA.  

The function defined below will take a data.frame with NCES data
as the first argument and the state id column name (in quotes) or
number as the second argument return data.frame with 
1) state postal codes for row.names, 
2) extra rows removed and
3) State Postal Codes as row.names.  

If the second argument is not provided the function assumes the row.names
are the full state names to be converted to abbreviations. 

The postal codes row.names are the default setting for input to 
micromapST. These prepared data.frames can be written to .csv
files and used with CCmaps and TCmaps.   

2. The conversion function  

## Run

microFull2Ab <- function(stateDF,stateId=NULL,
  ref=stateNamesFips){
  if(is.null(stateId)) nam <- row.names(stateDF) else
     nam <- stateDF[,stateId]
  nam <- ifelse(nam=="District of Columbia","D.C.",nam)
  check <-  match(nam,row.names(ref)) 
  bad <- is.na(check)
  good <- !bad
  nbad <- sum(bad)
  if(nbad>0){
    warning(paste(nbad,"Unmatch Names Removed",nam[bad])) 
    stateDF <- stateDF[!bad,]
    nam <- nam[!bad]
    check <- check[!bad]
    good <- good[!bad]
  }
  ngood <- sum(good)
  if(ngood < 51)warning(paste("Only",ngood,"State Ids"))
  row.names(stateDF) <- ref[check,2]
  return(stateDF)
}

## End


3. An example============================= 

## Run

library(micromapST)

# Read .csv file

tmp <- read.csv(file="Math2013Grade8GenderFixed.csv",
       header=T, as.is=TRUE)
head(tmp)

# Prepare the needed data.frame

mathG8Gender13 <- microFull2Ab(tmp,"State")
head(mathG8Gender13)

## End

R modified the last label Male - Female.
We could fix this but won't because we don't
need to in the examples below.  

4. A quick look at micromapST dot plots================

We have the state data.frame. We need to
make a panel description data.frame and then
call the micromapST() function. 

4.1 Making a panel description data.frame___________

This data.frame has a row for each plot column. 
The row labeled "type" indicates the type
of column such as  
map, id, dot plot, bar plot, and arrow plot,
and so on.  

Other rows indicate additional information used
in column construction such as labels and
state data.frame variable based on their
column indices.  

Starting with a list state data.frame variables
and their indices is often help in the process.

## Run

nam <- 1:ncol(stateDF)
names(nam) <- colnames(stateDF)
nam

panelDesc <- data.frame(
type=c('map','id','dot','dot','dot','dot'),
lab1=rep("",6),
lab2=c('' ,'','All','Male','Female','Male - Female'),
lab3=c('','','Possible 0-500','Possible 0-500',
     'Possible 0-500',''),
col1 = c(NA,NA,7,10,13,16))

t(panelDesc)

## End

The transpose of the data.frame appears as all
character strings which is the lowest common
representation.  The structure corresponds to the
column in the micromap.  The first column
will have micromaps.  The next column will have
state names or abbreviations.  The next three columns are
dot plot columns.  

The plot almost has to have state ids to be meaningful.  
Otherwise the column content and order is
up the analyst.  However note that many people prefer to 
see the states ids on the left.  Also be aware
that no map is required.  It is possible that two columns of maps
with different accumulation styles would be useful.   

A GUI could let us select the column type from a list of 
options with brief explanations or suggestive pictures.   

With the column type and the state data frame in hand, a 
GUI could provide description entry options specific to type
of column.  Hand specification of the many  "" and NA place
holder entries shown about would not be needed.

Some column types required the specification the variables to use. 
For example a dot plot with confidence interval needs variables for
the estimate, lower and upper bounds respectively. The GUI
could support direct selection the variable names from the state
data frame list rather specification of the column number as done
above.
  
I hope that commenting on making it better help to clarify the current
micromap specification process.  


4.2  The micrommapST() function________________________ 

The first two arguments in order are the
state data.frame and the panel description
data.frame. The other argument are 
pretty self explanatory
     
## Run

fName = "MathG8Gender2013 Dots.pdf"
pdf(file=fName,width=7.5,height=10)
micromapST(mathG8Gender13, panelDesc,
  sortVar=7,ascend=FALSE,
  title=c("NAEP Math Grade 8 in 2013",
          "Average Scale Scores"))

dev.off()

## End

The pdf file will appear in the working directory.  


5. Cumulative Maps, Arrows and Reference Values===== 

There different types of maps that we can specify:
map, mapcum, maptail and mapmedian. 

The "map" version typically does the least to 
reveal spatial patterns but is likely the easiest to 
understand at first encounter.  

The cumulative highlighted maps, "mapcum" is natural version to
introduce next. This provides trail of where we have visit
variable sort path view of the states.  

The maptail option accumulates from the beginning and end
into the middle.  Accumulation pops more and more states
into the foreground. The panels can look very busy when
more than half of the states are in the foreground.  While
this involves a change in starting point and direction so is 
less intuitive, more experienced views may appreciate view
with a simple appearance. 

The "mapmedian"  simply highlight states above and below the
median for the sorting variables.   

Arrows provide a good way to show two values and
their difference.  Here we use arrows to compare males and
females.  Often we use arrows in a temporal context
to convey before and after values.  The arrow ends
use  position along a scale  to encode two variables
and length to encode there difference. 
(Arrow head  are associated ad optical illusion our ability
to compared lengths accurately may be reduce.)

In terms of showing the differen an weakness is that the
resolution for showing the difference can be low. 
Using the arrows from zero for the Male - Female
greatly increases the resolution in the example below albeit
at the cost not directly provide the context of the original
values.

Sometimes there is a relationship between the difference of
two paired values and their average.  Looking a ratio of these
two transformed values can be informative, but not in this example.
The calculation is evident in the script below but I remove this
from plotting script that follows.
 
## Run

mathG8Gender13$Zero <- rep(0,nrow(mathG8Gender13))
# not helpful
# vM <- stateDF$Male
# vF <- stateDF$Female
# mathG8Gender13$RelDif <- (vM-vF)/(vM+vF)/2  

panelDesc <- data.frame(
type=c('mapcum','id','arrow','arrow'),
lab1=c('','','Female To','0 To'),
lab2=c('' ,'','Male','Male-Female'),
col1 = c(NA,NA,13,17),
col2 = c(NA,NA,10,16),
refVals=c(NA,NA,NA,0))

fName = "MathG8Gender2013 Arrows.pdf"
pdf(file=fName,width=7.5,height=10)
micromapST(mathG8Gender13,panelDesc,
  sortVar=16,ascend=FALSE,
  title=c("NAEP Math Grade 8 in 2013",
          "Average Scale Scores"))

dev.off()


6. Maptail and Dots with confidence Intervals=======

The maptail maps accumulate from the bottom up
and the top down. The general is that maps can
look busy when more than have of the area is 
in the foreground.   

The follow creates good resolution .png file.
Such files may be easier to include
in Microsoft documents than .pdf file.  
If this isn't supported on your operating
systems got back pdf.  


## Run

panelDesc <- data.frame(
type=c('maptail','id','dotconf','dotconf'),
lab1=c('','','Male Estimates ','Female Estimates'),
lab2=c('' ,'','and 95% CIs','and 95% CIs'),
col1 = c(NA,NA,10,13),
col2 = c(NA,NA,11,14),
col3 = c(NA,NA,12,15))

fName = "Math G8 Gender 2013 DotConf.png"
png(file=fName,width=7,height=10,
    units="in",res=300)
micromapST(mathG8Gender13, panelDesc,
  sortVar=13,ascend=FALSE,
  title=c("NAEP Math Grade 8 in 2013",
          "Average Scale Scores"))

dev.off()

## End


7. More information is available about micromapSt

Some is in R.  

## Run

?micromapST 

## End
 
The micromapST.pdf file is paper entitled

micromapST: Exploring and Communicating
Geospatial Patterns in U.S. State Data

It has several examples showning options.
The examples can be instructive
Table 1 on page 11 covers
all the major options now available. 

It is difficult to design in such limited
space.  There are aspect ratio and overplotting
problems.  The use of color for linking discourages
using color for other tasks that really need color
for fast visual processing. For the most part I suggest
emphasizing LM plots more like those in the book. 

  
