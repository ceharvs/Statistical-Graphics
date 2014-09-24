                      Designs for Two Factors 

By        Daniel B. Carr
          First draft September 21, 2013
          Last revised Aug 25, 2014

Plot     Brigid Schulte, 2013.  "Recession had further polarized families"
Source:  Washington Post, Sept. 11, 2013.
         The article cited increased divides based on race, class
         and education
           
Original Qian, Zhenchao. "Divergent Paths of American Families", 
Source:  Department of Sociology at Ohio State University, Sept. 11, 2013 
         The PDF file obtained from the web is in class folder.  
         The newpaper article focused the US born part of Table 7 in the
         pdf file and combined to additional family types into the
         "Other" family type.    

         The two factors in the plot are race with 4 levels and
         family type with 6 levels. An example of a family type is
         "Married, both working".  There are 24 values percents than 
         correspond to the product set of 4 races and 6 family types.
         The family type percents are percent of the total for each
         race.  The percent totals for each race turn out to be 99 or 101
         due to rounding to integers. The values in Table 7 of the source
         were rounded.  They will work well enough for our redesign 
         purposes. 

         It is nice to see a two factor plot in the Washington Post. 
         For each race the Post graph design uses 6 "bubbles" arranged
         in a circle to show the family type percents.  Their design
         shows the 24 values in the bubbles so serves as a both 
         spread out table of numbers and a plot. It is not as
         convenient as a table of number for reading and
         comparing numbers.  While the original plot example may have
         higher entertainment appeal, it is not as good as our redesigns
         below in terms of simple appearance that helps us notice visual
         patterns.  We will use dot and bar plots that have encoding
         higher perceptual accuracy of extraction than bubble area plot. 
         We simply appear by rearranging some similar races and similar
         family types appear close together.  
  
         Another  bubble plot weakness is the use of color links to the
         legend for the 6 family types. It is not so easy to keep 6 pairs
         of colors (3 hues by 2 shades) and labels in mind. When focusing
         on different bubbles in the plot.  

         We go a little further with designs in some cases by showing
         reference values and could take the next step and explicitly
         show the differencs from common the common reference value.  

	   With more than three items there are fewer comparisons if we
         compare each item to a reference value that if we compare
         all pairs to each other.  

	   While there is some disaggreement, I advocated smaller
         perceptual groups by context subset of point with lines
         See the article by Carr and Sun in the folder.   

Old assignment:  Skip the year 
Due      Plots from 3.1 and 3.2 (2 points)  
         5 and 6 (2 points 
         7 one plot, your choice (1 points)
         8 dot plot (1 point)
         8.2 dot plot (1 point)
         8.4 dot plot and your R script (2 points)

         Comment: Which do you prefer in the context of two
         factor plots, dot plots or bar plots. Briefly provide
         your opinion. (1 point)
         
         10 points total       
                 
Sections 1.  Entering data
         2.  Dot plots with lines the origin 
             The second reorders row and columns
             to match Washington Post order
         3.  Simplifying appearance to help reveal patterns
         3.1 Reordering rows
         3.2 Reordering columns to change panel order 
         3.3 Comments on perceptual group to further
             simply appearance
         3.4 More comments on sorting and apppearance        
         4.  Race comparisons for each family type

         5.  Barcharts instead of dot plot
         6.  Adding a reference values using line
         7.  Alternative layouts
         7.1 A two column layout 
         7.2 A three column layout

         8. Using factors in lattice and gglot2 to
            produce a four column graph analogous to
            the Washington Post design 
         8.1 Some patterns appear  
         8.2 Creating two perceptual groups with three dots 
         8.3 Appearance comments
         8.4 Creating three groups of two

0. Setup

Install package ggplot2 and its dependencies.  
      
1. Entering Data 

When entering data by hand, use a convenient organization.
After reading the data into R.  Transform the data
structure as needed to produce the graph. 

I used R's c()function to create a vector with the 24
values. I entered the 24 values as arguments to c().  
Function arguments are separated by commas. 

A systematic approach helps when it comes to correctly labeling
the values. When using the graph from the Washington Post I
chose to focused locally on each race with its six bubbles
arranged in a circle. I went around the circle clockwise
and wrote down the six family type percents written in the
bubbles.  The left most race was white. I wrote the six white 
family type values in the first line of the R script c()
function line as shown below.  The next line has six values for
black on the second line and so on.  R continues to read lines
until it encounters the closing parenthesis for a function. The
spacing and alignment of numbers accross was for my convenience.
R will strip out the white space,     
    
Note many people prefer to enter data in Excel and read such files
into R.       

The script then restructures the vector dat of length
24 into a matrix with 6 rows and hence 4 rows. By
default matrix(data=dat, nrow=6) will use the first 6 elements of vector
dat as the row values in the first column, the next 6 elements as the
row values of the second columne ans so on for subsequent columns.  

The script then creates a vector of simplified names for the family
type, a vector of names for the races, and then uses these
as row and column names for the matrix mat.

## Run

dat<- c(22, 50,  8,  2, 13,  6, 
         5, 24, 13, 24, 20, 15,
        21, 33,  9,  7, 20,  9,
        24, 53,  4,  1, 12,  5)

mat <- matrix(data=dat, nrow=6)

type <- c( "Married, Father Working", "Married, Both Working",
           "Divorced Mother","Never-married Mother",
          "Other", "Grandparents")
race <- c("White","Black","Hispanic","Asian")
rownames(mat) <- type
colnames(mat) <- race
mat

## End______________________________

2.  Dot plots with lines to the origin.
    The second plot reorders row and columns
    to match Washington Post order


## Run: First plots

library(lattice)
#png(width=4.5,height=6,units="in",res=300,
#  file="Family_Dotplot1.png")
windows(w=4.5,h=6)

dotplot(mat,groups=FALSE,
  layout=c(1,4),aspect=.7,
  origin=0,type=c("p","h"),
  main="Who is Raising the Children?",
  xlab="Rounded Percents\nRace Totals Close To 100",
  scales=list(x=list(tck=0, alternating=FALSE)),
  panel=function(...){
    panel.fill(rgb(.9,.9,.9))
    panel.grid(h=0,v=-1,col="white",lwd=2)
    panel.dotplot(col=rgb(0,.5,1),cex=.9,...)
  }
)
if(names(dev.cur())=="png")dev.off()

## End

When the lattice dotplot has a matrix as input, the row names
become row labels and the column names become panel labels. 
R also associates counting integers with the row and column names
and uses the number follow the graph convention opposed to 
a table reading convention.  That is, small value appear near the
bottom of the y-axis and large values appear near the top.

The result is that Married, Father Working, is row 1 in mat,
so plots at the bottom of family type list.  Similarly White is
column 1 mat and so appears at bottom of the vertical stack of panels.

We can use indices (subscript) to rearrange matrix rows and columns.
To more comfortably compare the R plot with the article plot, I chose
to reverse the rows and columns using subscripts.

## Run: Second plot with reorder matrix input   

# Save a label to use in plots below
xlab <- "Rounded Percents\n Race Totals Close To 100"

windows(w=4.5,h=6)
reOrderMat <- mat[6:1,4:1]  # reverse the rows and columns 
dotplot( reOrderMat, groups=FALSE,
  layout=c(1,4),aspect=.7,
  origin=0,type=c("p","h"),
  main="Who is Raising the Children?",
  xlab=xlab,
  scales=list(x=list(tck=0, alternating=FALSE)),
  panel=function(...){
    panel.fill(rgb(.9,.9,.9))
    panel.grid(h=0,v=-1,col="white",lwd=2)
    panel.dotplot(col=rgb(0,.5,1),cex=1.1,...)
  }
)

## End   

We can scan up and down the panels comparing all values
against the common scale at the bottom.  The grid
lines make it easy compare the values against the common
scale.  We can see two largest percents are  White and Asian
familty topes of Married, Both Working.  The Asian dot is a little
to the right 50 percent grid line so we can tell it is larger
value than the White dot which is on the grid line. A similar
order for White and Asian appears for Married, Father Working.  
The whole pattern of dot for White and Asia appears similar.

3. Simplifying appearance and revealing patterns

Our eyes jump around a lot when we look at plots. 
However suppose we could look from dot to dot as we work our
way down the rows within each panel.  Consider the dot to dot
traversal lengths that our eyes would follow.  
We could sum the lengths for each panel. I think such sums provide
a rough indicator of panel appearance complexity and the
sum of the panel sums provides an indicator of plot complexity.  

Side note:  As an alternate measure that does not depend on the
height of the  column of panels we could sum the absolute difference
in the x  coordinates from row to row.     
 
We can simplify appearance by rearranging rows to reduce
the complexity indices suggested above!  More generally arranging
plot elements so similar elements appear close together tends
to simplify appearance.  

3.1 Reordering the Rows of a Matrix

We can reorder the rows of a matrix to make the
values in a Single column to be in increasing
or decreasing order.  Typically other columns
will not also be in increasing or decreasing order.  

As a compromise was can compute a value
for each row and sort rows to be in 
increasing or decreasing oder  based on thes
values. 
 
Here we compute the average of 
column (race) values for each row.  
In general the data can influence our choice. 
If there are outliers the median is likely
a better choice as Cleveland has suggested.    

The task can influence our choice of sorting
criterion. We might be particularly
interested in comparing the maximum values
so use the maximum across the columns as the
basis of sorting.   
  
There are more computationally involved
computed variables than we can use as a sorting variable.
A reasonable choice for multivariate data is often
the first principal component because order the
rows in the direction of maximum variation.  

Some algorithms such as minimal spanning tree
breadth traversal provide the ordering directly.
Unfortunately the traversal algorithm is in S-Plus
but not in R.  

## Run: Sorting based on Row means

windows(w=4.5,h=6)
means <- rowMeans(mat)  # Find row means is 

typeOrder <- order(means) 
 
dotplot(mat[typeOrder,4:1], groups=FALSE,
  layout=c(1,4),aspect=.7,
  origin=0,type=c("p","h"),
  main="Who is Raising the Children?",
  xlab=xlab,
  scales=list(x=list(tck=0, alternating=FALSE)),
  panel=function(...){
    panel.fill(rgb(.9,.9,.9))
    panel.grid(h=0,v=-1,col="white",lwd=2)
    panel.dotplot(col=rgb(0,.5,1),cex=1.1,...)
  }
)

## End

Now the Married, Both Working type with which tends to
have the largest value across races appear at the 
top of each panel.  The pattern of dots in the panels
look much simpler, especially in White and Asian panels. 

There are communication settings when a particular order
of row can be important that providing simple appearance.
Sometimes the  the communication goal is to obscure the
pattern but not in this class.

3.2 Reordering columns to change panel order

Do not stop redesigning immediately after the first
improvement.  Consider taking next steps.  

A quick glance at the plot suggests we can simplify appearance
by putting White and Asian panels (or facets in ggplot2) by each other. 
Is there a way to compute the panel order?  Yes.   

As first thought we might consider computing the
average across rows of family type values to
obtain the sorting variable to use in reordering
the columns.   

However, without rounding the family type percents
would total 100. The averages divide "100" by 6 and
so should all be the same.  

Another approach uses a powerful tool called multidimensional
scaling.  Multidimensional scaling (MDS) provide plotting
coordinates for cases with multiple variables.  When we 
think a matrix in R in a cases by variables context the
row of the matrix are cases and the columns are variables.  
We will use the transpose function t() function to transpose
our matrix.   

## Run

raceCase <- t(mat)
raceCase

## End
   
Multidimensional scaling is based on approximating the distance
matrix between all pairs of case.  The R function to compute
distance matrix between is dist().  The variable values
need to be in comparable units for this to make sense.  Our
values are percents so this is okay.   

## Run

raceDist <- dist(raceCase)
raceDist
## End

The lower triangle of the distance matrix shown in the 
R Console suffices since the matrix is symmetric and
the diagonal is 0. Reading rows  can see there is a
big distance between Black and White but the biggest
distance is between Asian and Black.  
  
Side note:_________________________
  
Computing the euclidean distance for Black and White
 
If there were only two variables this computation would be
like computing the hypotenuse length of a right triangle.
the difference the two variable for Black and White would
have two values. The length of one side of the triangle would
be the absolute value of the difference for the first variable.
The length for the second side of the triangle would be the 
absolute value of the difference for the second value. 
Here we have six differences that we square, sum and then take the
square root get a distance (or length).  

## Run compute the Euclidean distance between White and Black

mat
WBdif <- mat[,"White"]-mat[,"Black"]
WBdist <- sqrt( sum(WBdif**2))
WBdist

## End

End Side note______________________________

Classical multidimensional scaling finds plotting coordinates
for points in lower dimensions than the number of variables used
to produce the case distance matrix.  We can supply the 
dimension for which we want points. We want to order the cases
(races) so choose dimension k=1.   

The distance matrix for the provided coordinates in a given dimension
provides the best approximation case distance matrix for that
dimension based on the Frobenius norm. (Basically this norm sums the squared
discrepancies.)  

Except for degenerate situations there will be
discrepancies between the coordinate distance matrix and the 
case distance matrix.  We cannot squash a wire frame triangle (2D) or
tetrahedron (3d) onto a line without altering their shapes.  

With the points on a line we can use the order()
to find the subscripts to put the races in a preferred
order.   

## Run

pointsOnLine <- cmdscale(raceDist,k=1)
pointsOnLine
raceOrder <- order(pointsOnLine)
matFixed <- mat[typeOrder,raceOrder]

windows(w=4.5,h=6)
dotplot(matFixed, groups=FALSE,
  layout=c(1,4),aspect=.7,
  origin=0,type=c("p","h"),
  main="Who is Raising the Children?",
  xlab=xlab,
  scales=list(x=list(tck=0, alternating=FALSE)),
  panel=function(...){
    panel.fill(rgb(.9,.9,.9))
    panel.grid(h=0,v=-1,col="white",lwd=2)
    panel.dotplot(col=rgb(0,.5,1),cex=1.1,...)
  }
)

## End______________________________________

This look simpler than the previous ordering.  

4. Race Comparisons for each family type 

We transpose the matFixed data
matrix from above using t()
so rows are races. We change the layout
to have 1 column with 6 rows of panels.

## Run

matTranspose <- t( matFixed )

windows(w=4,h=6.8)

dotplot(matTranspose, groups=FALSE,
  layout=c(1,6), lwd=4,
  origin=0,type=c("p","h"),
  main="Who is Raising the Children?",
  xlab=xlab,
  scales=list(x=list(tck=0, alternating=FALSE)),
  panel=function(...){
    panel.fill(rgb(.9,.9,.9))
    panel.grid(h=0,v=-1,col="white",lwd=2)
    panel.dotplot(col=rgb(0,.5,1),cex=1.1,...)
  }
)

## End ____________________

The gaps in the grid lines at integer y values are not intended.
This seems to be a flaw. We can avoid the flaws using bars.     

5. Bar plots

We can produce a barchart by changing
the script "dotplot" to "barchart" in 
both places it occurs, and by commenting
out or removing script line that starts
with origin = 0 and specifies points
and a horizontal line.

## Run

windows(w=4,h=6.8)
barchart(matTranspose,groups=FALSE,
  layout=c(1,6), xlim=c(0,55),
#  origin=0,type=c("p","h"),
  main="Who is Raising the Children?",
  xlab=xlab,
  scales=list(x=list(tck=0, alternating=FALSE)),
  panel=function(...){   
    panel.fill(rgb(.9,.9,.9))
    panel.grid(h=0,v=-1,col="white",lwd=2)
    panel.barchart(col=rgb(0,.5,1),cex=1.1,...)
  }
)

## End ____________________________ 
  
6. Adding a reference line for the
   average percent for each family type

To be correct the percents should be
based on the national population. Perhaps
this in the paper cited above.     

If we had the number of families
for each race (and the correct time period)
we could compute a population weighted 
average percent for each family type.
We could add this to panels to provide
comparisons.  We could compute the differences
of the race values from the average
for each family type and show it
explicitly.

Below we obtain Naive averages and show
them as red reference lines
in the panels.  We plot the bars
on top of  tje reference lines to avoid
possibly hiding the end of the bars
and indicate in information priority.
In layering informmation we normally
put the most important information
in front (on top).
  
We should add a legend to explain
the red lines, but we won't do it here.

## Run

matTranspose
typeMean <- colMeans(matTranspose)
typeMean

windows(w=4,h=6.8)
barchart(matTranspose,groups=FALSE,
  layout=c(1,6),xlim=c(0,55),
  main="Who is Raising the Children?",
  xlab=xlab,
  scales=list(x=list(tck=0, alternating=FALSE)),
  typeMean=typeMean,
  panel=function(...){   
    panel.fill(rgb(.9,.9,.9))
    panel.grid(h=0,v=-1,col="white",lwd=2)
    i <- panel.number()
    panel.abline(v=typeMean[i],col="red",lwd=3)
    panel.barchart(col=rgb(0,.5,1),cex=1.1,...)
  }
)

## End ____________________________ 

Side note:  I added the vector typeMean as a
barchart argument above. 
In the lattice package this gets passed
through to the panel function so 
abline can find it. The panel number
tells which mean in the vector to use.
End Side note________________________________ 

7. Alternative layouts    

The vertical layout is nice from the
perspective of having one
horizontal scale that serves for
all of the panels.   

A drawback is that there is a label
on the left for every single value.  
The goals of showing less text, space constraints and other
considerations may motivate using different layout.  

We can try different panel layouts,
and change the panel order. It can
take some thinking to get the panels
ordered in the way we think
people will read. People
vary so we won't be right for 
all people. There are ways
to encourage people do read in
the intended direction.    

7.1  A two column layout 

Here I was hoping people would read down
the first column and then down the
second column when ordering the panels
below. Some may read 
horizontally and the strip labels
made foster this.  

People might be more inclined to read
down if there was a gap between the
two columns of panels similar to 
a two column text layout.    

Side note: Observe the use of subscripts
to order the columns of the table and
the used of panel.number()
indices to pick the right element
of the typeMean vector.
 
We may also want to adjust the window size,
font size and even alter the text
to make the plot look better.

## Run
windows(w=6.8,h=3.8)
ord <- c(4,1,5,2,6,3)
barchart(matTranspose[,ord], groups=FALSE,
  layout=c(2,3),xlim=c(0,55),
  main="Who is Raising the Children?",
  xlab=xlab,
  scales=list(x=list(tck=0, alternating=FALSE)),
  typeMean=typeMean,
  panel=function(...){   
    panel.fill(rgb(.9,.9,.9))
    panel.grid(h=0,v=-1,col="white",lwd=2)
    i <- ord[panel.number()]
    panel.abline(v=typeMean[i],col="red",lwd=3)
    panel.barchart(col=rgb(0,.5,1),cex=.95,...)
  }
)

## End

Note that the column of panels 
on the right has a lot of wasted space.  
There are options in lattice and ggplot
to allow the x-axis range to vary and to 
allow the panel width vary in accordance with the
actual x-axis range.  This will be addressed
in a later class.

7.2 A three column layout 

Here the goal is read across
the rows.  The challenge is to figure out
the subscript to put the matrix
columns in the desired order
and produce the plot. 

## Run
windows(w=6.8,h=3.1)
ord <- c(3,2,1,6,5,4)
barchart(matTranspose[,ord],groups=FALSE,
  layout=c(3,2),xlim=c(0,55),
  main="Who is Raising the Children?",
  xlab=xlab,
  scales=list(x=list(tck=0, alternating=FALSE)),
  typeMean= typeMean,
  panel=function(...){   
    panel.fill(rgb(.9,.9,.9))
    panel.grid(h=0,v=-1,col="white",lwd=2)
    i <- ord[panel.number()]
    panel.abline(v=typeMean[i],col="red",lwd=3)
    panel.barchart(col=rgb(0,.5,1),cex=.95,...)
  }
)

## End ____________________________ 

A further step could show deviations from the mean
across races using arrows from the mean of race percents
to the individaul race percents.  This would focus on the range
of the data.   
  
8. Using factors in lattice and gglot2 to produce 
   a four column graph analogous to the Washington Post
   design. 

The matrix ordering approach to reorder the plots
seem easier to me because used this for
many row-labeled plots examples. However
many graphics in lattice and ggplot make use of factors,
so examples can be helpful.  

The script below builds a data.frame called family with 
three columns: a percent column and two factor columns
called type and race.  The basic data entry is repeated
from above. The script below put it together differently
and incorporates the desired order for levels based on
examples above. This is followed by a lattice bar plot
and ggplot2 dot plot.    
     
## Run

dat<- c(22, 50,  8,  2, 13,  6, 
         5, 24, 13, 24, 20, 15,
        21, 33,  9,  7, 20,  9,
        24, 53,  4,  1, 12,  5)


type <- c( "Married, Father Working", "Married, Both Working",
           "Divorced Mother","Never-married Mother",
          "Other", "Grandparents")
race <- c("White","Black","Hispanic","Asian")
rownames(mat) <- type
colnames(mat) <- race
mat

famType <- factor(rep(type,4),levels=type[c(3,4,6,5,1,2)])
race <- factor(rep(race,c(6,6,6,6)),levels=race[c(4,1,3,2)])

family <- data.frame(percent=dat,type=famType,race=race)

xlab2 <- "Rounded Percents of Total Count for Each Race"

windows(w=7,h=3)
barchart(type~percent | race, layout=c(4,1),data = family,
   main="Who's Raising the Children?",
   xlab = xlab2,
   panel=function(...){
   panel.fill(rgb(.9,.9,.9))
   panel.grid(h=0,v=-1,col="white")
   panel.barchart(col=rgb(0,.2,1),cex=1.1,...)}
) 

library(ggplot2)

windows(w=7.5,h=3)
qplot(x=percent,y=type,facets=. ~ race, 
  fill=I("blue"), shape=I(21), size=I(2.7), data=family,
  main="Who's Raising the Children?",
  xlab=  paste("\n",xlab2,sep=''),
  ylab ="Type of Family\n") + guides(color=FALSE)+
#  geom_path()+
  theme(
    strip.background=element_rect(fill="lightgreen",colour=gray(.5),
      size=1), 
    panel.border=element_rect(fill=FALSE,colour=gray(.50)),
    axis.text=element_text(colour="black"))

## End 

# put the cases in family type index order 
# to draw paths connecting points

ord <- order(family$type)
familyOrd <- family[ord,]

windows(w=7.5,h=3)
qplot(x=percent,y=type,group=race, facets=. ~ race, 
  fill=I("blue"), shape=I(21), size=I(2.7), data=familyOrd,
  main="Who's Raising the Children?",
  xlab=  paste("\n",xlab2,sep=''),
  ylab ="Type of Family\n") + guides(color=FALSE)+
  geom_path()+
  theme(
    strip.background=element_rect(fill="lightgreen",colour=gray(.5),
      size=1), 
    panel.border=element_rect(fill=FALSE,colour=gray(.50)),
    axis.text=element_text(colour="black"))

## End 

8.1  Some patterns appear  

The percent of families with working father's is 
similar for Asians and Whites, lower for Hispanics and much
lower for Blacks.  

The percent with both parents working
is consistently higher than the percent with just the father workings.
A perhaps partially true way to interpret this is that when a family has
a working mother, a working father is more likely to stay with the family.     
A higher percent of black grandparents and never married mothers
are raising children than for the other races. 
 
The comparison of two plots above
in section 8. is not fair because I did not make the
many common elements look the same and did nothing to improve
the lattice plot. For example the scale at the top of the lattice plot
makes the plot look busy and is easy shift to the other sided of the
panel. When it comes to details I want the class
to learn the most about ggplot2 so show "improving" a couple of things.
Above I show how to put dark gray lines around the facets and how
to make tick labels black.  We could also make all the tick marks
go away and are so motivated since there are grid lines. However
I but can't move the tick labels closer to the faces for better
proximity grouping with the grid lines. For the moment I leave the ticks
alone even though they are prickly.
     
I usually prefer dot plots to bar plots.  

8.2 Creating two perceptual groups with dots each.

I conjecture that if we connect small group of dots with lines
to create perceptual groups of two or three, we could remember
the a shape long enough to compare it against another 
shapes.  Comparison would be faster because there would be fewer groups to 
compare than dots to compare. 

When creating factor for our perceptual groups we want
the family types for the first three rows to be in one
perceptual group.  The first three types are "Married,
Both Working", "Married Father Working" and "Other".
We want the other family types to in the second perceptual group
and are not concerned about the names for the levels. One
way to partition the family types is the use the 
numeric values of the famType factors.  

## Run

with(family,levels(famType))
with(family,as.numeric(famType))
 
## End 

We want "cases"  with values 1, 2, and 3 three in
one perceptual group and cases with 4, 5 and 6 in 
the other perceptual group.  

We can used cut() to do this with breaks = c(.5, 3.5, 6.5).
Values 1, 2 and 3 will be in the interval .5 and 3.5 
so belong to first category and be recoded as 1.  
Values 4, 5, and 6 will be in the interval 3.5 to 6.5 
so belong in the second category and be recoded as 2.  

If we used breaks = c(.5, 2.5, 4.5, 6.5) we could
partion the family times into three perceptual groups.

## Run

breaks <- c(.5, 3.5, 6.5)
index <- with(family,
  cut(as.numeric(famType),breaks=breaks))
grp <- c("grp1", "grp2")
perGroup <- factor(grp[index],levels=grp)
family$perGroup <- perGroup

# put the cases in family type index order 
# to draw paths connect the perceptual groups
ord <- order(family$type)
familyOrd <- family[ord,]

windows(w=6.5,h=3)
qplot(x=percent,y=type,facets=. ~ race, group=perGroup, 
  fill=I("blue"), shape=I(21), size=I(2.7), data=familyOrd,
  main="Who's Raising the Children?",
  xlab=  paste("\n",xlab2,sep=''),
  ylab ="Type of Family\n") + guides(color=FALSE)+
  theme(
    strip.background=element_rect(fill="lightgreen",colour=gray(.5),
      size=1), 
    panel.border=element_rect(fill=FALSE,colour=gray(.50)),
    axis.text=element_text(colour="black"))+
  geom_path()

## End

We see middle dots stick out in the two black groups of three.
The groups have a different shape that the corresponding
groups of three for the other races. The Black Married Father Working
percent is very low and Black Never-married Mother percent is very
high. 

Those that get into looking at plots will likely notice many of the
same things.  Still, to me it seems easier to locate and talk
about the groups of three than individual points.  Ease of
visual identification and verbal description is very important. 

We can see that the lower group of three for Hispanics is shifted
to the right relative to Asians and Whites. This sentence described
nine points.  We can more easily compare the slopes for the 
top two points in top groups of three. I can see the slope
between Grandparent and Other is larger for Hispanc even though
the line isn't there. It see easy to focus on points separated
by the only gaps.

What do you see?  What do people see?  So often people do not
engage in looking at plots. I conjecture the simple patterns
can draw people in to looking more closely.  

I talk about slopes.  It is true that the constant y separation, 
is a construction artifact that influences our preception of and the 
calculation slopes.  Nonetheless the ordering of slopes is
still meaningful and addresses comparing pair of factors to each
other.   
  
See the Carr and Sun newsletter article in the folder for a more complex
example.  That example accommodates 10 points (10 factor
levels) by connecting dots in three groups using a 3-4-3 pattern. 
I think the middle group of four works moderately well for comparison
purposes. I suspect comparison is more much difficult with groups
of five but manageable when the middle dot is distinguished from the
rest by say color.  Then it creates smaller perceptual groups.
I conjecture that the comparison difficulty increases rapidly
as the number of dots connected by a line exceeds five. 
     
A substantial portion of people do not want to connect dots.  
Some insist that this implies interpolation.  Myself, I don't
know how to interpolate between the classes of Grandparents and Other.
I think the issue is more the lack of familiarity with perceptual
grouping examples. There may well be a lack cognitive
research demonstrating confirming or denying my
conjecture efficiency. I need to search the literature.

Of course when people disagree it is natural to think
they would agree with us if they just understood. 
Fairly often dissagreements remain even when those
disaggreeing are very smart and have considered the arguments.     

8.3 Appearance comments

This lattice dot representation with a line to zero is not
standard but reasonable.  However, it may not be obvious
that the center of dot represents the value.  This
could be addressed by plotting a little white dot
on top of each blue dot.  

As indicated in the newsletter article each strip
label seem to dominate the body of it's panels
The color areas in a bar plot have "visual" weight and
can hold there own again the strip labels.  This is good
in calling attending the statistics. Still, for me bars
get too much area. Little bars can be just as important
as big bars but are easy to overlook.  
Bars hide high grid lines and iterrupt the visual flow 
when scanning across them except the sorted provide
a nice monotonic sequence. 

It is harder for me to imaging connecting centers of bar
ends to other centers fo bars. The edges get in
way of my lines.    

I can see around dots like water flowing
around rocks. At the same time connecting lines are easy
to imagine. I can see them easier that the W in the stars, and
seeing Cassiopeia's chair is pretty easy to see once I'm 
be pointed in the right direction.  Imagination takes a bit
of work. Showing the lines offloads the work and we
can know we are seeing the same lines as other people.  
We can use that energy to think about
what the pattern means.  With lines present the pattern
won't go away while we are thinking.  
   
8.4 Creating three groups of two

Modify the cut() function in 8.2 to produce 
three groups of size 2.  Add this new grouping factor
to the family data.frame. Follow the 8.2 example
to order this data.frame in family type order and
adapt the qplot script as necessary to use this
new data frame and its aesthesics to produce the plot.              

 




