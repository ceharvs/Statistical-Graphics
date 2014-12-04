File          Layouts for Cases in Clusters and for Cluster
By            Daniel B. Carr
Copyright     2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014 
              Class use permitted
           
Sections:      

1.  Rectangular space-filling layouts
1.1 Generating random data in 8-D around the
    9 vertices of a simplex. 
1.2 Using RColorBrewer to get colors for 9 clusters 
1.3 Producing the space filling layouts
1.4 Discussion of the algorithm
1.5 Discussion of the plots
1.6 Extensions: Slicing at different angles
    in 2-D and 3-D. Ru Sun dissertation

2.  Containment tree layout using Lennard-Jones Force
2.1 Background and Lisong Sun paper
2.2 Function description
2.3 Data structures and parameters
2.4 LJ related Functions  
2.5 Example
2.6 Voronoi space filling trees
     
3.  Hexagon cluster graphs
    Results shown in class
    Contact me if you want algorithms for a 
    class project
                   
Due

1.3 Both plots
2.5 Final plot

0. Setup

## Run

source('spaceFillingLayout.r')
library(MASS)
library(RColorBrewer)

## End____________________________________________


1. Introduction to space filling layouts for
   cases in clusters.  

In data exploration for ourselves and sometimes
for communication with others we want layouts for
cases that put similar cases close together.  This
can prove a basis for such tasks as linked brushing,
plotting glyphs, and potentilly mouse hovers to 
identify cases similar to a selected case.   

With a distance matrix for cases at hand we can use
classical multidimensional scale to layout cases
represented by points in 1-D, 2-D, or 3-D.  With a
distance matrix for cluster centroids at had we can
layout clusters represents by points in 1-D, 2-D or 3-D.
However constructing a compact a layout of cases around
the cluster center points be is a challenge. 
   
This section illustrates a space filling layout for cases
that have be clustered by hierarchical clustering methods
such as those supported by R's hclust function.  The layout
produce point (case) locations in a rectangle so that all
points can be surrounded by small rectangles with equal
areas. Further, for any level of clustering, cluster
rectangles can be drawn to enclose the cluster cases.   
  
I based the  spaceFillLayout function used below on the 
pseudo code in 

Wills., G. J. 1998. "An Interactive View for Hierarchical Clustering,"
Information Visualization, '98, IEEE Computer
Society, LosAlamitos. CA 26-31.  

His paper describes linkedbrushing of points in the layout and
in other views such as in scatterplot matrices. The interactive
system illustrated the layout for around 41000 zip code regions.   

To illustrate the layout and to encourage thinking about
higher dimensions than three, we start by generating 
multivariate normal clusters in eight dimensions.

1.1 Generating multivariate data centered on the 9 vertices of
    an 8-D simplex, computing the distance matrix and clustering
    the cases.   

## Run
library(MASS)
n = 50
m0 = rep(0,8)
set.seed(137)
samp = mvrnorm(n,mu=m0 ,Sigma=diag(8))

for ( i in 1:8){
m = m0    # All zeros
m[i] = 8  # ith component 8
samp = rbind(samp,mvrnorm(n,mu=m,diag(8)))
}

clusterWard <- hclust(dist(samp),method="ward.D2")

## End______________________________________________________

1.2 Using RColorBrewer to get colors for the 9 clusters

## Run

colorCodes = brewer.pal(9,name="Set1")
colorCodes[6]="#000000"

## End

The sixth color,yellow the doesn't show up well
on a white background.  We might choose to use
a gray background.  The choice above is to 
replace yellow by black. 

1.3 Producing the space filling layouts

The spaceFillingLayout.r script file contains  
two functions

The spaceFillLayout function processes the output of hclust() to 
to produce a list that contains bivariate coordinates for plotting
1) points that represent cases,
2) boxes aroung singleton points, and
3) lines separating clusters at different levels of clustering.
  
The spaceFillDrawBox function does the drawing and has arguments
that provide drawing options.  The arguments and defaults are 

nclust = 5 : The number of last merged clusters to outline
clusFill=NA 
clusBorder="black"
pointFill = NA # box fill color for a singleton clusters 
pointBorder= "red"    

## Run: Show the 9 clusters

# Obtain coordinates for plotting
clustLayout = spaceFillLayout(clusterWard)

# Compute subscripts for the color vector of length nine
# to show the cluster memberships for all 450 points.  
# Each group of 50 rows was a different cluster of points
# around a simplex vertex in 8D.

colorIndex = (1:450-1) %/% 50 + 1  # Many ways to compute this

# Plot the points

windows(width=10,height=8.5)
plot(clustLayout$points,col=colorCodes[colorIndex],pch=16, las=1,
   main="Space Filling Layout", xlab="Pixel Coordinates",
   ylab="Pixel Coordinates" )  

# Add lines for 9 clusters
spaceFillDrawBox( clustLayout, nclust=9)

## End_____________________________________________

##Run Remove axes, show top 100 clusters,
#     highlight single clusters by changing the box color

windows(width=10,height=8.5)
plot(clustLayout$points,type='n',axes=FALSE,main="Space Filling Layout",
 xlab='',ylab='')
spaceFillDrawBox(clustLayout,nclust=100,
          pointFill="#A0E0FF",pointBorder="black")
points(clustLayout$points,col=colorCodes[colorIndex],pch=16)  

## End____________________________________________

1.4  Algorithm discussion

The algorithm starts by assigning coordinates for a rectangle
that bounds all the point plotting locations.  It then cuts the
rectangle into two rectangles whose relative areas are
the same as the relative sizes of the last to clusters that
were merged into the single cluster that contains all the cases.

The choice is always to cut the longest side of the rectangle. 
For example if the rectangle width is greater than the height,
the algorithm uses a vertical cut to create two side by side
rectangles. 

Most of time the counts for the two merging clusters
will differ and there are two possible cuts that will create
the rectangles with the correct proportions.  Either 
cutting line can be used as long as the smaller cluster
is associated with the smaller rectangle.

The process of splitting rectangles continues bases on 
merges in the cluster tree describe below until there are
no more merges.   Then each rectangle that has not been
split correspond to one case center of each rectangle becomes
the plotting coordinate for it's case. 

The script sections below show the beginning, middle and last part
of the merge cluster matrix in the R console.

# Run 

mergeMat <- clusterWard$merge
rownames(mergeMat) = 1:nrow(mergeMat)
mergeMat[1:3,]

## End

Row 1 creates the first cluster.  It has two
cases, 17 and 18.  The negative numbers indicate
they are  single cases.  Their absolute values indicate
their row numbers in the data set used to
obtain the distance matrix.  Rows 2 through 3 are also
joining cases in the in the data sets.  

The mergeMatrix row numbers are positive numbers
refer to earlier rows in the matrix and the
implied cluster of cases.      l 
 
## Run

mergeMat[341:344,]

## End

Row 341 refers to rows 94 and 232 and  
joins the two implied clusters of cases.  

Row 342 joins the single case 134 to cluster 238
Row 343 join  the single case 308 to cluster 158
Row 344 joins clusters 237 and 300

## Run

mergeMat[447:449,]

## End

Here cluster 447 merges clusters 445 and 446
Then cluster 448 merges cluster 437 with cluster 447
Then cluster 449 merges cluster 432 with cluster 448

cluster 499 has all cases. 
There were n=450 cases and n-1=449 merges. 

We can add two columns to the merge matrix
and process forward to store the number of
cases associated with 1st and 2nd columns
clusters that are merged.  

Then new columns tell the proportions to use
in splitting the rectangles as we process the
augmented merge matrix backwards 
The initial rectangle is numbered 449 and it 
contains all the cases.   

Above we chose draw the rectangles correspond to 9 clusters.  The
splitting line for merge 449 partitions points into two clusters.
The splitting line for merge 448 partition the points
into the 3 clusters. We use the the last 8 splitting lines
to show 9 clusters.

1.5  Discussion of the plot

In the example we know how the data was generated and use one
color for all 50 points generated around each vertices of the 8-D
simplex.  Hence there are 9 colors. 

The vertex at the origin is closer to the other eight vertices
be it only differs by 1 coordinant.  The other pairs differ
by two coordinates.  Consider the origin and vertex at
(8, 0, 0, 0, 0, 0, 0, 0).  These are are 8 standard deviations
a part on the x-axis. We might see occasional points 3 sigma from
the vertex centers. Two such points toward (4, 0, 0, 0, 0, 0, 0) 
one from each clusters should still be well separated. It is  not
surprising that the clustering worked.  I have seen mistakes
as 6 sigma separations. 

The situation is surprising as the dimensions get bigger. The
"volume" of spherical shells grows as we get further from the
center, so more points are in outer shells than we might 
expect.  

1.7 Discussion of methods   

The point layout by itself gives mixed information.  While rectangles
containing points of clusters that will merge into the cluster
represented by the rectangle, some of the neighboring points may
pretty far a part in terms distance matrix values.  

Ru Sun's GMU dissertation:
"2-D and 3-D Layouts to Aid Human Cognition of Local Structure
in Multivariate Data" studied one approach to locating cases in a
cluster close to together.  An example appears in my talk on
clustering earth grid cells. 

Her dissertation also address the problem of slivers that can
occur using the above algorithm. Consider the whole computer screen
as the rectangle to split.  If there is a single outlier that merges
last and there are 10000 cases, then the first split of the screen
would be a vertical line creating a rectangle that is 1/10000 of the
screen width. On my screen with 1900 pixels width the sliver rectangle
would be .19 pixels wide.  This is a problem that can be avoided.  
My screen has 2.28 million pixels or 228 pixels point. Slicing
in more directions and produce rounder areas to show points.   
Ru's algorithm (call COP) picks the split from candidate splits
at different angles that produce the two "roundest" areas. 
The folder contains an example using splitting line orientations that
are multiples of 30 degrees.  The 2-D result looks a bit like cities.    

 
2.  Containment tree layout using Lennard-Jones Potential

2.1 A bit of background

As a visiting scientist at Los Alamos National Lab during
one summer I went with my host, Steve Smith, about once a
week to visit Tom Caudell and his students at the
University of New Mexico. Tom had a vibrant group of
students and met with them weekly to hear them discuss
their progress and to hear one student make an extended
presentation on an assigned topic.  

I still remember a report on the human limitations in
locating the origin of different sounds in a 3D environment
and the layout of auditory, visual, and proprioceptive memory
in the brain. I wished I could be a part of such a research
environment at GMU. (It is a challenge to obtain
funding for a critical mass of full time students.)  

A highlight for me was seeing one of the Lisong Sun's
research on representing trees as circles within circles. 
The root node of tree is the largest circle.  It contains
all other circles.  The direct descendants of the root node
are circles within the root node circle.  Each level of
the tree hierarchy is represents as circles within circles.

A typical spring model for laying out cases has to
deal with the forces on all pairs of leaf nodes. The
recursive formulation with relative scaling breaks
the problem into the smaller problems of laying out
just the immediate descendents of each node. 

When I saw Lisong demonstrate his work I was immediately
impressed and delighted.  He could easily represent
trees with thousands of nodes on the screen. The algorithm
was very fast. The various renderings using Flatland
(their 3-D software) were beautiful.   

A reference is 
 
L. Sun, S. Smith and T. P. Caudell, 2003. A Low Complexity Recursive
Force-Directed Tree Layout Algorithm Base on Lennard-Jones Potential.
UNM Technical Report: EECE-TR-03-001.  This is a paper in the weeks
zip file.

2.2 Function description 

The following is my attempt to place circles of different sizes
sizes compactly within a circle. This function below is based 
partly on his pseudo code.  There is a big limitation and some
differences to note.    

1) The function below does not include recursion so only
   handles 1 layer 

2) The Lennard-Jones force attracts two circles to each
   other when they are not too close together. It pushes
   circles apart when they are too close.
   At a certain distance between two circle centroids
   the forces balance 

   Lisong uses a balance distance like 1.05*(radius A + radius B)
   For my hexagon layout application I used balance distance
        of radius A + radius B + constant 

3) The algorithm has a central force at attracted circles to the origin.
   Both the Lennard-Jones and the central force can have weights
   The function below only has a weight for the central force

4) A borrowed idea from Lisong's C code restricts the step size 
   at each iteration.  Intuitively, the repulsive force between
   substantially overlapping circles can be really strong, dominate
   the central force, and cause an "explosion".  It may take many
   interations for the central force to bring the circles back toward
   the rest of the circles near the origin.  A controlled explosion
   seems to save iterations.        
  
2.3 Data structures and parameters

LJ             means Lennard-Jones
clustSize      number of cases in each cluster (mass in the paper)
radii          circle radii for clusters = sqrt(clustSizes)
position       n x 2 matrix of cluster centroids:  x and y are columns
centricFactor  attraction strength toward (0,0), default 1
LJFactor       Factor scaling the strength of LJ
	          attractive and repulsive forces between cluster pairs
               Not used and implicitly set to 1
separate       Target separation distance between circles
               where attractive and repulsive LJ forces balance
               LJmod: default 4,    distance = separate + radii[i]+radii[j]
               LJit:  default 1.1,  distance = separate*(radii[i]+radii[j])
slow           For each iteration the step size for shifting circle
               centers is bounded by the circle_radii /slow
               Default 10. Smaller values allow faster change  
               The constraints the circle movement in each iteration
dtime		   LJdraw parameter 
               Pauses dtime (dead time in seconds) between each plot
               in the animation.  Default .05
skip           A script parameter,Iteration i base skip == 0 is plotted        
			
2.4 LJ related Functions

## Run

#     LJ_layout: moves circle centers to balance forces
#     This variation start the LJ repulsive 4 units before the circle
#     touch 

LJ_layout = function(position,radii,centricFactor=1,separate=4,slow=10){
      n = nrow(position)
      containRad = max( sqrt((position*position)%*%c(1,1))+radii )

	#central force on each child
	dxy = -position * cbind(radii,radii) * centricFactor/containRad	

	#LJ force due to all pairs
	for (i in 1:(n-1)){
         for(j in (i+1):n){
		vec=position[i,]-position[j,] 
       	curDist = sqrt(sum(vec*vec))
            unitVec= vec/curDist
            desiredDist= radii[i]+radii[j]+separate  # variation 
		ratio=desiredDist/curDist
       	expForce = (36*ratio^12-6*ratio^6)/curDist
            newDxy = expForce*unitVec
            dxy[i,] = dxy[i,] + newDxy
            dxy[j,] = dxy[j,] - newDxy 
      }}   
      # restrict maximum movement to 1/slow of radius
      dxyLength = sqrt((dxy*dxy) %*% c(1,1))
      denom = ifelse(slow*dxyLength> radii,slow*dxyLength*radii,radii) 
      dxy = dxy/cbind(denom,denom)
      position=position+dxy
	return(position)
}

# LJ_draw:   Draws the clusters

LJ_draw = function(position,radii,mycolors="#0080FF",dtime=.05){
     z1 = Sys.time()  
     z2 = Sys.time()
     while(difftime(z2,z1,units='secs')< dtime)z2 = Sys.time()
     containRad = max( sqrt((position*position)%*%c(1,1))+radii )
     rb = c(-containRad,containRad)
     dev.hold(1)
     plot(rb,rb,type='n')
     x=position[,1]
     y=position[,2]
     dev.hold(1)
     symbols(x,y,circles=radii,inches=FALSE,
             bg=mycolors,add=TRUE)
     dev.hold(1)
     symbols(0,0,circles=containRad,inches=FALSE,add=TRUE)
     dev.hold(1)
     text(x,y,1:length(x))
     dev.flush(4)
}     

#     LJ_start: initializes circle center positions and radii
#     Each cluster is in its own orbit around the origin 
#     The angular assigment is random
#     The first cluster is centered at the origin
#     The orbit distance is such that the cluster circles would just touch
#          if all the angles used were the same angle.  

LJ_start = function(clusterSize){
   radii = sqrt(clusterSize)
   nClust = length(radii)
   positionLength = cumsum(2*radii)-radii -radii[1]
   positionLength[1]=0
   angles =  2*pi*runif(nClust)
   x = positionLength*cos(angles)
   y = positionLength*sin(angles)
   position = cbind(x,y)
   return(list(position=position,radii=radii))  
}

#   LJ_shiftCenters  

LJ_shiftCenters = function(position,radii){
   len = sqrt((position*position)%*% c(1,1)) + radii
   containRad=max(len)  
   ind = which(len==containRad)
   if(length(ind)>1)return(position) # two or more touching circles
   touchPT = position[ind,]     # circle center of touching circle 
   moveVec = -touchPT/sqrt(sum(touchPT^2))  # unit vector

#  Let the target position length be curRad-radii
   target = containRad-radii
#  Conceptually length(position + k*moveVec) = length(target)

#  Use quadratic formula
   quadC = position[,1]^2 + position[,2]^2 - target^2
   quadB = 2*(position[,1]*moveVec[1]+position[,2]*moveVec[2])
#  quadA = sum(moveVec^2) = 1 since a is a unit vector 

   tmp = sqrt(quadB^2 - 4*quadC)
   kLarge  = (-quadB + tmp)/2 # negative values in wrong direction
#   kSmall =  (-quadB - tmp)/2 # for the ind circle this is 0  
   k = min(kLarge)/2
   return(position+ matrix(rep(k*moveVec,nrow(position)),
                           ncol=ncol(position),byrow=T))
}

## End______________________________________________

2.5  Example

The spring model below may run slow on your computer
due to settings and plotting that slows it down. You can
clik in the graphics window while it is running hit
the escape key to stop the plotting. 

You can make the skip number bigger, say 3, and rerun
an animation that just shows every 3rd step so runs
faster.      

## Run

# 1) Made up cluster sizes

clusterSize = c(60,50,40,30,20,10)

# 2) Compute initial plotting location
# Make radii the square root of the cluster size
#
# In practice I often start with multidimensional scaling
# the circle overplot some and the algorithm converges fasters

tmp = LJ_start(clusterSize)
position = tmp$position
radii = tmp$radii

# 3)  Initial plot
mycolors = rainbow(length(clusterSize))
windows()
par(pty='s')
LJ_draw(position,radii,mycolors)

# 4 Animation 1
#
# Some time run a second iteration
# with modified parameter to improve circle spacing
   
centricFactor=1
slow=2     # Don't reduce the step size too much 
skip=1
for (i in 1:400){
  position = LJ_layout(position,radii,centricFactor,separate=2,slow=slow)
  if(i%%skip==0)LJ_draw(position,radii,mycolors)
}

# Final plot
#
# If a second circle is not close to touching
# the containment circle centers
# can be shifted to make the containment circle
# smaller
 
tmp = LJ_shiftCenters(position,radii)
mycolors = rainbow(length(clusterSize))
windows()
par(pty='s')
LJ_draw(tmp,radii,mycolors)


## End____________________________________________

As the containment circle shrinks so do the 
limits on the containing plot frame.  The changes
give the impressive of pushing the extreme ticks
off the plot.  Perhaps I am easily amused but I 
enjoy watching it happen.

In my global atmospheric examples, I started with
MDS 2D positions.  The radii overlapped. The circle
push apart quickly toward covergence.  

2.6 The Voronoi Treemap paper by Balzer and Deussen
    provides an appealing way to layout clusters.  Their
    graphics are very very nice. However the 
    paper is megabytes in size so is not
    included  the folder.  The paper is available
    over the web.     

3.  Hexagon Cluster Graphs

If time permits I will give my talk on clustering
earth grids cells.  At the end of the talk 
I show hexagon cluster graphs that use the LJ algorithm
above algorithm to position circles with areas proportional
to the the number of cases in cluster of earth grid cells.  

I overlay hexagon grids on the circles, areas are porportional to 
the number of cases on a hexagon grid whose cell areas match the
areas of of cases in a circle. The next steps uses a case swapping
algorithn mentioned in the talk to put the cases in each cluster
closer to their neighbors as based on the similar atmospheric
profiles distance matrix for cells of the earth.  

I used this layout in Crystal Vision where the means of cluster
compress atmospheric summarys with grid cells
of the earth.  This provide two additional variables for brushing
the global 33 multivariate data for thousands of clusters. Unfortunately
Crysal Vision ceased working with the introduction of Windows 7.
Now I just have pictures and can't do live demos.     

 


 
