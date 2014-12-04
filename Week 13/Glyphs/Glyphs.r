File        Glyphs
By          Daniel B. Carr

Purpose     Examples and selected comments on glyphs
            Hypersphere generation and examination

Problems     1 Stars examples from R
             2 Segment Diagrams from R
             3 Thin bars and profile glyphs

             4 Generate data on a hypersurface

            Hypersurface Views

             5 Star glyphs
             6 Profile glyphs
             7 Color matrix and SVD sorting

             8 Parallel coordinates
             9 Rotating ray glyphs
            10 Scatterplot Matrix 

Due          1 One plot your choice
             2 Plot     
             3 Second plot
             6 One plot your
             7 Second plot
             8 Second plot
             9 A beginning or ending still shot
            10 plot  
             

Comments    This assignment suggests some of the limitations of glyphs,
            especially those that fail to use or poorly use position
            along a scale as part of the encoding.

            This assignment currently omits commonly tried Chernoff face
            glyphs and more recently developed human looking faces.  
            The fact that a large part of our brain is devoted to face
            recognition provide hope that human faces would provide
            a particularly powerful encoding. 
  
            I am not familiar with particular good results but face
            encodings but they continue to draw interest and may still prove
            useful.  A problem may be that we that bring to many
            personal associations to faces and exploiting would
            require personal encodings.   

            Other assignment will address Conditioning and
            Provide more details about lattice graphics.  
   
See         Companion class notes on 
            Glyphs Encoding and on 
            Dimensionality, Sectioning and Conditioning 

             
1.  Star examples from R=====================================

The case names and variable legend help to provide context
in the examples below.  We still do not know about things
such as the year, but the example is more about showing capability
than serious study of the data.   

The top row of stars makes it evident that the names are below
the stars.  However the top label appears closer to the 
stars below.  This is poor design because the natural perceptual
grouping based on proximity is not the intended based for 
interpretation.  Perhaps  a gray line in the background
between rows would avoid raising  doubt.

The number of cases, 32, is modest and the number of variables
shown in not very high, 7.  The dimension of the data structure may
may be lower than 7 dimensions        


##Run

# full circle
windows()
stars(mtcars[, 1:7], key.loc = c(14, 1.5), 
      main = "Motor Trend Cars", flip.labels=FALSE) 


# half circle
windows()
stars(mtcars[, 1:7], key.loc = c(14, 2), 
      main = "Motor Trend Cars", full = FALSE) 

## End

2. Segment Diagrams from R======================================
 
Color fill provides a sense of value added.  We can use color
to quickly search to compare the values of a specific variable
when there are just a few easily distinguished colors.  
There is a interesting results for colors represent at points
on a CIE representation (see Ware's book Information Visualization)
Colors points on the convex hull vertices (that are not on straight
lines) are supposed to be distinguishable in preattentive vision.    

Given that we have located the parts of two glyphs to compare 
we still have to make comparison and know that comparison accuracy
will declines as distance between the glyphs increases.  

How do we compare sectors?  I suspect we respond more to sector area
than to sector edge length,  but this may not be the case because
length is readily evident.  Note that sector area is not a linear
function of edge length. My guess is that the perceptual accuracy
of extraction is reduced compared to a length encoding.    

Another question about the glyph concerns gestalt formation.  
How difficult is it to make overall comparison before we focus
on details? I suspect that comparison is pretty difficult.  Working
memory only holds about four items, so it may be very difficult to 
remember more than four sectors in making comparisions.  At least
the color draws attention. Step one is to get people to look.     

## Run 
windows()
palette(rainbow(12, s = 0.6, v = 0.75)) 
stars(mtcars[, 1:7], len = 0.8, key.loc = c(12, 1.5), main = "Motor Trend Cars", draw.segments = TRUE) 
palette("default")
##End

3. Thin bars and profile glyphs

I think glyphs composed of thin bars with a connecting base line
(like a comb on its back) would provide more accurate decoding
for individual variables.  This is the framework for profile
and castle glyphs.

We can exploit Weber's law and add grid lines to help assess the heights 
of individual variables more accurately.    
  
##Run: bar plot (first cut without labels
thinBar = function(mat,xSpacing,nr,nc,ylab=NULL,
          ylabLoc=0,ybot=.2,ytop=1.0,mycolors,grid="#C0C0C0",...)
{
   n=nrow(mat)
   nvar=ncol(mat)
   if(missing(xSpacing))xSpacing=seq(.15,.85,length=7)
   if(missing(nr))nr=floor(sqrt(n))
   if(missing(nc))nc=ceiling(n/nr)
   if(missing(mycolors))mycolors=rainbow(7)
   matRange=apply(mat,2,range)
   matScaled01=scale(mat,matRange[1,],diff(matRange))   

   px= as.vector(rbind(xSpacing,xSpacing,rep(NA,nvar)))
   ymin=rep(ybot,nvar)
   yna = rep(NA,nvar)
   ydif=ytop-ybot
   plot(c(0,nc),c(1,nr),type='n',axes=FALSE,...)

   for(i in 1:n){
      # row wise
      x = (i-1)%%nc   
      y = nr - (i-1)%/%nc 
      ymax = ymin+matScaled01[i,]*ydif
      rect(x+xSpacing[1],y+ybot,x+xSpacing[nvar],y+ytop,
          density=0,col=grid)
      yMidLines = y+ybot+c(.25,.5,.75)*ydif
      segments(rep(x+xSpacing[1],3),yMidLines,
               rep(x+xSpacing[nvar],3),yMidLines,col=grid)
      for(j in 1:nvar)
          lines(x+rep(xSpacing[j],2),y+c(ybot,ymax[j]),
                col=mycolors[j],lwd=3,lend='butt') 
      lines(x+xSpacing[c(1,nvar)],rep(y+ybot,2)) 
      }
}

windows()
mycolors = rainbow(7)
mycolors[5]=mycolors[4]
mycolors[4]="#000000"
thinBar(mtcars[,1:7],mycolors=mycolors)
##End

The profile glyphs connect the top of the lines. Castle glyphs basically
use wider bars that touch so the "skyline is composed of connected
horizontal and vertical lines.  The "skyline" provides a gestalt shape
and a general height for comparison.  Also it is high time to get
case labels into the plot.      

      
##Run:  thin bars with profiles and names
thinBar = function(mat,xSpacing,nr,nc,ylab=NULL,
          ylabLoc=0,ybot=.2,ytop=1.0,mycolors,grid="#C0C0C0",labs,...)
{
   n=nrow(mat)
   nvar=ncol(mat)
   if(missing(xSpacing))xSpacing=seq(.05,.95,length=nvar)
   if(missing(nr))nr=floor(sqrt(n))
   if(missing(nc))nc=ceiling(n/nr)
   if(missing(mycolors))mycolors=rainbow(7)
   matRange=apply(mat,2,range)
   matScaled01=scale(mat,matRange[1,],diff(matRange))   

   px= as.vector(rbind(xSpacing,xSpacing,rep(NA,nvar)))
   ymin=rep(ybot,nvar)
   yna = rep(NA,nvar)
   ydif=ytop-ybot
   plot(c(0,nc),c(0,nr),type='n',axes=F,xlab='',ylab='',...)

   for(i in 1:n){
      # row wise
      x = (i-1)%%nc   
      y = nr - (i-1)%/%nc -1 
      ymax = ymin+matScaled01[i,]*ydif
      rect(x+xSpacing[1],y+ybot,x+xSpacing[nvar],y+ytop,
          col="#CCCCCC",border=NA)
      yMidLines = y+ybot+c(.25,.5,.75)*ydif
      segments(rep(x+xSpacing[1],3),yMidLines,
               rep(x+xSpacing[nvar],3),yMidLines,col="#909090")
      polygon(x+xSpacing[c(1,1:nvar,nvar)],
              y+c(ytop,ymax,ytop),col="#F0F0FF",border=NA)

      for(j in 1:nvar)
          lines(x+rep(xSpacing[j],2),y+c(ybot,ymax[j]),
                col=mycolors[j],lwd=3,lend='butt') 
      if(!missing(labs))text(x+.5,y+.13,labs[i],adj=.5,cex=.6) 
      lines(x+xSpacing[c(1,nvar)],rep(y+ybot,2),lwd=1)
   }
}

windows(width=8,height=6)
par(mai=c(0,0,.7,0))
mycolors = rainbow(7)
mycolors[5]=mycolors[4]  #playing with colors
mycolors[4]="#000000"    # sticking black in the middle
thinBar(mtcars[, 1:7],mycolors=mycolors,labs=row.names(mtcars))
##End

Okay, this glyph plot still lacks
    a legend to indicate the variables names,
    the units of measure and
    an indication of the encoding,
so is still beneath our standards. 
However, it is at least a colorful start. 

There are still more comparison issues to address. In an interactive
setting we might provide many re-expression options.

1)  Arrange the glyphs to be plotted near their neighbors
    bases on values is a dissimilarity or distance matrix. 
 
2)  Arrange the glyphs to be in clusters 

3)  Drag and drop to rearrange variables

4)  Provide alternative views
    Scatterplot matrices
    Parallel Coordinate plots
    QQplots for univariate distribution comparison

5)  Provide linking and brushing
    Across alternative view

6)  Provide focusing techiques in addition to brushing
    Mouse to select one or more cases and 
    Highlight cases like this one
    Highlight more cases like this group
    Highlight cases near a path through through these two cases
    Highlight  cases near a path through through these three cases

6.1) A slider could control how many cases to highlight
6.2) Consider different highlighting options such as
         Blending non-highligted points with the background 
         Make highlight points appear closer with 3-D cues  
       
7)  Add conventional filtering tools

8)  Add variables transformations
        

4.  Generate data on a hypersurface=============================

In mathematics a hypersurface is some kind of submanifold. In
algebraic geometry, a hypersurface in projective space of 
dimension n is an algebraic set that is purely of dimension n - 1.

Hypersurface can be defined by based on constraints such as f(x,y,z,w)=0,
or by parametric construction as below.  

Here we create a hypersurface by embedding 3D data in 4 dimensions 

## Run
# Generate 3-D points
u = runif(800,-1,.8)
v = runif(800,-1,.8)
w = runif(800,-1,.8)

# Embed in 4-D
x1 = u^2 + 2*v^2 + 3*w^2
x2 = u + v - w
x3 = 2*u^2 - v^2 + w^2
x4 = -u^2 + 7*u*v + v^2 - 2*w^2

mat = cbind(x1,x2,x3,x4)

## End

5. Stars view of a hypersurface===============================

The interested question is, “Can we see the presence of a hypersurface
Constraint used glyphs that show 4 variable glyphs?”
Can we see that structure is really three dimensional or less?

We have not added noise so we are in the relative simple case
of seeing points that are on the hypersurface.

As indicated in the companion notes, glyphs don't scale well.
Here we used 4 pages to show 800 points.  Between change blindness 
across pages and the lack of sorting within the matrix of glyphs,
our changes of seeing a constraint seem pretty dim.  

## Run

matRange = apply(mat,2,range)
matScaled01 = scale(mat,matRange[1,],scale=diff(matRange))
apply(matScaled01,2,range)  # check

rowcol = expand.grid(list(i=1:17,j=1:12))
rowcol=rowcol[1:200,]

for (page in 1:4){
dat = matScaled01[1:200+200*(page-1),]
windows(width=10,height=8)
par(xpd=T)
stars(dat,scale=F,labels=NULL,nrow=12,ncol=17,
      key.loc=c(34.7,28.3),key.labels=c('x','y','z','w'),
      main=paste('Hypersurface Part',page))
}

##End

The labeling of points is not helpful in this context, so omitted.   

The plots provide little clue that the data is on a hypersurface.  

If the stars or other glyphs were carefully sorted to
put similar glyphs together then perhaps there would be
a chance of seeing structure.

6. Profile glyph view of a hypersurface====================

This older script produces a more tradition profile plot.  

##Run

# scale the data into [lspace, 1] leaving space for a label

lspace=0  #     lspace = .1
ranges = t(apply(mat,2,range))
nmat = scale(mat,center=as.vector(ranges[,1]),
		  scale=as.vector((ranges[,2]-ranges[,1])/(1-lspace)))
nmat = scale(nmat,center=rep(-lspace,ncol(nmat)),scale=F)

# layout for a page
nr = 10
nc = 20
nvar= ncol(nmat)

# column gap size
gap = .05

# x polygon coordinates
px = c(gap,seq(gap,1-gap,length=nvar),1-gap,NA)
npx = length(px)

# x polygon coordinates for a full page
x = rep(rep(0:(nc-1),rep(npx,nc))+px,nr)
ybase = rep(0:(nr-1),rep(nc*npx,nr))

npage = (nrow(nmat)-1)%/%(nr*nc) + 1

for (k in 1:npage){
   windows(width=10,height=8)
   plot(c(0,nc),c(0,nr),type='n',axes=F,xlab='',ylab='',
      main=paste("Hypersphere Page",k))
   subs = seq((k-1)*nr*nc+1,min(k*nr*nc,nrow(nmat)))
   ns = length(subs)
   locy =    as.vector(t(cbind(rep(lspace,ns),nmat[subs,],
                      rep(lspace,ns),rep(NA,ns))))
   if(ns < nc){x = x[1:length(locy)];ybase= ybase[1:length(locy)]}	
   polygon(x,locy+ybase,col=4)
   polygon(x,locy+ybase,density=0)
  #text((rep(0:(nc-1),nr)+.5)[1:ns],rep(0:(nr-1),
  #   rep(nc,nr))[1:ns],labels=as.character(subs),cex=.55,adj=.5)
}

##END

7. Color matrix views of a hypersurface================================= 

A row of four colored rectangles can be consider a glyph.  
While a certain minimum size rectangle is required to see
Color, colored boxes take little space.  Using this
representation makes it easier to get all 800 points on 
one page.   

There are costs associate with using color. 
Converting continuous values to five color classes
loses a lot of information.  Further color is poor
encoding even for ordered variables.   

## Run:   8 columns of 100 row glyphs
#     Note: this could be recode to use rect() in stead of polyon
 
nmat = scale(mat,center=as.vector(ranges[,1]),
		  scale=as.vector((ranges[,2]-ranges[,1])))

pencolor = matrix(c(
0,0,0,
1,1,1,
.5,.5,.5,
0,.43,.86,
.5,.75,1,
.9,.9,.9,
.94,.47,0,
1,.75,.5),ncol=3,byrow=T)
mycolors = rgb(pencolor[,1],pencolor[,2],pencolor[,3],max=1)

windows(width=7.5,height=10)
palette(mycolors)
pan = panelLayout(nrow=1,ncol=8,leftMar=0,colSep=.1)

px = c(0,0,1,1,NA)-.5
py = c(0,1,1,0,NA)-.5

px2 = c(px+1,px+2,px+3,px+4)
polyx = rep(px2,100) 

py2 = rep(py,4)
polyy = rep(py2,100) + rep(seq(100,1),rep(20,100))
 
for (i in 1:8){        #process 8 groups of 100 cases
panelSelect(pan,1,i)
panelScale(rx=c(0.5,4.5),ry=c(0,101))
ib = 100*(i-1)+1
ie = ib+99

dat = as.vector(t(nmat[ib:ie,]))   # get 100 cases and 
breaks = seq(0,1,length=6)
pen = as.vector(cut(dat,breaks, include.lowest=T,labels=F))
polygon(polyx,polyy,col=pen+3,border=F)
polygon(polyx,polyy,col=3,density=0)
}
panelSelect(pan,margin='top')
panelScale()
text(.5,.5,'Hypersurface',cex=1.2,adj=.5)
## End

Again the structure is not obvious and again we could complain
about the lack of sorting.  This time we will order
the rows and columns as we did in the assigment
or Row and Column Ordering in Graphics.  

1  Traveling Salesman Problem order from the seriate package
2  Breadth traversal of a minimal spanning tree 
3  First eigenvector of a singular value decomposition  


Here we use singular value decomposition first eignenvector order
Remember nmat is centered and scaled data. Using this might be argued.    

# SVD ordering
tmp = svd(nmat)
ordx = order(tmp$u[,1])
ordy=order(tmp$v[,1])
newmat=nmat[ordx,ordy]

windows(width=7.5,height=10)
pan = panel.layout(nrow=1,ncol=8,left.mar=0,col.sep=.1)

for (i in 1:8){        #process 8 groups of 100 cases
   panelSelect(pan,1,i)
   panelScale(rx=c(0.5,4.5),ry=c(0,101))
   ib = 100*(i-1)+1
   ie = ib+99

   dat = as.vector(t(newmat[ib:ie,]))   # get 100 cases and 
   breaks = seq(0,1,length=6)
   pen = as.vector(cut(dat,breaks, include.lowest=T,labels=F))
   polygon(polyx,polyy,col=pen+3,border=F)
   polygon(polyx,polyy,col=3,density=0)
}
panelSelect(pan,margin='top')
panelScale()
text(.5,.5,'SVD Sorted Hypersurface',cex=1.2,adj=.5)

##End

8. Parallel coordinates===============================================

Parallel coordinates provide a way to plot all the data in a single plot.
Obtain and install the lattice package if it is not already available.

## Run

library('lattice')

# Hypersurface example
windows()
parallel(~mat,col=4,xlab="Hypersurface")

##End

There is a lot of overplotting, but the edges suggest structure.
The internal lines can suggest structure when not too badly
obscured by overplotting. (There are density representations
for parallel coordinate plots.) 

One way to thin lines might be to bin the data in 4-D
and select a representative point from each bin to overplot
or use in a separate plot.

Here we select 8 rows from the SVD sorted rows from Exercise 7 above,
put them last, add codes 1 and 2 to plot them in a different color.

##Run

subs = seq(50,750,by=100)
newframe = data.frame(rbind(newmat[-subs,],newmat[subs,]),Highlight=
           as.factor(rep(c('no','highlight'),c(792,8))))

# Note "highlight" get becomes level 1  and 'no" level 2 in 
# the factor due to the default alphabet ordering
# in creating a factor.   
# The "no" = level 2 colors and line with below  "green" and lwd=1 

windows()
parallelplot(~newframe[1:4],groups=Highlight,data=newframe,        
          col=c("purple","green"),
          lwd=c(2,1),
          key=list(title="Highlight 8",
             columns=2,
             points = list(pch = c(19,19), 
                      col=c("purple","green")),
             text=list(c("Highlight","Rest"))
         )
)

## End

9. Rotating Ray glyph view of a hypersurface==================

A previous assignment showed a tour of 4-D
data using stereo ray glyphs.  

Here we spin the ray glyphs to provide a 3-D
view that is augmented by a ray angle encoding the 4th variable.
The construction spins about the y-axis but could be made
more general. 

Note: if the plot is to slow and you want to stop, click in the 
plot and the depress the esc key.  

In term of spinning 3-D point cloud by mouse it is convenient
to think of mousing on an invisible sphere, dragging the mouse
conceptually attached to the sphere in the desired direction with
and the desired speed and let it go to what the cloud spin.     

## Run
   
# Scale in [-1 1],  subtract the midrange as a start 
matRange = apply(mat,2,range)
matScaledOne = scale(mat,(matRange[1,]+matRange[2,])/2,
                scale=diff(matRange)/2)
apply(matScaledOne,2,range)  # check

xyz = matScaledOne[,1:3]
x = matScaledOne[,1]
y = matScaledOne[,2]
z = matScaledOne[,3]
w = matScaledOne[,4]

d = sqrt((xyz*xyz) %*% c(1,1,1)) # largest value from origin of 3D rotation
d = max(d)
plim = c(-d,d)

dx  =  .03*d*cos(w*pi/2)
dy  =  .03*d*sin(w*pi/2)

windows(width=8,height=8.3)
par(pty='s',mai=c(.02,.02,.02,.02))
nviews = 500
for (i in 1:nviews){
  angle = (.02*i) %% (2*pi)
  xnew = x*cos(angle)+z*sin(angle)
  dev.hold(1)
  plot(plim,plim,type='n',axes=F,xlab='',ylab='')
  dev.hold(1)
  points(xnew,y,pch=20,cex=.45)
  dev.hold(1)
  segments(xnew,y,xnew+dx,y+dy)
  dev.flush(3)
}

##End

10. Scatterplot matrix 

With out further augmentation I do not consider the scatterplot
matrix a type of glyph plot. It is often more useful than glyph
plots.  In this example we structure that was not evident in most
of the glyph plots above.  

## Run
#Hypersurface
splom(~mat,xlab="Hypersurface")
 
## End
             
Now we see more edges and they are suggestive
of constraints and a lower dimensional structure.  
Overplotting is still a bit of an issue.  

We have serious difficult in linking together
the 6 bivariate components of a 4-tuple.  
Brushing, when used, helps us to focus on subsets of 4-tuples, 
but mentally linking the bivariate points to assess the 
the distance between just two 4-tuples is hard.  
 
For 3-D data,  I think we just distances better in 3-D scatterplots.
For 4-D data, by may do very poorly judge the distance between two 3-D
Augmented by rays, but I conjecture we do this better the judging the
Distance between 4-tuple pairs in a scatterplot matrix.  This begs
for experimentation.

