File          Cluster Subspaces
By            Daniel B. Carr
Copyright     2005, 2006, 2007, 2008, 2009,2010, 2011, 2012, 2013
              Class use permitted

Reference     Guo, D, M. Gahegan, D. Peuquet, and A. MacEachren
              2002? "Breaking Down Dimensionality: Effective and
		  Efficient Feature Selection for High-Dimensional
              Clustering" GeoVISTA Center

              This assignment is instructive but there are more
              more sophisticated methods.  

Local         Topic: Learning local feature relevance for pattern
Expertise     classification and clustering
              Dr. Carlotta Domeniconi
              Department of Computer Science, GMU
              http://cs.gmu.edu/~carlotta/

Sections      1. Introduction
              2. Classing continuous variables
              3. Entropy, Scaling, and Conditional Entropy
              4. Color matrix function for viewing variable pairs
                 conditional entropy
              5. Generating clusters in subspaces
              6. Data, entropy tables and views 
              7. Comments				

Due           Three plots from 6

1.  Introduction

The goal here is to find subspaces where subsets of cases cluster. 
The approach 
i)   Converts continuous variables into ordered categorical variables
     and then ignores the ordering
ii)  Find the Maximum Conditional Entropy (MCE) for all pairs of
     variables Vi and Vj based on two way tables of counts
     The MCE = max( CE( Vi|Vj), CE(Vj|Vi) ))
iii) Plots the symmetric matrix of MCE using singular value
     decomposition to provide row and column order.  
     The analyst looks for blocks of variables with low MCE
     and then has the option of performing separate clustering
     on each block of variables.  The other variables can be
     omitted from clustering purposes.    
        
2.  Bin a pair of continuous variables
    into square two-way table of cells

Let nRow number of table rows or columns since the table is square.
The authors state that the objective is to create tables cells
with an average of 35 counts or more. Then nRow should be small
enough so that n cases/nRow**2 >= 35. Let maxRow = sqrt(n/35).  
For algorithmic convenience the authors pick nRow as the largest power
of two smaller than maxRow.

2.1  The authors recommend recursively splitting the
     intervals by the mean of the data in each interval.  
     A procedure for this is below

##Run
nestedMeansClass <- function(x, nsplit){
  xR <- range(x)
  # include min(x)in the lowest category using cut
  xMin <- xR[1]-.000001*diff(xR)  
  xMax <- xR[2]
  xMean <- mean(x)
  xBrks <- c(xMin, xMean, xMax)
  xClass <- cut(x, xBrks, labels=F)

  if(nsplit==1)return(xClass)
  for(i in 2:nsplit){
    xMeanNew <- tapply(x, xClass, mean)
    xMean <- sort(c(xMeanNew, xMean))
    xBrks <- c(xMin, xMean, xMax)
    xClass <- cut(x, xBrks, labels=F)
  }	
  return(as.vector(xClass))
}

# test x
x <- rnorm(10000)
n <- length(x)
nSplit <- floor(logb(sqrt(n/35), base=2))
xClass <- nestedMeansClass(x, nSplit)
n
nSplit
table(xClass) 
##End_______________________________________

Note that the above makes 1 pass through the
data to get the range and 2 passes through the 
data for each split: tapply(x, ...), cut(x, ...))

2.2 Sorting the variable to obtain the classes may not
be prohibitively slow.

##Run

equalClass <- function(x, nRow){
  n <- length(x)
  subs <- round(c(1, 1:nRow * n/nRow))
  xBrks <- sort(x)[subs]
  return(as.vector(cut(x, xBrks, include.lowest=T, labels=F)))
}

x <- rnorm(10000)
n <- length(x)
nRow <- floor(sqrt(n/35))
eClass <- equalClass(x, nRow)
table(eClass)
##End_______________________________________

3.  Entropy, Scaling, and Conditional Entropy

If p is vector of probabilities for a categorical variable,
the entropy is -sum(p*log2(p))  

The main calculation below uses a scaled univariate
entropy function for application to either rows or columns.
The scaling by log2(length(p)). This has some merits. 
  The scale factor is the maximum possible entropy
    that results when the cells have equal probabilities.
    Thus the scaled result is bounded by 1.   
  The logs appear in numerator and denominator
    so the conversion factor for changing bases
    will cancel out.  The manuscript appears to 
    use log base e, rather than base 2.            
Below Sc is a suffix used as a reminder about the scaling

##Run  
entropySc <- function(p)return(-sum(p*log2(p), na.rm=T)/
                                    log2(length(p)))

conditionalEntropy <- function(tableXY){
#  xGy   means x given y
#  yGx   means y given x 
#  CE    means Conditional Entropy
#
#            X
#         1 2 3 4  yMarg 
#    Y
#    1    1 0 3 2    6
#    2    4 2 1 0    7
#    3    3 1 0 1    5
#
#  xMarg  8 3 4 3   18

yMarg <- apply(tableXY, 1, sum)          # row margin index by y
n <- sum(yMarg)                        # grand total
xGyProbs <- sweep(tableXY, 1, yMarg, '/') # rows are probs
xGyCE <- apply(xGyProbs, 1, entropySc)   # scaled univariate entropies     
xGyCEwa <- sum(xGyCE*yMarg)/n          # weighted average

xMarg <- apply(tableXY, 2, sum) # column margin index by x
yGxProbs <- sweep(tableXY, 2, xMarg, '/') # columns are probs
yGxCE <- apply(yGxProbs, 2, entropySc) # scaled univariate entropies 
yGxCEwa <- sum(yGxCE*xMarg)/n  # weighted average

return(max(xGyCEwa, yGxCEwa))
}	

# test

paperMat <- matrix(c(
0, 1, 3, 0, 0, 0, 
1, 9, 1, 0, 1, 2, 
7, 14, 3, 7, 6, 0, 
7, 6, 13, 19, 12, 5, 
0, 4, 14, 5, 1, 1,
1,2,3,2,0,0),ncol=6,byrow=T)

conditionalEntropy(paperMat)
# .812  this checks against the manuscript

##End_______________________________________________

4. Color matrix function for viewing variable pairs conditional entropy

The plan is show the color matrix of conditional entropies
for all the variable pairs and sort the variables in the effort
to draw attention to subspaces.  A color encoding that uses
five classes can be pretty primitive and hide clues about
the presence of clusters.  Do we want to see the numbers?  
If so how to we color the cells so the numbers are readable.

Plan A is to fill the cells in color and then overplot a
smaller white box as a background for the numbers.
 
There are different approaches so sorting square matrices.
The function below was originally written in Splus and had
three options:
1) use the first left eigenvector from svd
   (singular value decomposition to establish the order.  
2) use of breadth traversal of a minimal spanning tree
   to determine the  order. Since the Splus algorithm required
   data and not dissimilarities, multidimensional scaling was
   used to create pseudo data to use in mstree() 
3) supply your own order via ord (and set svdOrd=F) 

Option 2) does not yet work in R but I have heard of efforts to provide
the needed mstree() function 

##Run
  
scaledEntropyView <- function(ceMat,title="",
  colorPalette=NULL,ncolors=5,colorNA="red",svdOrd=T,ord=NULL,
  whiteV=.5,whiteH=.8,cex=.84,labCex=.84,tCex=1.2){
if(is.null(colorPalette)){
Blues <- c('#000088','#D0D0FF')
#  YlOrBr <- rev( c("#FFFFD4", "#FED98E", "#FE9929",
#    "#D95F0E", "#993404")) 
colorPalette <- colorRampPalette(Blues, space = "Lab")
}
colors=c(colorNA,colorPalette(ncolors))
 
varNames=colnames(ceMat)
if(is.null(varNames))varNames <- paste('V',1:nc,sep='')

# only uses lower triangle entries from ceMat
nc <- ncol(ceMat)
ceMat[nrow(ceMat) < ncol(ceMat)] <- 0
diag(ceMat) <- 0
ceMat <- ceMat+t(ceMat)

# get order vector
if(svdOrd){
   ord <- rev(order(svd(ceMat)$u[,1])) 
}else {
   if(is.NULL(ord)){         # get spanning tree order)
     fakeValues <- cmdscale(ceMat,k=nc,eig=T)
     nvar <- sum(fakeValues$eig>0)
     ord <- mstree(fakeValues$points[,1:nvar])$order[,1]
   }
}

ceMat <- ceMat[ord,ord]
varNames <- varNames[ord]
diag(ceMat) <- NA
panels <- panelLayout(nrow=1,ncol=1,
  leftMar=0,topMar=.8, bottomMar=0)

entropy <- ceMat[row(ceMat)> col(ceMat)]
entropyR <- range(entropy)
brksEntropy <- seq(entropyR[1],entropyR[2],length=ncolors+1)

# shift for NA
polycol <- cut(ceMat,brksEntropy,include.lowest=T,labels=F)+1  
polycol <- ifelse(is.na(polycol),1,polycol)  # NA color 1

k <- 0.5
dx <- c(-k,k,k,-k,NA)
dy <- c(-k,-k,k,k,NA)

mat <- expand.grid(list(x=1:nc,y=nc:1))
newx <- rep(mat$x,rep(5,length(mat$x)))
newy <- rep(mat$y,rep(5,length(mat$y)))

rx <- c(0.5,nc+.5)
ry <- c(0.5,nc+.5)
panelSelect(panels,1,1)
panelScale(rx,ry)
polygon(newx+dx,newy+dy,density=-1,col=colors[polycol])
polygon(newx+whiteH*dx,newy+whiteV*dy,density=-1,border=NA,
  col='white')
xdiag <- rep(1:nc,rep(5,nc))
polygon(xdiag+dx,rev(xdiag)+dy,density=-1,col="#C0C0C0")

diag(ceMat) <- 0
valsText <- format(round(ceMat,2))
diag(valsText) <- ""
text(mat[,1],mat[,2],valsText,cex=cex,adj=.5)
text(1:nc,nc:1,varNames,adj=.6,cex=labCex)
panelOutline()
panelSelect(panels,mar='top')
panelScale()
if(length(title)==1)text(.5,.8,title,cex=tCex,adj=.5) else
	text(c(.5,.5),c(.9,.5),title,cex=tCex,adj=.5)
}

##End______________________________________________
 
5. Generating clusters in subspaces to test the approach

## Run
library(MASS)

# Define composing function
myMVdat <- function(n1,cen1,cov1,row1,col1,
                   n2,cen2,cov2,row2,col2,
                   ntot,dtot){     
   d1 <- length(cen1)
   d2 <- length(cen2)
   cluster1 <- mvrnorm(n=n1, mu=cen1, Sigma=cov1)
   cluster2 <- mvrnorm(n=n2, mu=cen2, Sigma=cov2)
   full <- matrix(rnorm(ntot*dtot),ncol=dtot)
   full[row1:(row1+n1-1),col1:(col1+d1-1)] <- cluster1
   full[row2:(row2+n2-1),col2:(col2+d2-1)] <- cluster2
   return(full)
}

# Describe components
# cluster 1
n1 <- 1000
d1 <- 3
cen1 <- rep(0,d1)
cor1 <- matrix(c(
  1, .5,-.3,
 .5, 1.,-.4,
-.3,-.4,  1),
ncol=3,byrow=T)
sd1 <- c(.05,.02,.03)
cov1 <- diag(sd1) %*% cor1 %*% diag(sd1)
row1 <- 1001
col1 <- 3

# cluster 2
n2 <- 1500
d2 <- 5
cen2 <- rep(.5,d2)
cov2 <- .0004*diag(d2)
row2 <- 8001
col2 <- 8

# background
ntot <- 10000
dtot <- 15

# build test data set
testDat <- myMVdat(n1,cen1,cov1,row1,col1,
  n2,cen2,cov2,row2,col2, ntot,dtot)

##End_________________________________________  

6.  Data, entropy tables and views 

Just the lower triangle part of the symmetric table is generated.

6.1  The data as is

##Run
testDim <- dim(testDat)
nr <- testDim[1]
nc <- testDim[2]


nSplit <- floor(logb(sqrt(nr/35),base=2))
classDat <- apply(testDat,2,nestedMeansClass,nSplit)

ceTable1 <- matrix(0,nrow=nc,ncol=nc)

for(j in 1:(nc-1)){
for (i in (j+1):nc){
  tableIJ <- table(classDat[,i],classDat[,j])
  ceTable1[i,j] <- conditionalEntropy(tableIJ)
}}	

windows()
scaledEntropyView(ceTable1,title=
  c("Scaled Conditional Entropy","Original Data"))
#sorting doesn't work so well here

##End

6.2 Moving cluster centers to lower density locations

## Run
cen1 <- rep(-2,3)
cen2 <- rep(1.5,5)

testDat <- myMVdat(n1,cen1,cov1,row1,col1,
  n2,cen2,cov2,row2,col2, ntot,dtot)
  
testDim <- dim(testDat)
nr <- testDim[1]
nc <- testDim[2]
nSplit <- floor(logb(sqrt(nr/35),base=2))
classDat <- apply(testDat,2,nestedMeansClass,nSplit)
ceTable2 <- matrix(0,nrow=nc,ncol=nc)

for(j in 1:(nc-1)){
for (i in (j+1):nc){
  tableIJ <- table(classDat[,i],classDat[,j])
  ceTable2[i,j] <- conditionalEntropy(tableIJ)
}}	

windows()
scaledEntropyView(ceTable2, title=
  c("Scaled Conditional Entropy",
  "Clusters Moved To Lower Density Region"))

##End_________________________________________

6.3  Try the other classing function

##Run
testDim <- dim(testDat)
nr <- testDim[1]
nc <- testDim[2]

nClass <- floor(sqrt(nr/35))
xClass <- equalClass(x,nClass)
classDat <- apply(testDat,2,equalClass,nClass)
ceTable3 <- matrix(0,nrow=nc,ncol=nc)

for(j in 1:(nc-1)){
for (i in (j+1):nc){
  tableIJ <- table(classDat[,i],classDat[,j])
  ceTable3[i,j] <- conditionalEntropy(tableIJ)
}}	

windows()
scaledEntropyView(ceTable3,title="Equal Count Classes")

##End

7. Comments

More could be done with the appearance. 
I am tempted to suppress relative entropy values >.98
Also I am tempted to used the value (1-relative entropy)
but that might confuse people.  

It may take some experimentation and thought to get a
better understanding of the factors that influence the 
algorithm's ability to find subspaces.

Factors seem to be
i)  change in density from cluster to non-clusters
    in 2-D projections. I think it would be easier to see
    the clusters when they are located away from the center
    of the background normal distribution.  
ii) how much the clusters are split across different table cells
    It seems that classing the data and ignoring
    class order is dropping a lot of information.  

Low entropy for one variable implies low entropy
when the variable is paired with another one.  However
I am still curious about looking at three variables
at a time and the conditional entropy H(X,Y|Z).  Of
course there is a jump in computational and time requirements.

I wonder about rotation of the data.  I can envision
clusters in two staggered diagonal patterns that have
pretty uniform 1-D margins. A rotation could reveal
two modes in one of the 1-D margins.   

The R script here is pretty slow.  The authors have
their own implementation and been able to study much
larger examples.    

It is nice to have clues about what variables are noise
with respect to clustering.  There are more algorithms to
investigate.

    
