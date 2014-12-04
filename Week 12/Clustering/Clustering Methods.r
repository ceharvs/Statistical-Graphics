File          Cluster Methods, Data and Graphics
By            Daniel B. Carr
Copyright     2012, 2013, 2014 
              Class use permitted

Read: 

R for Everyone: Chapter 22 
ILSR:  Section 10.3, Lab2 and Lab3
         
Sections:      

1. Clustering introduction
 
2. The rat gene expression data set 
2.1 Variable selection and feature calculation
2.3 Read data and compute features
2.4 Data clustering objectives and cluster use  
             
3.  Clustering, distance matrix and dendrograms
3.1 Some dendrogram design considerations
3.2 A horizontal-label parabolic-line tree layout function
 
4. Cluster all the genes, specify the number of clusters 
   and assign cases to clusters.
 
5.  K-means 
5.1 Compute different numbers of clusters and compare

6.  Compute PAM clusters, compare with k-mean clusters 

7.  Distance matrix graphics and heatmap example  

8.  Showing clusters of cases in multdimension
    scaling 2D and 3D view
8.1 Look at clusters in a 2-D scaled view
8.2 Look at clusters in a 3-D scaled view

9.  Silhouette cluster criticism plot

Due: 6 points        

3.2 The first parabolic tree plot
5.1 Both plots
8.1 2-D plot with a state postal code
8.2 The snapshot3D png pot 
9. Cluster criticism silhouette plot 

Related assignments for some classes:
Color matrix layouts
Cluster and structure mismatches           
  
Related  Stat 763 Topics:
1) Sammon mapping
2) Cluster compression via  
   Entropy constrained vector quantization

3) Distances and the Wasserstein metric 
4) Multidimensional Scaling
5) Singular Value Decomposition uses
   Projection and searching  
6) 3D graphics

#================================================== 

0. Setup  

# install packages as needed 
library(cluster)
library(MASS)
library(rgl)
library(RColorBrewer)
library(vegan)
library(useful)
source("panelFunctions.r")
source("parabolaTree.r")

1. Clustering Introduction

We often use clustering (unsupervised classification) in the 
early stages of learning about entities and related phenomena.  
We assume there is as yet no objective classification system
that will suit our classification purpose or that there an absence
of suitable data to provide objective classification.   

Our simple goal is to put cases into groups (classes) so
any two cases in a group tend to have similar feature values
and any two cases in two different groups have some dissimilar
feature values. 

There are very many clustering methods available as the number
continues to grow.  Some of the basic clustering approaches
include agglomerative clustering, divisive clustering, and model
based clustering, and K-means clustering  

Computing speed is an issue with very large data sets. 
Variants of K-means algorithms are a common choice because
of the speed. This does not mean that K-means is the best
of the clustering methods for a particular data set.  
With additional knowledge some methods will seem to be
better than othersfor particular data sets.    
 
We hope that the clustering methods we try will do well enough
so we see patterns meaningful and learn.    

The data example below used rat gene expression values over time 
for a subset of genes related to the development of the central 
nervous system.   We cluster the genes in order to consider
if the there was a co-regulation mechanism and to see how
the clusters related to the gene function classifications. 

Two publications are in the folder.

The folder also has a plot from clustering Stanford Yeast genes
expression values obtained during a yeast cycle. The reference
is DeRisi J.L., Iyer V. R., and Brown P.O. 1997. Exploring the
metabolic and genetic control of gene expressio on a genomic scale,
Science 278, 680-686.  

Yes this is another set of gene times but this time with around
6000 genes. The distance matrix was bit to large for our workstations
back then so we SPlus on a GMU supercomputer to obtain the clusters.
The plot has panel for each cluster.  It overplot the lines that
connect the point of each gene plot a blue in to show the mean.
This revealed we could have eliminate the flat line cluster with
over 5000 genes use a workstations.  It is still the case that
a large distance matrix used for agglomerative clustering can
a problem.  

In STAT 763 we return to this data and address the representation of
statistics indexed by sequences letter, such as ACTGGA, that
stand for nucleotides.  (Another examples related the human immune
system used Amino Acid sequences). The 3-D graphics used my software
called GLISTEN to support the rendering interaction.  

As another example of using clusters in data exploration I typically
give a talk of the cluster earth grid cells.  Again this uses
agglomerative cluster and the distance matrix is  a bit of a problem
The calculation is slow.  Each grid cell is described by multiple
cluster compress vectors.  The number of vectors per cell and the
cluster sizes they represent varies from grid cell to grid cell.
Obtaining the expected distance between pair of grid cells involve
solving constrainted optimation problem.    

2. The rat gene expression data set 

We use a rat gene expression data set below.  Two pdf files in
the folder are a newletter article I wrote about data and a
publication.  An additional PNAS paper is
available to those interested.    

Figure 1 on page 27 of the newsletter article shows the 
gene expression series for 112 genes related to Central
Nervous System.  The measurements address gene mRNA expression
amounts at different rat ages.  The top left
panel Figure 1 designates the ages on the x-axis with codes E11,
E13, E15, E18, E21, P0, D7, D14, and A where E stand for gestation
day, P0 day of birth, D stands for day and A stands for adult. 
The study sacrificed 3 rats for each of 9 ages and reported the
corresponding average gene expression amounts.  

As a side note, this design confounds rat to rat variation with age. 
Technological advances have enabled the gathering of longitudinal data.

2.1  Variable selection and feature calculation

From the data set we select variables
and compute features to use in the clustering process. 
Our choice of features may make a big difference in the results. 

I was collaborating primarily with two researcher genomics expertise
that brought be the data. They had already decided to divide
the expression values for each gene it series maximum value. 
This explains the y-axis scale upper value of 1 in Figure 1.
Note that the transformed values are unitless. 
While the scale goes from 0 to 1 minimum is not typically zero.
The transformation did not subtract the minimum value from each series.   

They also chose to augment the series with the first differences
of the series.  This increased the number of features from 9 to
9 + 8 =1 7.  The result is 112 x 17 feature matrix used to compute
the Euclidean distance matrix than became input to hclust() for 
agglomerative clustering.   

I thought a bit about appending the first different matrix and
then observed that the distance matrix could be sames if they
used weighted Euclidean distance for the first 9 features with
weight 2 for featurs 2 to 8 and weights of 1 for figures 1 and 9.
We discussed dividing the first differences by the age gap but
did not do this.   

2.3 Read data and compute features  
  
## Run

gemIdGene <- read.csv(file='gemIdGene.csv',row.names=1)
genes <- gemIdGene[,1:9]
genes
t(apply(genes,1,range))

# calculate difference and append as new variables.  
augMat <- t(apply(genes[,1:9],1,diff))
colnames(augMat) <- paste("Dif_",colnames(genes)[-1],sep='')  
geneMat <- cbind(genes[,1:9],augMat)
head(geneMat)

## End____________________________


2.4 Data clustering objectives and cluster use 

We wanted to know which clusters of genes seemed to follow the
same up and down regulation pattern.   

We wanted to compare some alternative clustering methods using
such as one that used mutual information.  See Figure 5 on page 28.

We wanted to see how the clusters related to the classification of
genes based on their function.  

 
3. Clustering, distance matrix and dendrograms

Below we produce dendrograms using clustering
based on a distance matrix for all pairs of cases. 

For readability in this assignment we first randomly
60 genes to limit the dendrogram size
to 60 genes.  Then we produce the distance matrix
for all pairs of these genes.   

## Run

set.seed(137)
subs <- sample(1:nrow(geneMat),60,replace=FALSE)
geneMat60 <- geneMat[subs, ]  
eucDist <- dist(geneMat60)

## End

The common historical methods for hierarchical agglomerative
clustering are options in the hclust() function that comes with R.
They all merge all single cases based on the distance between
them.  They differ in their choice distances when merging a single
case with a cluster and a cluster with a cluster. 
 
We also use a divisive cluster function, diana(), which
is available in the cluster package.  

## Run

library(cluster)

clus1 <- hclust(eucDist, method= "complete")
clus2 <- hclust(eucDist, method= "single")
clus3 <- hclust(eucDist, method= "ward.D")
clus4 <- hclust(eucDist, method= "average")
clus5 <- as.hclust(diana(eucDist))

## End________________________________


The script below uses panel layout functions
to puts 3 dendrograph per page in two pdf files 

## Run

pdf(file="Dendrograms1.pdf",width=7.5,height=10)
par(cex=.8,las=1)
pan <- panelLayout(nrow=3,ncol=1,topMar=.3,
       rowSep = c(.2,.5,.5,0))

panelSelect(pan, mar="top")
panelScale()
text(.5,.5,"Dendrograms for Different Clustering Choices",
  adj=.5, cex = 1, font=1)

panelSelect(pan,1,1)
panelScale(c(-2, 64),c(-3.5,3.5))
par(new=TRUE)
plot(clus1, ann=FALSE, cex=.6)
mtext(side=3,"Complete",cex=1,font=2)

panelSelect(pan,2,1)
panelScale(c(-2, 64),c(-3.5,3.))
par(new=TRUE)
plot(clus2,ann=FALSE,cex=.6)
mtext(side=3,"Single",cex=1,font=2)

panelSelect(pan,3,1)
panelScale(c(-2, 64),c(-3.5,3.5))
par(new=TRUE)
plot(clus3, ann=FALSE,cex=.6)
mtext(side=3,"Ward",font=2,cex=1)

dev.off()

pdf(file = "Dendrograms2.pdf",width=7.5,height=10)
par(cex=.8,las=1)
pan <- panelLayout(nrow=3,ncol=1,topMar=.3,
       rowSep = c(.2,.5,.5,0))

panelSelect(pan, mar="top")
panelScale()
text(.5,.5,"Dendrograms for Different Clustering Choices Set2",
  adj=.5,cex = 1,font=1)

panelSelect(pan,1,1)
panelScale(c(-2, 64),c(-3.5,3.5))
par(new=TRUE)
plot(clus3, ann=FALSE, cex=.6)
mtext(side=3,"Ward.D",cex=1,font=2)

panelSelect(pan,2,1)
panelScale(c(-2, 64),c(-3.5,3.5))
par(new=TRUE)
plot(clus4,ann =FALSE, cex=.6)
mtext(side=3,"Average",cex=1,font=2)

panelSelect(pan,3,1)
panelScale(c(-2, 64),c(-3.5,3.5))
par(new=TRUE)
plot(clus5, ann=FALSE,cex=.6)
mtext(side=3,"Devisive",font=2,cex=1)

dev.off()

## End_________________________

Single link clusters have nice theoretical
properties but most people addressing
applications avoid its creeping cluster growth
as illustrated in the first pdf file generated
above.  Of the agglomerative clustering
approaches I tend to choose the "ward" or
"average" methods.

## End ____________________________________

3.1 Some dendrogram design considerations

In English horizontal text is preferable for labels. 

For large trees is can be helpful to provide
and overview showing modest number of clusters 
near the from the top of the tree.  Some may
outlier singleton clusters.  For large cluster
we may want drill down to see more of the tree.  

Long ago saw something like at University of 
Washington where the research of  Andreas Buja,
John Alan McDonald and Werner Stuetzle drew my attention.
Their tree view had cascaded scaling progressive disclosure.
It would be help have tools like this in R.   
 
3.2 A horizontal-label parabolic-line tree layout function

I wrote this function long ago and did not refined it. It
sufficed for several small example and only occasionally produced
bothersome connecting lines.  

As illustrated below entering the name of the function
causes the function definition to be listed.   This is a
reasonable function to inspect to see how the scaling works.  
The parabola part script can be skipped. 

There is an argument description near the top
of the functgion which may be of help if you want to use the
function in your final project.


##Run

# Look at the function arguments
head(parabolaTree)

pdf(file="ParabolaTree_RatGene60.pdf",width=7.5,height=10)
parabolaTree(clus3,lab=clus3$labels,cex=.75,
  title="Rat Gene Expression for 60 Genes", labelF =.12,
  xGrid=seq(2, 12, by=2))
dev.off()

## End__________________________________________

Modify the parabolaTree arguments in the script below.     

   For xGrid include a line at 0,
   
   Reduce the parabola horizontal height
   fraction (parabolaF) of the merge value
   range to a smaller value from the default of .083   

   Make the  nameGapF fraction for the parabola
   an a little larger the default .01 

## Run

pdf(file="ParabolaTreeRatB.pdf",width=7.5,height=10)
parabolaTree(clus3,lab=clus3$labels,cex=.75,
  title="Rat Gene Expression for 60 Genes", labelF =.12,
  xGrid=seq(?, 12, by=2), parabolaF=?, nameGapF=?)
dev.off()

##End______________________________________


4. Cluster all the genes, specify the number of clusters 
   and assign cases to clusters.  

The cutree() function takes an hclust object as
an input argument. 

The argument, k, if provided directly specifies the
number of clusters.  

The argument, h, if provided is the tree merge height
(the distance between clusters and cases when merged)
above which no more clusters are merged.  This
determines the number of clusters.   

The returned object is vector that has cluster
membership for each case in case order. 
Tabling the vector tells number of cases in each class.  

## Run

eucDistAll <- dist(geneMat)

clusAll <- hclust(eucDistAll, method="ward.D")
caseClasses1 <- cutree(clusAll,k=6)
caseClasses1
table(caseClasses1)

caseClasses2 <- cutree(clusAll,h=2.24)
table(caseClasses2)

## End______________________________________________

Deciding how many cluster to use is not well defined
problem without some additional constraints. 

There are some ways of looking at the data that may help
on occasion.  See examples below related to looking
at clusters and cluster criticism. 

Section 5 below on K-mean clustering look at that total
sum of squares from fitting each cluster's mean to its data.  
We usually hope there is the modest number of clusters at
which total within cluster sum square from fitting mean is
small or a dramatic improvement over using 1 fewer cluster.  
This approach to cluster number selection is generally applicable.    

5. K-means

A common variant of K-means randomly picks k cases
to be the seeds of k different clusters.
Each of the remaining cases is associated with the
cluster whose mean vector is closest (squared distance).
That clusters mean vector is then updated based
on new case's values.  

There are many variants on this idea. 

There is a random starting variant. For example
there is a problem if some of the first cases
picked to be seeds of different clusters really
belong to the same cluster. As a partial solution
the R algorithm provides a nstart argument. R
runs the clustering algorithm nstart times.
It keeps the clustering that best fits the data
for the given number of cluster centers. 

A different kind of variant seeks to avoid
multiple passes through the data.  It starts
with a large number of clusters, using a
given power of 2, say 11.
It runs K-mean but accumulates but the cluster
mean and cluster covariance matrix using update
algorithms.  Finally it reduces the number of
clusters using the 2^11 = 2048 clusters means and
covariance matrices  
    
5.1 Compute different numbers of clusters and compare

## Run
km2 <- kmeans(geneMat, centers=2, nstart = 20)
km3 <- kmeans(geneMat, centers=3, nstart = 20)
km4 <- kmeans(geneMat, centers=4, nstart = 20)
km5 <- kmeans(geneMat, centers=5, nstart = 20)
km6 <- kmeans(geneMat, centers=6, nstart = 20)
km7 <- kmeans(geneMat, centers=7, nstart = 20)
km8 <- kmeans(geneMat, centers=8, nstart = 20)
km9 <- kmeans(geneMat, centers=9, nstart = 20)

# Look at the result for three clusters
 
km3$centers   
km3$cluster # a vector give case cluster membership cluster

# Comparing within cluster total sum of squares
plot(2:9, c(km2[5], km3[5], km4[5], km5[5],
  km6[5], km7[5], km8[5],km9[5]),
  main = "K Means:  How many clusters?",
  las=1, xlab="Number of Clusters", 
  ylab="Within Clusters Total Sums of Squares")

## End________________________

The plot shows diminishing returns with no big jump that
might suggest a stopping place.

As indicated in "R for Everyone" Hartigan's Rule is a good metric
for determining the number of clusters.  The FitKMeans() function
in the package useful implements this.  It takes the next step
from using the within cluster total sum of square and accounts
for the number of rows and columns.  

## Run
geneBest <- FitKMeans(geneMat, max.clusters=20,nstart=25,seed=43)
geneBest 
PlotHartigan(geneBest)

## End______________________

I find this is example interesting.  First, our published work
used 6 clusters not 5.  The plot suggest 6 or 7 might a reasonable
number.  I wonder if genes in the 6 k-means clusters are the same
as in our 6 clusters. 

Second, we might argued that there are really only 9 variables rather
the 17 variables and that may make a difference in the calculations.
Is there role selection procedure to address variable weights when
computing the distance matrix?    

Jared Lander also suggests obtaining cluster number guidance
using clusGap() in the cluster package.  I have read (but not verified)
that there are guidance algorithms in the hopach package.   
 
6. Compute PAM clusters, compare with k-mean clusters   

PAM stands for partitioning around mediods. For k clusters this finds
the k points that minimize the sum of dissimilarities to their
closest representative objects.

##  Run computer clusters and look at a results

library(MASS)

eucDistAll <- dist(geneMat)

pam2 <- pam(eucDistAll, k=2, diss=TRUE)
pam3 <- pam(eucDistAll, k=3, diss=TRUE)
pam6 <- pam(eucDistAll, k=6, diss=TRUE)

pam2

# See if the gene K-means and pam class labels are the same
# for 2 clusters

all(names(km2$cluster) == names(pam2$clustering))
table(km2$cluster, pam2$clustering)

## End____________________________

The K-means label 1 corresponds to pam label 2
and vice versa.     

There are 5 mismatches with the K-means clustering
with 2 clusters and the pam cluseres    

7. Distance matrix graphics and heat map example  

A later class typically illustrates many
different ways arrange the row and columns of a 
matrix in order to bring out pattern. 

The order of cases in a dendrogram not particularly good order.
For every merge of two clusters there is an
assignment of which cluster goes on the left when build a
and which goes on the right when building a dendrogram.  
This assign may be control by an additional criterion, but
that may not be a good one.  In the sense of constructing a valid
dendrogram the assignmment is arbitrary. For N
cases there are N-1 merges and hence  2^(N-1) arbitrary left|right 
assignments producing the order.  

Some reasonable alternaties  to order the  rows (and columns) of a
distance matrix are the 1st left singular vector of an singular value
decomposition of the data matrix and a breadth traversal of a minimal
spanning tree.  

See Carr, D. B. and A. R. Olsen 1996.  "Simplifying Visual
Appearance By Sorting:  An Example Using 159 AVHRR Classes,"  
Statistical Computing & Graphics Newsletter, Vol. 7 No. 1  pp. 10-16.

Below is a quick "heat map" view. For me this is dendrogram augmented
color matrix plot.         

## Run
library(RColorBrewer)
hmcol <- colorRampPalette(brewer.pal(6, "RdBu"))(236)
heatmap(as.matrix(eucDist), sym=TRUE, col=hmcol,
distfun=function(x) as.dist(x))

# End

8. Showing clusters of cases in 2-D and 3-D.  

Classical multidimensional scaling, cmdscale() uses a singular value
decomposition to provide approximations to the distance matrix
based on fewer variables than in the original data.  These pseudo
variables are eigenvectors created by the decomposition. 

We scale the cases into 2-D or 3-D views where plotting is easy.
In 2-D we add a class labels at cluster centroids and a convex hull.  
We can also use identify() to label points.   

In 3-D we get a better represention of this distance between cases.
Showing convex hull might be nice.  We can used text3d() to label
points.  This might be chosen from the 2-D view.

Sometimes distinct clusters in higher dimensions will be distinct in 
in low dimensional views.  Sometimes they overplot in low
dimensional views.  Without addition clue such as colors,
apparent clusters in a low dimension may mixtures of distinct cluster
in higher dimensions.  

Below we consider a data set on state high school dropout rates
2009-2010 provided by student.  His paper suggested the source as
the Organization for Economic Cooperation and Development
Programme for International Student Assessment.  Google on PISA OECD
to pursue reports and data. I have not verify that the particular 
data set is available there.    

At the end of the 2-D script below CLICK IN THE PLOT by a few points
to obtain state abbreviations and then right "mouse" and select stop. 
Otherwise R may seem stuck is awaiting your response in the
plot.  

8.1  A 2-D example

## Run  

library(vegan)
dropout <- read.csv(file="stateDropoutRates.csv",
                    header=TRUE,row.names=1)
# Rates for 9th, 10th, 1lth and 12th grades 
dat<- dropout[ ,2:5] 

kclus <- kmeans(dat,centers= 5, iter.max=100, nstart=100)
kclus$cluster
groups <- levels(factor(kclus$cluster))

distMat <- dist(dat)
stateMds2 <- cmdscale(distMat,k=2)
windows()

ordiplot(stateMds2,type='n',las=1,
xlab="x",ylab="y", main= "State Dropout 2-D MDS Plot")
ordispider(stateMds2, factor(kclus$cluster), label = TRUE,lwd=1)
ordihull(stateMds2, factor(kclus$cluster), lty = "dotted")

cols <- c("red", "blue",rgb(0,.6,1), "violet", "darkorange")
for(i in seq_along(groups)){
  points(stateMds2[factor(kclus$cluster) == groups[i], ],
 col = cols[i], pch = 16)
}

identify(stateMds2[,1],stateMds2[,2],row.names(stateMds2))

## End________________________________________


8.2 Look at clusters in a 3-D scaled view

When the plot appears grab corner and stretch to enlarge the plot
Click in the plot and drag to rotate it. 
Right mouse and drag down to enlarge and drag up to reduce 


## Run 

stateMds3 <- cmdscale(distMat,k=3)

tst <- as.factor(kclus$cluster)
cenx <- tapply(stateMds3[,1],tst,mean)
ceny <- tapply(stateMds3[,2],tst,mean)
cenz <- tapply(stateMds3[,3],tst,mean)
cenMat <- cbind(cenx,ceny,cenz)
cenMat

n <- nrow(dat) # number of states
odd <- seq(1,2*n,by=2)
odd+1
lineMat <- matrix(0,nrow=2*nrow(dat),ncol=3)
lineMat[odd,]<-stateMds3
tmp <- cenMat[kclus$cluster,]
dimnames(tmp) <- NULL
lineMat[odd+1,] <- cenMat[kclus$cluster,]


# A function of a function's result
myFun <- function(x)diff(range(x))
xyzR <- apply(stateMds3,2,myFun)

library(rgl)
open3d()
plot3d(stateMds3, type="s", radius=3, col=cols[kclus$cluster],
 xlab="Mds1", ylab="Mds2",zlab = "Mds2", main="MDS Coordinates")
aspect3d(xyzR)
segments3d(lineMat,lwd=2)

## End_________________________________________

Enlarge and rotate for a better view the

## Run

snapshot3d(file="MDS 3D plot of States.png")



9. Cluster criticism using the silhouette plot 

This provides one method
for suggesting mis-clustered cases.  

For each case j find its average dissimilarity
to all the cases in each of the clusters.
Let aj be the average dissimilarity to its cluster
Let bj be smallest of average dissimilarity
  to the other clusters.

With each case j as a different row, plot a bar
   from 0 to Sj = (bj-aj)/max(aj, bj).

If average dissimilarity bj > average dissimilarity aj 
as we would hope, the Sj is positive and less than 1. 

If the average dissimilarity to a different cluster is 
smaller than aj, then Sj is negative the cluster membership
is suspicious. Substantially negative values are really
suspicious.  
  
## Run

silpam6 <- silhouette(pam6)
plot(silpam6, main="")
    
## End____________________________


