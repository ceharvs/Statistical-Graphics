# Script from ISLR
# with slight modification

# Due 2 points
# 2. Second Biplot
# 4. Second qplot

0. Setup
library(ggplot2)

1. Look at the Arrest Data
states=row.names(USArrests)
states
names(USArrests)
head(USArrests)
colMeans(USArrests)

apply(USArrests, 2, var)

2. Principal Components

pr.out=prcomp(USArrests, scale=TRUE)
names(pr.out)
pr.out$center
pr.out$scale
pr.out$rotation
dim(pr.out$x)

3. Biplots

biplot(pr.out, scale=0,las=1)

pr.out$rotation=-pr.out$rotation
pr.out$x=-pr.out$x
biplot(pr.out, scale=0)

4. Percent of Variance Explained

pr.out$sdev
pr.var=pr.out$sdev^2
pr.var
pve=pr.var/sum(pr.var)
pve
x = 1:length(pve)
qplot(x,pve, xlab="Principal Component", 
  ylab="Proportion of Variance Explained", 
  main="US Arrests By State",ylim=c(0,1)) +
  geom_line()+geom_point(shape=21,fill="red",cex=3)

qplot(x,cumsum(pve), xlab="Principal Component",
  ylab="Cumulative Proportion of Variance Explained",
  main="US Arrests By State",ylim=c(0,1))+
  geom_line()+geom_point(shape=21,fill="red",cex=3)


