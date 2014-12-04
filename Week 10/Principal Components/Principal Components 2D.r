# Principle Components
# Illustrates Centering and Rotation in 2D

Due:  3 points
2.  Juxtaposed plot
4.  Juxtaposed plot 
5.  Second Biplot

#  0. Setup

library(ggplot2)
library(gridExtra)

hwTheme <- theme_gray()+
  theme(
  strip.background=element_rect(fill=rgb(.9,.95,1),
    colour=gray(.5), size=.2), 
  panel.border=element_rect(fill=FALSE,colour=gray(.50)),
  axis.text=element_text(colour="black")
  )

hwThemeSmallText <- hwTheme +
  theme(
  axis.text=element_text(size=rel(.8)),
  axis.title=element_text(size=rel(.8)),
  plot.title=element_text(size=rel(.8))
  )

# 1. Read data and check for missing values
  
mfcancer <- read.csv(file="US_CancerMortality_byGender.csv",
  header=TRUE, as.is=TRUE)
head(mfcancer)
tail(mfcancer)
any(is.na(mfcancer))

# Make a data frame with male and
# female columns for 2007


rate <- mfcancer[, "X2007"]
male <- mfcancer[, "Gender"]=="Male"
mat <- cbind(rate[male], rate[!male])
rownames(mat) <- mfcancer$State[1:51]
colnames(mat) <- c("Male","Female")
mat
df <- as.data.frame(mat)
head(df)

# 2. Center the variables 
#    and compare plots of the data and
#    centered data. Just the axis scales
#    should look different  

# Center the column by subtracting
# the mean using scale() 

matCen <- scale(mat,scale=FALSE)
dfCen <- as.data.frame(matCen)
head(dfCen)

# Save the means for reference lines
maleM <- mean(df$Male)
femaleM <- mean(df$Female)

# produce plots

title <-paste("State Cancer Mortality Rates 2007",
        "\nDeaths Per 100,000",sep="")   

p <- ggplot(df,aes(x=Male,y=Female))+
     geom_hline(yint=femaleM,color='blue')+ 
     geom_vline(xint=maleM,color='blue')+
     geom_point(fill="red",shape=21)+
     labs( title=title)+hwThemeSmallText

pCen<- ggplot(dfCen,aes(x=Male,y=Female))+
       geom_hline(yint=0,color='blue')+ 
       geom_vline(xint=0,color='blue')+
       geom_point(fill="red",shape=21)+
       labs(x="Male - Mean(Male)",
         y="Female - mean(Female)",
         title=title)+hwThemeSmallText

windows(width=6, height=3)
grid.arrange(p,pCen,ncol=2)

# 3. Principal components and rotation 
#
# Below we keep the original units of
# measure setting scale to FALSE
# Otherwise after centering the
# variables, prcomp will them by 
# their standard deviations.

pc <- prcomp(mat,scale=FALSE)
pcDat <- pc$x # principal components
pcRotate <- pc$rotation  #rotation matrix

# Verify rotation
# Reflections are possible
#   We can multiple any pc column by -1
#   and retain the center, variance and
#   orthogonality fo other pc's 
   
# The determinant of a rotation matrix is 1. 
# If there are also an odd number of reflections
# abound axes, the determinant will be -1. 

det(pcRotate)  # -1 mean there is also a reflection 

# Rotate the centered data and compare

matRot <- matCen %*% pcRotate
head(pcDat)
head(matRot)
all.equal(pcDat,matRot)

pcRotate

# Rounding, the first principle component is
# -.78 times the centered male vector
# -.63 times the centered female vector. 

# 4. Juxtaposed before and after rotation  
#    scatterplots
#
#  Find axis limits to accommodate
#  rotating the first principal component

big <- max(abs(pcDat[,1]))

dfPC <- data.frame(PC1=pcDat[,1],PC2 = pcDat[,2])
rownames(dfPC) = rownames(dfCen)

# Find some extreme point to label
# with state postal codes

id1 <- which.min(dfPC$PC1)
id2 <- which.max(dfPC$PC1)
id3 <- which.min(dfPC$PC2)
subs <- c(id1,id2,id3)

# Add a label State postal code
# column filled with NA to both
# data frames.  Then include 
# the three labels we want to
# show.
#     

dfPC$State <-NA
dfPC$State[subs] <- row.names(dfPC)[subs] 
dfCen$State <- NA
dfCen$State[subs] <- row.names(dfPC)[subs]

# Store the two plots and then juxtapose
#
pCen <- ggplot(dfCen,aes(x=Male,y=Female))+
  geom_hline(yint=0,color='blue')+ 
  geom_vline(xint=0,color='blue')+
  geom_point(fill="red",shape=21)+
  ylim(-big,big)+
  xlim(-big,big)+
  geom_text(aes(y=Female-5,label=State),size=4,vjust=1)+
  labs(x="Male - Mean(Male)",
    y="Female - mean(Female)",
    title=title)+hwThemeSmallText

pRot <- ggplot(dfPC,aes(x=PC1,y=-PC2))+
  geom_hline(yint=0,color='blue')+ 
  geom_vline(xint=0,color='blue')+
  geom_point(fill="red",shape=21)+
  ylim(-big,big)+
  xlim(-big,big)+
  geom_text(aes(y=-PC2+5,label=State),size=4,vjust=0)+
  labs(x="PC1 From Male and Female Rates",
    y="-PC2 From Male and Female Rates",
    title=title)+hwThemeSmallText

windows(width=6, height=3) 
grid.arrange(pCen,pRot,ncol=2)

# The warning message is not a problem
# The plan was to show just 3 of the
# 51 state codes.

# From the left panel to the right panel there
# is it looks like 135 degree rotation. Actually
# there was also a reflection about the y-axis.
# The labels, -PC2, indicates this.   

# Multiplying a pricinpal component by -1 does
# not change the two basic properties. 

# Different principle components of a data sets
# have a dot product of zero. By convention
# the are listed in decreasing variance order.
   
# The vectors are orthogonal. The
# mean the dot product is zero.
# Dot product calculation

with(dfPC, sum(PC1 * PC2))
with(dfPC, sum(PC1 * -PC2))

# Both are zero for practical purposes.

# The variance does not change

with(dfPC, var(-PC2))

# 5. Biplot

windows(width=6,height=6)
biplot(pc,scale=0,las=1,
  main= "Covariance PC Loadings Top and Right Axes")


pcCor <- prcomp(mat,scale=TRUE)
biplot(pcCor,scale=1,las=1,
  main="Correlation PC Loading Top and Right Axes")

# 6. Percent of variability represented

dfPC$State <- NULL
vec <- diag(var(dfPC))
100*vec/sum(vec)  

# The first principal component accounts for
# 95% of the variability 

# 6. Ordering States by the first principal
     component

ord <- with(dfPC,order(PC1))
round( dfPC[ord,1:2],1)
