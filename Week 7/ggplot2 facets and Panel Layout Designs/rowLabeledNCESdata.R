               Facets for variables and perceptual grouping
        
By             Daniel B. Carr

Sections     

0. Setup
1. Get data and create variables

2. Order the data.frame rows, select desired
   variables, provide better names for facet
   labels

3. Add factors (categorical variables) with the
   desired level orders for state names 
   perceptual groups and rows within groups

4. Produce a perceptually grouped one variable
  dot plot 

5. Restructure the file with melt() to show
   all three variables using facet_grid()

6. Produce two 3 variable plots with x-axis
   scale variations

7. Instructive ggplot2 text faceting examples from 

Due: Total points: 6  
                 
     6, the two plots.  (2)  
               
     Variant on 6.  Make a superposed plot
     with three distinct symbols as an
     alternative to the justaposed panels. 
               
     Use ggplot to make a vertically stacked bar plot
     use all of the state using 4 achievment
     percents in the  original data set,
     See 3.7 in the R graphics Cookbook.

     Provide your opinion  how the data might best  
     be shown for a serious discussion
     on student achievement as indicated
     by the four categories.  
               
     7. The last plot               
               
       
National 8th grade Math Proficiency NAEP Test Scores Data for 2011
source: National Center for Education Statistics, 
http://nces.ed.gov/nationsreportcard/naepdata/
4 categories: 
% < Basic, % at Basic, % Proficient, % Advanced

0. Setup 

## Run

library(micromapST)
library(ggplot2)
library(reshape2)
source('stripRemover.r')
               
hwTheme <- theme_gray()+
  theme(
  strip.background=element_rect(fill=rgb(.9,.95,1),
    color=gray(.7), size=.4),
  strip.text=element_text(size=rel(1.05)),
  panel.border=element_rect(fill=FALSE,colour=gray(.7)),
  axis.text=element_text(colour="black"),
  panel.grid.minor = element_blank())
               
hwThemeB <- theme_gray()+
  theme(
  strip.background=element_rect(fill=rgb(.9,.95,1),
    color=gray(.7), size=.4),
  strip.text=element_text(size=rel(1.05)),
  panel.border=element_rect(fill=FALSE,colour=gray(.7)),
  axis.text=element_text(colour="black"),
  panel.grid.minor = element_blank(),
  panel.grid.major.y = element_blank())               
               
               
## End_____________________________________

1. Getting data and creating variables

## Run

data(Educ8thData)  
head(Educ8thData)

df <- Educ8thData
df$Proficient_or_Above <- df$PctProficient + df$PctAdvanced
df$Basic_or_Above <- df$PctAtBasic + df$Proficient_or_Above
head(df)               

               
## End_____________________

2. Order the data/frame rows, select desired
   variables and provide better names for facet labels

## Run

ord <- order(df$Basic_or_Above, df$Proficient_or_Above,
  df$PctAdvanced, decreasing=TRUE) # breaks ties

dfOrd <- df[ord,c(9,8,7)]
colnames(dfOrd) <- c("Basic_or_Above",
 "Proficient_or_Above","Advanced")
 
head(dfOrd)

## End__________________________

3. Add factors with the desired
  level orders for state names and perceptual groups 

## Run

# State abbrevations
ab <- rownames(dfOrd)
dfOrd$State <- factor(ab,rev(ab))

# Row grouping factor
#   Note we could to 1:11 and use the
#   the facet_grid argument as.table
classes <- paste("G",11:1,sep="")
reps <- rep(c(5,5,5,5,5,1,5,5,5,5,5))
dfOrd$Grp <- factor(rep(classes,reps),level=classes)

# A factor for rows within groups
top <- rep(1:5,5)
subs <- c(top,1,top)
labs <- c('1', '2',
          '3', '4', '5')
tmp <- labs[subs]
dfOrd$Row <- factor(tmp,levels=labs)

head(dfOrd)

# Set row colors within perceptual groups
# Brewer colors scheme would work

rowColor<- rgb(
  red  = c(1.00, 1.00, 0.00, 0.10, 0.80, 0.35),
  green= c(0.00, 0.50, 0.75, 0.65, 0.45, 0.35),
  blue = c(0.00, 0.00, 0.00, 1.00, 1.00, 0.35)
)
# light red, orange, green,
# greenish blue,violet, dark gray

## End______________________________

4. Produce a perceptually grouped one variable dot plot

Dot plot using facet_grid for perceptual grouping

## Run
               
qplot(Basic_or_Above, State, size=I(3.8),data=dfOrd,
  main = "NAEP Scores: 2011 Math 8th Grade Achievement",
  xlab="Percent Basic or Above", ylab="States") +
  aes(color=Row)+geom_point(shape=21)+
  hwTheme+ scale_color_manual(values=rowColor)+
  facet_grid(Grp ~ ., scale="free_y", space="free" ) 

               
# qqplotnNot working 
# p <- qqplot(dfOrd, aes(x=Basic_or_Above, y=State,
#             color=Row,size=I(3.8)),group=Grp))+
#   labs(x="Percent At Least Basic", y="States",
#       title="NAEP Scores 2011 Math 8th Grade Achievement")+
#   geom_point(shape=21)+
#   hwTheme+ scale_color_manual(values=rowColor)+
#   facet_grid(Grp ~ ., scale="free_y", space="free" )                
#                
# names(dfOrd)
## End____________________________________

5. Restructure the file to show all three variables 

Restructure the dataframe into the long form. This
will stack the three non-categorical variables
in one column and create a factor using the 
variable names of levels to distinguish the
different variables.  We will used this 
factor to index the columns of the facet_grid.

The melt function in the reshape2 package is perfect
for the restructuring for this simple data.frame.

## Run

dfLong <- melt(dfOrd, variable.name="Achievement")
head(dfLong)
colnames(dfLong)[5] <- "Percent"
head(dfLong)

## End_____________________________

6. Produce two 3 variable plots with x-axis
   scale variatons

6.1 Common scale and panel width for x-axis

## Run

               
p = qplot(Percent, State, size=I(3.8), data=dfLong,
       main = "NAEP Scores: 2011 Math 8th Grade Achievement",
       xlab="Percent", ylab="States") + 
     aes(color=Row) +  geom_point(shape=21)+
     hwTheme  + scale_color_manual(values=rowColor, guide=FALSE)+
    facet_grid(Grp ~ Achievement, scale="free_y", space="free") 

pFix= stripRemover(p, "y")
pFix    

## End
               
               
6.2 Separate scales and panel widths for the x-axis  
               
The y grid lines are removed in hwThemeB

## Run               
               
p2 = qplot(Percent, State, size=I(3.8), data=dfLong,
      main = "NAEP Scores: 2011 Math 8th Grade Achievement",
      xlab="Percent", ylab="States") + 
    aes(color=Row) +  geom_point(shape=21)+
    hwThemeB +
    scale_color_manual(values=rowColor, guide=FALSE)+
    facet_grid(Grp ~ Achievement, scale="free", space="free")
                 
               
p2Fix= stripRemover(p2, "y") 
p2Fix               

## End               
               

7. Instructive facet example that adds margin panels
   From ggplot2 Section 7.2 with minor modifications 

head(mpg)
table(mpg$drv)               
               
## Run
               
               
# Extract a subset of vehicles
#   exclude 5 cylinder cars 
#   and the 4-wheel and front-wheel drive cars               #
mpg2 <- subset(mpg, cyl != 5 & drv %in% c("4","f"))

# 7.1
qplot(cty, hwy, data = mpg2) + facet_grid(drv ~ cyl)+
  hwTheme

p <- qplot(displ, hwy, data= mpg2) + 
  geom_smooth(method="lm", se = F, lwd=1, col="red")
               
# 7.2
p + hwTheme

# 7.3
p + facet_grid(cyl ~ drv) + hwTheme

# 7.4
p + facet_grid(cyl ~ drv, margins = T) + hwTheme

# 7.5

qplot(displ, hwy, data= mpg2) +
geom_smooth(aes(colour = drv), lwd=1, method="lm", se = F) + 
facet_grid(cyl ~ drv, margins=T) + hwTheme


