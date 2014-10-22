               Facets for variables and perceptual grouping
        
By             Daniel B. Carr

Sections      1. Packages
              2. Get data and create variables
              3. Order the data.frame rows, select desired
                 variables, provide better names for facet
                 labels

              4. Add factors (categorical variables) with the
                 desired level orders for state names 
                 perceptual groups and rows within groups

              5. Produce a perceptually grouped one variable
                 dot plot 

              6. Restructure the file with melt() to show
                 all three variables using facet_grid()

              7. Produce two 3 variable plots with x-axis
                 scale variations

              8. Instructive ggplot2 text faceting examples from
                 Section 7.2 with a few slight modifications 

Due           Two plots from 7 and one plot from 8.        

Uses		  Packages ggplot2, reshape2 and micromapST

National 8th grade Math Proficiency NAEP Test Scores Data for 2011
source: National Center for Education Statistics, 
http://nces.ed.gov/nationsreportcard/naepdata/
4 categories: 
% < Basic, % at Basic, % Proficient, % Advanced

1. Packages 

## Run

library(micromapST)
library(ggplot2)
library(reshape2)

## End_____________________________________

2. Getting data and creating variables

## Run

data(Educ8thData)  
head(Educ8thData)

df <- Educ8thData # shorter labels
df$atLeastProf <- df$PctProficient + df$PctAdvanced
df$atLeastBasic <- df$PctAtBasic + df$atLeastProf

## End_____________________

3. Order the data/frame rows, select desire
   variables and provide better names for facet labels

## Run

ord <- order(df$PctAdvanced, df$atLeastProf,
  df$atLeastBasic, decreasing=TRUE) # breaks ties

dfOrd <- df[ord,c(7,8,9)]
colnames(dfOrd) <- c("Advanced",
 "At Least Proficient","At Least Basic")
 
head(dfOrd)

## End__________________________

4. Add categorical variables with the desired
  level orders for state names and perceptual groups 

## Run

# State abbrevations
ab <- rownames(dfOrd)
dfOrd$State <- factor(ab,rev(ab))

# Row grouping
classes <- paste("G",11:1,sep="")
reps <- rep(c(5,5,5,5,5,1,5,5,5,5,5))
dfOrd$Grp <- factor(rep(classes,reps),level=classes,ordered=TRUE)

# A factor for rows within groups
top <- rep(1:5,5)
subs <- c(top,1,top)
labs <- c('1', '2',
          '3', '4', '5')
tmp <- labs[subs]
dfOrd$Row <- factor(tmp,levels=labs)

head(dfOrd)

# Set row colors with perceptual groups
# Brewer colors scheme would work

rowColor<- rgb(
  red  = c(1.00, 1.00, 0.00, 0.10, 0.80, 0.35),
  green= c(0.00, 0.50, 0.75, 0.65, 0.45, 0.35),
  blue = c(0.00, 0.00, 0.00, 1.00, 1.00, 0.35)
)
# light red, orange, green,
# greenish blue,violet, dark gray

## End______________________________

5. Produce a perceptually grouped one variable dot plot

 Dot plot using facet_grid for perceptual grouping

## Run
windows()
qplot(Advanced, State, size=I(2.7), data=dfOrd,
  main = "NAEP Scores: 2011 Math 8th Grade Achievement",
  xlab="Percent", ylab="States") +
  aes(color=Row) +  geom_point(shape=21)+ 
  scale_color_manual(values=rowColor)+ 
  theme_set(theme_gray(base_size = 9))+  
  facet_grid(Grp ~ ., scale="free_y", space="free" ) +
  theme(strip.text.y = element_text(size=0,color=gray(.8)))


## End____________________________________

6. Restructure the file to show all three variables 

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

7. Produce two 3 variable plots with x-axis
   scale variatons

## Run

# Equal column panel widths and common scale 

windows()
qplot(Percent, State , size=I(2.7), data=dfLong,
  main = "NAEP Scores: 2011 Math 8th Grade Achievement",
  xlab="Percent", ylab="States") +
  aes(color=Row) +  geom_point(shape=21)+ 
  scale_color_manual(values=rowColor)+ 
  theme_set(theme_gray(base_size = 9))+  
  facet_grid(Grp ~ Achievement, scale="free_y", space="free" ) +
  theme(strip.text.y = element_text(size=0,color=gray(.8)))

# Next let the width of the panel match the range of the data
# This remove blank are at the left and right sides of the
# panels increase the resolution of the gaps between the
# x values.  

windows()
qplot(Percent, State , size=I(2.7), data=dfLong,
  main = "NAEP Scores: 2011 Math 8th Grade Achievement",
  xlab="Percent", ylab="States") +
  aes(color=Row) +  geom_point(shape=21)+ 
  scale_color_manual(values=rowColor)+ 
  theme_set(theme_gray(base_size = 9))+  
  facet_grid(Grp ~ Achievement, scale="free", space="free" ) +
  theme(strip.text.y = element_text(size=0,color=gray(.8)))

## End

8. Instructive facet example that adds margin panels
   From ggplot2 Section 7.2 with minor modifications 

head(mpg)

mpg2 <- subset(mpg, cyl != 5 & drv %in% c("4","f"))

qplot(cty, hwy, data = mpg2) + facet_grid(drv ~ cyl)

p <- qplot(displ, hwy, data= mpg2) + 
  geom_smooth(method="lm", se = F, lwd=1, col="red")
p

p + facet_grid(cyl ~ drv)
p + facet_grid(cyl ~ drv, margins = T)

qplot(displ, hwy, data= mpg2) +
  geom_smooth(aes(colour = drv), lwd=1, method="lm", se = F) + 
  facet_grid(cyl ~ drv, margins=T)


