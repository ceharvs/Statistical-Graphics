#Micromaps test
setwd("C:/Users/chris_000/Documents/school/Statistical-Graphics/Redesign/shiny-app")

library(micromapST) # has state boundary files
df<- read.csv("data/mmdata.csv", row.names=1)
source("panelFunctions.r")
source("confidentClass3WayMap.r")
df$AllGender_LeadingDeathMechanism <- factor(df$AllGender_LeadingDeathMechanism)

windows(width=7.5, height=6)
confidentClass3WayMap(df,refRegion=FALSE,
                      rowVar=4,colVar=3,colorVar=2,
                      rowLab="Mathematics Grade 8",
                      colLab="Mathematics Grade 4",
                      colorLab="Reading Grade 8",
                      title="2009 State Average NAEP Scores As Compared to Virginia")