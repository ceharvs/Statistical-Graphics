#Micromaps test
setwd("C:/Users/ceharvey/Documents/Personal/school/Statistical-Graphics/Redesign/shiny-app")

library(micromapST) # has state boundary files
df<- read.csv("data/mmdata.csv", row.names=1)
source("panelFunctions.r")
source("confidentClass3WayMap.r")
df$AllGender_LeadingDeathMechanism <- factor(df$AllGender_LeadingDeathMechanism)

# Format data for Micromaps
df$AllGender_LeadingDeathMechanism <- factor(df$AllGender_LeadingDeathMechanism)
df$Q_AllGenders_Donors <- cut(df$AllGenders_Donors, quantile(df$AllGenders_Donors, probs=seq(0,1,1/3)), labels = 1:3, include.lowest = TRUE)
df$Q_AllGenders_Recovered_All <- cut(df$AllGenders_Recovered_All, quantile(df$AllGenders_Recovered_All, probs=seq(0,1,1/3)), labels = 1:3, include.lowest = TRUE)
df$Q_AllGenders_Recovered_Kidney <- cut(df$AllGenders_Recovered_Kidney, quantile(df$AllGenders_Recovered_Kidney, probs=seq(0,1,1/3)), labels = 1:3, include.lowest = TRUE)
df$Q_AllGenders_Recovered_Pancreas <- cut(df$AllGenders_Recovered_Pancreas, quantile(df$AllGenders_Recovered_Pancreas, probs=seq(0,1,1/3)), labels = 1:3, include.lowest = TRUE)
df$Q_AllGenders_Recovered_Liver <- cut(df$AllGenders_Recovered_Liver, quantile(df$AllGenders_Recovered_Liver, probs=seq(0,1,1/3)), labels = 1:3, include.lowest = TRUE)
df$Q_AllGenders_Recovered_Intestine <- cut(df$AllGenders_Recovered_Intestine, quantile(df$AllGenders_Recovered_Intestine, probs=seq(0,1,1/3)), labels = 1:3, include.lowest = TRUE)
df$Q_AllGenders_Recovered_Heart <- cut(df$AllGenders_Recovered_Heart, quantile(df$AllGenders_Recovered_Heart, probs=seq(0,1,1/3)), labels = 1:3, include.lowest = TRUE)
df$Q_AllGenders_Recovered_Lung <- cut(df$AllGenders_Recovered_Lung, quantile(df$AllGenders_Recovered_Lung, probs=seq(0,1,1/3)), labels = 1:3, include.lowest = TRUE)
df$Q_AllGenders_Recovered_All <- cut(df$AllGenders_Recovered_All, quantile(df$AllGenders_Recovered_All, probs=seq(0,1,1/3)), labels = 1:3, include.lowest = TRUE)
df$Q_AllGenders_Transplanted_Kidney <- cut(df$AllGenders_Transplanted_Kidney, quantile(df$AllGenders_Transplanted_Kidney, probs=seq(0,1,1/3)), labels = 1:3, include.lowest = TRUE)
df$Q_AllGenders_Transplanted_Pancreas <- cut(df$AllGenders_Transplanted_Pancreas, quantile(df$AllGenders_Transplanted_Pancreas, probs=seq(0,1,1/3)), labels = 1:3, include.lowest = TRUE)
df$Q_AllGenders_Transplanted_Liver <- cut(df$AllGenders_Transplanted_Liver, quantile(df$AllGenders_Transplanted_Liver, probs=seq(0,1,1/3)), labels = 1:3, include.lowest = TRUE)
df$Q_AllGenders_Transplanted_Intestine <- cut(df$AllGenders_Transplanted_Intestine, quantile(df$AllGenders_Transplanted_Intestine, probs=seq(0,1,1/3)), labels = 1:3, include.lowest = TRUE)
df$Q_AllGenders_Transplanted_Heart <- cut(df$AllGenders_Transplanted_Heart, quantile(df$AllGenders_Transplanted_Heart, probs=seq(0,1,1/3)), labels = 1:3, include.lowest = TRUE)
df$Q_AllGenders_Transplanted_Lung <- cut(df$AllGenders_Transplanted_Lung, quantile(df$AllGenders_Transplanted_Lung, probs=seq(0,1,1/3)), labels = 1:3, include.lowest = TRUE)


windows(width=7.5, height=6)
confidentClass3WayMap(df,refRegion=FALSE,
                      rowVar=19,colVar=18,colorVar=17,
                      rowLab="Mathematics Grade 8",
                      colLab="Mathematics Grade 4",
                      colorLab="Reading Grade 8",
                      title="2009 State Average NAEP Scores As Compared to Virginia")