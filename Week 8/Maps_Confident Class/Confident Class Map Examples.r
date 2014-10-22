# File      Confident Class Map Examples.r
# By        Daniel B. Carr
# Copyright 2010, 2011, 2012, 2013, 2014
#           Class use permitted
  
# Due        3 points
#            Your preferred plot from 1 
#            Your preferred plot from 2
#            If you don't have a preference indicate
#            so or indicate why you have a preference
#
#            Improvement suggests are welcome and
#            not graded.  
#
# Data       NP_NAEP_MergedDat.csv
#            VA_NAEP_MergedDat.csv

# 0. Setup

library(micromapST)
source("confidentClassMap.r")
source("confidentClassMap2.r")
source("panelFunctions.r")

# 1. State comparisons versus Virginia

# Read file
dFrame<- read.csv("VA_NAEP_MergedDat.csv",row.names=1)
head(dFrame)

windows(width=8, height=8)

# Pick data frame variables that have numbers
# 1, 2 and 3 the represent 
# below, similar to, and above
# Set refRegion="VA"
# Provide labels

windows(width=7.5, height=6)
confidentClassMap(dFrame,refRegion="VA",
  rowVar=3,colVar=1,colorVar=7,
  rowLab="Mathematics Grade 8",
  colLab="Mathematics Grade 4",
  colorLab="Reading Grade 8",
  title="2009 State Average NAEP Scores As Compared to Virginia")

windows(width=7.5, height=6)
confidentClassMap2(dFrame,refRegion="VA",
  rowVar=3,colVar=1,colorVar=7,
  rowLab="Mathematics Grade 8",
  colLab="Mathematics Grade 4",
  colorLab="Reading Grade 8:",
  title="2009 State Average NAEP Scores As Compared to Virginia")
 
# 2. State comparisons versus National Public

df<- read.csv("NP_NAEP_MergedDat.csv", row.names=1)
head(df)

windows(width=7.5, height=6)
confidentClassMap(df,
  rowVar=3,colVar=1,colorVar=7,
   rowLab="Mathematics Grade 8",
   colLab="Mathematics Grade 4",
   colorLab="Reading Grade 8",
   title="2009 State Average NAEP Scores As Compared to National Public")

windows(width=7.5, height=6)
confidentClassMap2(df,
  rowVar=3,colVar=1,colorVar=7,
   rowLab="Mathematics Grade 8",
   colLab="Mathematics Grade 4",
   colorLab="Reading Grade 8:",
   title="2009 State Average NAEP Scores As Compared to National Public")

