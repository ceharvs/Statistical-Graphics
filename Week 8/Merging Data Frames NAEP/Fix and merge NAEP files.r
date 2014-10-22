File      Fix and merge NAEP state files.r 			    
By        Daniel B. Carr
Date      Revised Oct 27, 2013

Task:  Convert several .csv state comparison files
       obtain using NAEP tools into data frames
       for input to the confident Class Map
       function.   

NAEP:  Stands for National Assessment of Educational Progress
       Document are the folder contain this R script.


Steps  

1. Read Several NAEP state comparison files
   for different tests:
   Math, Reading, (Science and Writing)

2. Use state postal codes such as "VA"
   as row labels. 

3. Write individual data frames for
   later use. In addition the comparison
   codes we use for confident class Maps
   keep the percents below basic, basic,
   proficient and advanced percents and 
   the average score

4. Merge state comparison results and the 
   average (performance) score for differnt
   tests such as math and reading for the same
   year.    

    Recode the symbols '<', '=','>' as 
    1: confidence interval below the reference value
    2: confidence interval includes the reference value
    3: confidence interval above the refernce value
5. Write the merged file out as comma delimited file.

6. Do 4 and 5 separate for comparsion to the 
   National Public and to Virgina

Shows:      

Using for() loops to process multiple files
Using character string vectors to 
Reading files
Assign names to data.frames
Access data.frames   
Removing selected rows
Converting state names to abbrevations
Merging files
Write a .csv file
                  
Caution  The actual confidence levels is not as good as
         might be assumed and there are multiple comparison
         issues.   
   
## Run the whole script for the national public
## Modify Section 1 for Virginia the base            

#1.  Set the prefix files base names, column labels 

prefix <- "NP_"  # "VA_"

baseName <- c( "Math09G4", "Math09G8",
  "Read09G4", "Read09G8")

dfName <- paste(prefix,baseName, sep='')
fileName <- paste(prefix,baseName,'.csv',sep='')

# preferred column labels  
colName = c('Int','State','Compare','nHigher','nSame','nLower','Ave',
             'BB','Basic',"Pro","Adv")

# 2. Read a file with State names and abbreviations
#    State name conversion file
stateName = read.csv("stateNamesFips.csv")

# 3. loop to process files

for (i in 1:length(fileName)){
  dat <- read.csv(fileName[i],as.is=TRUE,
  header=FALSE,col.names=colName)

  # remove known unwanted rows
  good <- is.na(match(dat[,2],
     c("National Public","DoDEA")))
  dat <- dat[good,]

  # replace row labels with state abbreviations
  #  StateName 4th column has full state names
  #            3th column has abbreviations   
  subs = match(dat[,2],stateName[,4])   
  row.names(dat) = stateName[subs,3]   

  # Make comparisons numeric
  numb = match(dat[,3],c("<","=",">",""))
  dat[,3] <- ifelse(numb==4, NA, numb)

  #remove first columns
  dat <- dat[,-c(1,2)] 

  # save the data frame with a given name 
  # for future use
  assign(dfName[i],dat,pos=1)

}

## End__________________________ 

#4. Merge files into one file to use
#   for producing confident class maps    

# Select code and achievement (mean) columns  
subs <- c(1,5)

# Merge to files
#   The merge below matches row names (state abbreviations)
#   puts the matched row names in column 1
#   Uses integers as new row names
#
#   we reestablish the abbrevations as the
#   row names and remove the first column.  
#   in preparation for the loop which does the 
#   same
#
#  We make the column test specific before merging
#      The alternative is to get warning messages
#      R will make the column names using by adding
#      a suffix. 
 
df1 <- get(dfName[1])[,subs]
colnames(df1) <- paste(baseName[1],c("Comp","Ave"),sep='_')

df2 <- get(dfName[2])[,subs]
colnames(df2) <- paste(baseName[2],c("Comp","Ave"),sep='_')

mergeDf <- merge(df1,df2 ,by="row.names")
rownames(mergeDf)= mergeDf[,1]
mergeDf <- mergeDf[,-1]

for(i in 3:length(dfName)){
  dfNext <- get(dfName[i])[,subs]
  colnames(dfNext) <- paste(baseName[i],c("Comp","Ave"),sep='_') 
  mergeDf = merge(mergeDf, dfNext, by="row.names")
  rownames(mergeDf)= mergeDf[,1]
  mergeDf <- mergeDf[,-1]
}

outFile <- paste(prefix,"NAEP_MergedDat.csv",sep='')
write.csv(mergeDf,file=outFile,quote=FALSE)

## End ________________________
