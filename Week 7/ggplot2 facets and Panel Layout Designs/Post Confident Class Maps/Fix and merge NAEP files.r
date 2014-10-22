File      Fix and merge NAEP state files.r 			    
By        Daniel B. Carr
Date      Revised Oct 27, 2013

Used For  3-Way confidence interval conditioned maps

Tasks     1. Read Several NAEP state comparison files
             For different tests:
                 Math, Reading, (Science and Writing)
             
             Input files have a prefix such as 
               "NP_"  for the national public and
               "VA_" for Virginia
             Here state value are compared to the National
             Public reference. Input files have prefix "NP"
 
             NAEP compares state confidence intervals
             to an analyst selected reference value. I 
             did this twice and saved the two files
             giving them names such as NP_Read09G8
             to indicate the reference value for 
             comparison, test, year, and grade.  
           
          2. Add state abbreviations

          3. Write individual data frames for
             later use. These include percents
             below basic, basic, proficient and
             advanced variables not used in the 
             confident class examples. 

          4. Merge the files retaining the 
             state comparison results and the 
             average (performance ) score  

             Recode the symbols '<', '=','>' as 
             1: confidence interval below the reference value
             2: confidence interval includes the reference value
             3: confidence interval above the refernce value
 
	    5. Write the merged file out as comma delimited file.

Shows     Using for() loops to process multiple files
          Using character string vectors to 
            Read files
            Assign names to data.frames
            Access data.frames   
          Removing selected rows
          Converting state names to abbrevations
          Merging files
          Write a .csv file
                  
Caution  The actual confidence levels is not as good as
         might be assumed.  

         When NAEP compares each state's confidence
         to the Virginia mean this does not take into
         account the uncertainty in the Virginia mean. 

         When NAEP compares each state's confidence
         interval to the National Public this is less
         of a problem since the population size is so
         large that National Public confidence
         interval for the mean is tiny so can often
         be reasonably treated as a constant. 
         However, in this context the National Public
         value include results from a few very high
         population states such as California so
         there are some non-negligible correlations
         to address in making comparisons.    
 
         Also there is the multiple comparison 
         problem.  If we make 50 x 3 = 150
         comparisons and were actually a 
         .05 chance of being wrong we would expect and
         average of 7.5 errors per 3-way map.  
        
         Of course we should also keep  practical
         significance in mind.  The performance
         percents stashed away in the data.frames
         provide context to help us interpret the
         test results.  When 50 percent of DC 
         student's are below basic that seems
         really bad.  

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

# 2. Read file with State names and abbreviations
#  State name conversion file
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
#   Uses integers as row names new row names
#
#   we reestablish the abbrevations as the
#   row names and remove the first column.  
#   in preparation for the loop which does the 
#   same
#
#  We make the column test specific before merging
#      The alternative is to get warning messages
#      R will make the column names using by addint
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



 

