          Clean Crime Data
By:       Daniel Carr
File:     Regression.r 

Topics:   1.  String processing for labels
          2.  Fixing and reading the data 
              Change the missing data codes to NA
          3.  Dealing with missing data
          3.1 Removing variables
          3.2 Removing cases
          3.3 Writing the CSV file
         
Read:     Crime data comments.pdf

Data
Source:   
  http://archive.ics.uci.edu/ml/datasets/Communities+and+Crime+Unnormalized
                        
1. String processing to extract variable labels______________

The download of the description file did not work.
I copied text from the web site html file for variable
descriptions and pasted this into Wordpad.  

Next steps included
1) Hand removal of county Code, community Code and fold number variable
   label lines and blank lines.
2) Hand removing "--" and at the beginning of the lines.  
   In hind sight I could have removed "-- ", but then chose
   to illustrate removing blank in R.    
3) Observations 
   The variable description lines are one and not convenient
       as variable labels.  
   The text left ":" on each line can be variable label
   strsplit() can split such strings
   scan() can read a line at time with "\n" as the delimiter  

## Run

varNam <- scan(file="CrimeVarNames.txt", what="a", sep="\n")
tmp <- strsplit(varNam, ":") 
tmp

## End_____________________________

The result is a list structure with 144 length 2 vectors
The first of each pair is the string we want.
unlist() will make a vector from the list
We will extract the strings with odd numbers subscripts     

## Run

varLab <- unlist(tmp)[seq(1, 288, by=2)]
varLab

## End___________________________

The first character is blank
nchar() produces the lengths of strings in the vector.
substring () will let us pick the beginning and 
ending of the substring we want to keep.
Below we skip over the 1st character.  

## Run

n <- nchar(varLab)
varLab <- substring(varLab, 2, n)

# some labels are too long

varLab[1] <- "place"
varLab[2] <- "state"
varLab[3] <- "population"

varLab

## End

2. Reading and fixing the data________________________________ 

When saving the downloaded comma delimited data file, I chose 
the .csv extension so I could read it in Excel.       

I hand deleted the columns and labels corresponding to the
variable descriptions I had removed above.  

It is wise to document hand editing and save a copy of
annotations removed from .csv files for latter reference. 

I observed that the missing data code was "?".  This is easy
to fix in R, so I chose not to use hand search and replace methods. 
I prefer using R scripts to limit the amount hand editing
because itdocuments the work and enable easy replication.
   
## Run:  Read data, address missing data codes and variables labels

crimeDat <- read.csv(file="crimeDat.csv", header=FALSE, na.string="?")
colnames(crimeDat) <- varLab
head(crimeDat)

## End___________________________

3. Addressing missing data______________________________

The treatment of missing data is an important and
substantially studied topic but little addressed here.  

Some outliers have a story to tell.  Carr and Nicholson [1983]
provided a scatterplot matrix approach to looking at outliers/
It used the appropriate scale frame coordinates to replace
the missing value of (x,y) pairs.  In one example our plots
showed that missing rain fall chemical concentration values
were related to small volume rain fall samples.  However
sometimes concentrations were reported for even smaller
volumes than some shown as missing values samples.     
Such reported small volume sample seemed suspect.  
There are issues to address when values are close to
detection limits.  

There many reasons data is not available and hence in 
some sense is missing.  

Federal agencies often suppress values with high uncertainty.
This is often related to small sample sizes.  

When small populations are involved federal agencies often
suppress values due to concerns about confidentiality.

The absence of data is often overlooked. Sometimes
data can  be absent because it purposely not collected
or totally hidden. For example data on the distribution
of cigarette smoke exposure to local populations is
not collected.  Survey samples don't get at individual
dose distribution.  Smoking "rates" tend to be
inferred from state cigarettee tax revenues.
Little data, little liability.  There are
lots of toxic releases.   

Missing data can be characterized if different ways.
In some situation analysts proceed assuming the data
is missing at random.  When data is missing at random
the chances of producing a biased analysis by omitting
cases and variables with missing data may be reduced.
Further the use of imputation methods to fill in missing
data values is easier to justify. 
  
There are various approach to imputation. 
Random Forests provides a data imputation approach.
I have little background so will not address this or
than say I am more comfortable when imputation
seems more like interpolation and extrapolation. 

This assignment jumps in and removes variables and cases
with missing data. The task is to illustrates some
approaches to regression and difficulties in terms of
variables selection and model interpretation relative to
the variables.
      
3.1 Find and remove columns with a lot of missing data________________ 
    Do not remove the planned dependent variable.  

## Run: count the number missing in each column and remove variables  

myNA <-  function(x) sum(is.na(x))
missDat <- apply(crimeDat, 2, myNA)

missDat   

# Keep the Violent Crime column with  221 or less missing
crimeFix1 <- crimeDat[, missDat < 222]

dim(crimeFix1)
head(crimeFix1)

## End_______________________________

3.2  Find and remove cases with missing data________________________

## Run:

caseMiss <- apply(crimeFix1, 1, myNA)
table(caseMiss)
crimeClean <- crimeFix1[caseMiss == 0, ]

dim(crimeDat)
dim(crimeClean)

## End

3.3 Writing the CSV file

## Run
write.csv(crimeClean,file="crimeCleanB.csv",quote=FALSE)

## End


