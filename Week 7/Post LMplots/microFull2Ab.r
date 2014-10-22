
microFull2Ab <- function(stateDF,stateId=NULL,
  ref=stateNamesFips){
  if(is.null(stateId)) nam <- row.names(stateDF) else
     nam <- stateDF[,stateId]
  nam <- ifelse(nam=="District of Columbia","D.C.",nam)
  check <-  match(nam,row.names(ref)) 
  bad <- is.na(check)
  good <- !bad
  nbad <- sum(bad)
  if(nbad>0){
    warning(paste(nbad,"Unmatch Names Removed",nam[bad])) 
    stateDF <- stateDF[!bad,]
    nam <- nam[!bad]
    check <- check[!bad]
    good <- good[!bad]
  }
  ngood <- sum(good)
  if(ngood < 51)warning(paste("Only",ngood,"State Ids"))
  row.names(stateDF) <- ref[check,2]
  return(stateDF)
}

