black = read.csv(file='LA_Parish_BlackPopPC.csv')
white = read.csv(file='LA_Parish_WhitePopPC.csv')
hispanic = read.csv(file='LA_Parish_HispanicPopPC.csv')

colnames(black)
nrow(white)
nrow(black)
nrow(hispanic)

white[,2] = rep("white",nrow(white))
black[,2] = rep("black",nrow(black))
hispanic[,2] = rep("hispanic",nrow(hispanic))

subs = c(1,2,7:13)
all = rbind(white[,subs],black[,subs],hispanic[,subs])

names(all) = c("ID","Race",'2001','2002','2003','2004','2005','2006','2007')
write.csv(all,file='KatrinaRacePC.csv',quote=FALSE,row.names=FALSE)
