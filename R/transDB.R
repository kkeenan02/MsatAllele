`transDB` <-
function(STRandOUT,data,corrida){

 c1<-STRandOUT$V6!="NA"
 

 
 Marker<-na.omit(STRandOUT[c1,5])
 Sample<-na.omit(STRandOUT[c1,4])
 Fragment<-na.omit(STRandOUT[c1,6])
Date<-rep(data,length(Fragment))
Plate<-rep(corrida,length(Fragment))



FAllTab<-data.frame(Marker,Sample,Fragment,Date,Plate)


c2<-STRandOUT$V7!="NA"


 Marker<-na.omit(STRandOUT[c2,5])
  Sample<-na.omit(STRandOUT[c2,4])
 Fragment<-na.omit(STRandOUT[c2,7])
Date<-rep(data,length(Fragment))
Plate<-rep(corrida,length(Fragment))

SAllTab<-data.frame(Marker,Sample,Fragment,Date,Plate)


Tab<-rbind(FAllTab,SAllTab)


Tab

}

