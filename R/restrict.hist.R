`restrict.hist` <-
function(DataBase,marker,MIN,MAX,limits=0.01,ticks=1){

ShortDB<-DataBase[(DataBase==marker),]
c1<-ShortDB[,3]>MIN & ShortDB[,3]<MAX

hist(ShortDB[c1,3],breaks=seq(MIN,MAX,limits),axes=FALSE,col="grey",main=paste(marker,"restricted from",MIN,"to",MAX),xlab="Allele size (bp)")
axis(1,pos=-0.15,at=seq(round(min(DataBase[(DataBase==marker),3]),digits=0)-1,round(max(DataBase[(DataBase==marker),3])+1),ticks),las=0 )
axis(2,pos=MIN)

}

