`AlleleHist` <-
function(DataBase,marker,limits=0.01,ticks=1){

bins<-seq(    round(min(DataBase[(DataBase==marker),3]),digits=0)-1,round(max(DataBase[(DataBase==marker),3])+1,digits=0),limits)


hist(DataBase[(DataBase==marker),3],breaks=bins,main=marker,xlab=paste("Allele size (bp)", as.character(limits),"classes"),col="grey",axes = FALSE)
axis(1,pos=-0.15,at=seq(round(min(DataBase[(DataBase==marker),3]),digits=0)-1,round(max(DataBase[(DataBase==marker),3])+1,ticks)),las=0 )
axis(2,pos=round(min(DataBase[(DataBase==marker),3]),digits=0)-1)
}

