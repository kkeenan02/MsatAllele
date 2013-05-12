
write.data<-function(DB,Mtable,file="MLG table"){

nloc<-nrow(Mtable)
Loci<-unique(DB$Marker)

for(l in 1:nloc){


if(Mtable$Method[l]=="Alg") {


LowerBins<-BinStat(DB,Mtable$Marker[l])$Min

DefBinLim<-data.frame(rep(Mtable$Marker[l],length(LowerBins)),names(LowerBins),LowerBins)
names(DefBinLim)<-c("marker","alleles","LowerBin")

}else{ 
DefBinLim<-get(as.character(Mtable$Method[l]))
}


#using cut and the above generated tables to define allele codes
#first filter the database for the loci


DBlocus<-DB[DB$Marker==Loci[l],]

BinnedAlleles<-cut(DBlocus$Fragment,breaks=c(DefBinLim$LowerBin-.01,DefBinLim$LowerBin[nrow(DefBinLim)]+4),
	labels=DefBinLim$alleles)

DBlocus<-cbind(DBlocus,BinnedAlleles)

#sorting out individuals



inds<-unique(DBlocus$Sample)
ninds<-length(inds)
ltable<-matrix(nrow=ninds, ncol=3)
ltable<-data.frame(ltable)
names(ltable)<-c("Sample",paste(Loci[l],".a1",sep=""),paste(Loci[l],".a2",sep=""))

for(i in 1:ninds){

	ltable[i,1]<-as.character(inds[i])

	A<-DBlocus$BinnedAlleles[DBlocus$Sample==inds[i]]
	A<-as.numeric(as.character(A))

	if(length(A)<=2){
	ltable[i,2:3]<-A
	}else{ 
	warning(paste("more than two observations for sample",inds[i]))
	}


	}

#writing a table with the data from this marker using asign

assign(paste("Locus",l,".Alleles",sep=""),ltable)


}

wtable<-get(paste("Locus1",".Alleles",sep=""))

for(l in 1:(nloc-1)){
wtable<-merge(wtable,get(paste("Locus",l+1,".Alleles",sep="")),by="Sample",all.x=TRUE,all.y=T)
}

#writing to file
write.table(wtable,paste(file,"2 columns per locus.txt"),sep="\t",quote=FALSE, 
,na="",row.names=FALSE)


#writing a table with one column per locus

wtable2<-matrix(ncol=(nloc+1),nrow=nrow(wtable))
wtable2<-data.frame(wtable2)
wtable2[,1]<-wtable[,1]

for(l in 1:nloc){
	wtable2[,l+1]<-do.call(paste, c(wtable[c(paste(Loci[l],".a1",sep=""),paste(Loci[l],".a2",sep="")) ], sep = ""))
	}

wtable2[wtable2=="NANA"]<-NA
names(wtable2)<-c("Sample",as.character(Loci))

write.table(wtable2, paste(file,"1 column per locus.txt"),sep="\t",quote=FALSE, 
,na="",row.names=FALSE)


}




