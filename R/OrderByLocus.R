`OrderByLocus` <-
function(DataBase,marker){

LocusDB<-DataBase[DataBase[,1]==marker,]

o<-order(LocusDB[,3])
sample<-LocusDB[o,2]
read<-LocusDB[o,3]

LocusDBDF<-data.frame(

Sample=sample,
Read=read)

LocusDBDF
}

