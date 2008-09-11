

get.allele<-function(RefDB,Marker,fragment){


LocusDBF<-OrderByLocus(RefDB,Marker)


c1<-(LocusDBF[,2]>=fragment-0.8 & LocusDBF[,2]<=fragment+0.8)
Checkrange<-LocusDBF[c1,2]

j<-1
dif<-0

Lcheck<-length(Checkrange)

while(j<Lcheck )
{
dif<-Checkrange[j+1]-Checkrange[j]
if(dif>=0.4)break
j<-j+1
}

ifelse(j<Lcheck-j,Allrange<-Checkrange[(j+1):Lcheck],Allrange<-Checkrange[1:j])



Allele.code<-round(median(Allrange),digits=0)

ifelse(is.na(Allele.code),round(fragment,digits=0),Allele.code)
}
