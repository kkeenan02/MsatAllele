

bin.limits<-function(DBase,locus){




fragments<-DBase[DBase[,1]==locus,3]

St<-min(fragments)
End<-max(fragments)

frag.space<-seq(St,End,0.01)


old.bin<-get.allele(DBase,locus,St)
bins<-St
bin.lim<-old.bin


i<-1

repeat{

allele<-get.allele(DBase, locus, frag.space[i])

if(allele==old.bin)
{
i<-i+1
}else{
bins<-c(bins,frag.space[i])
bin.lim<-c(bin.lim,allele)
old.bin<-allele
i<-i+1
}

if(i>length(frag.space))break
}

bins<-bins-0.01




ord<-order(fragments)
o.fragments<-fragments[ord]

obs.bins<-c(bins[1],bins[2])

i<-2
vect<-1
repeat{
c1<-(o.fragments>=bins[i] & o.fragments<bins[i+1])
if(length(c1[c1 == TRUE])>0){
obs.bins<-c(obs.bins,bins[i],bins[i+1])
vect<-c(vect,i)
i<-i+1}else{
i<-i+1}
if(i>length(bins))break
}
obs.bins<-obs.bins[1:(length(obs.bins)-1)]

bin.round<-bins[vect]

bin.round<-bin.round+0.05



i<-1
bin<-1:length(bin.round)
repeat{
bin[i]<-get.allele(DBase,locus,bin.round[i])
i<-i+1
if(i>length(bin))break}

bin.list<-list(limits=obs.bins,bin=bin)
bin.list


}
