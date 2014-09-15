BinStat<-function(DataBase,loci){
  
  
  o<-order(DataBase[DataBase[,1]==loci,3])
  
  Frag<-DataBase[DataBase[,1]==loci,3]
  Frag<-Frag[o]
  
  
  Bin<-1:length(Frag)
  i<-1
  
  repeat{
    Bin[i]<-get.allele(DataBase,loci,Frag[i])
    i<-i+1
    if(i>length(Frag))break}
  
  
  
  
  Bins  <-levels(as.factor(Bin))
  N     <-tapply(Bin,as.factor(Bin),length)
  Min   <-tapply(Frag,as.factor(Bin),min)
  Max   <-tapply(Frag,as.factor(Bin),max)
  Range <-Max-Min
  Sd    <-round(tapply(Frag,as.factor(Bin),sd),digits=3)
  MEAN  <-round(tapply(Frag,as.factor(Bin),mean),digits=2)
  MEDIAN<-round(tapply(Frag,as.factor(Bin),median),digits=2)
  
  
  Binstats<-data.frame(
    
    Bins  =Bins , 
    N     =N    , 
    Min   =Min  , 
    Max   =Max  , 
    Range =Range, 
    Sd    =Sd   , 
    MEAN  =MEAN , 
    MEDIAN=MEDIAN)
  
  Binstats
  
}