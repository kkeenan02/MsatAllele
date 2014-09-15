BinStats <- function(DataBase, loci){
  o <- order(DataBase$Fragment[DataBase$Marker == loci])
  Frag <- DataBase$Fragment[o]
  LocusDBF <- OrderByLocus(DataBase, loci)
  Bin <- sapply(Frag, function(x){
    getAllele(LocusDBF, loci, x)
  })
  Bins  <-levels(as.factor(Bin))
  N     <-tapply(Bin,as.factor(Bin),length)
  Min   <-tapply(Frag,as.factor(Bin),min)
  Max   <-tapply(Frag,as.factor(Bin),max)
  Range <-Max-Min
  Sd    <-round(tapply(Frag,as.factor(Bin),sd),digits=3)
  MEAN  <-round(tapply(Frag,as.factor(Bin),mean),digits=2)
  MEDIAN<-round(tapply(Frag,as.factor(Bin),median),digits=2)
  Binstats<-data.frame(Bins  = Bins, N = N, Min = Min,
                       Max = Max, Range = Range, Sd = Sd,
                       MEAN = MEAN, MEDIAN = MEDIAN)
  return(Binstats)
}