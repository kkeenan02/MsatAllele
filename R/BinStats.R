BinStats <- function(DataBase, loci, limit = 0.8){
  o <- order(DataBase$Fragment[DataBase$Marker == loci])
  Frag <- DataBase$Fragment[DataBase$Marker == loci][o]
  if(is.list(limit)){
    lims <- do.call("rbind", limit)
  }
  Bin <- sapply(Frag, function(x){ 
    if(is.list(limit)){
      lim <- lims[which(lims[,1] == min(lims[,1][x <= lims[,1]])), 2]
    } else{
      lim = limit
    }
    getAllele(Frag, x, lim)
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