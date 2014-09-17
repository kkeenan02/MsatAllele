allCum <- function(DataBase, loci, ymin = NULL, ymax = NULL,
                   c1 = "blue", c2 = "orange", ytsize = 1, 
                   psize = 1, pch = 1, limit = 0.8){
#   DataBase = DB
#   loci = "Ssa85"
#   ymin = NULL
#   ymax = NULL
#   c1 = "blue"
#   c2 = "orange"
#   ytsize = 1
#   psize = 1
#   pch = 1
#   limit = 0.8
  o <- order(DataBase$Fragment[DataBase$Marker == loci])
  Frag <- DataBase$Fragment[DataBase$Marker == loci][o]
  #LocusDBF <- OrderByLocus(DataBase, loci)
  Bin <- sapply(Frag, function(x){
    getAllele(Frag, x, limit)
  })
  Color.vect <- 1:length(Frag)
  tempC <- c1
  Color.vect[1] <- c1
  i <- 2
  repeat{
    if(Bin[i]==Bin[i-1]){
      Color.vect[i] <- tempC
      i <- i + 1
    }else{
      ifelse(tempC == c1, tempC <- c2, ifelse(tempC == c2, tempC <- c1, 
                                              tempC <- c1))
      Color.vect[i] <- tempC
      i <- i + 1
    }
    if(i > length(Frag)) break
  }
  samps <- DataBase$Sample[DataBase$Marker == loci][o]
  
  if(is.null(ymin) & is.null(ymax)){
    Bin <- data.frame(Sample_ID = samps,
                      Sample = 1:length(Bin),
                      Fragment = Frag,
                      Bin = Bin,
                      col = as.factor(Color.vect)) 
  } else {
    Bin <- data.frame(Sample_ID = samps,
                      Sample = 1:length(Bin),
                      Fragment = Frag,
                      Bin = Bin,
                      col = as.factor(Color.vect))
    Bin <- Bin[(Bin$Frag >= ymin & Bin$Frag <= ymax),]
  }
  p <- ggplot(Bin, aes(x = Sample, y = Fragment, colour = col)) +
    geom_point() +
    scale_y_continuous(breaks = round(seq(min(dat$y), max(dat$y), by = 0.2),1))
  list(plt = p,
       df = Bin)
}