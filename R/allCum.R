 allCum <- function(DataBase, loci, ymin = NULL, ymax = NULL,
                       c1 = "black", c2 = "grey", ytsize = 1, 
                       psize = 1, pch = 1){
#   DataBase = readRDS("BASELINEDB.Rds")
#   loci = "Ssa85"
#   ymin = NULL
#   ymax = NULL
#   c1 = "black"
#   c2 = "grey"
#   ytsize = 1
#   psize = 1
#   pch = 1
  o <- order(DataBase$Fragment[DataBase$Marker == loci])
  Frag <- DataBase$Fragment[o]
  LocusDBF <- OrderByLocus(DataBase, loci)
  Bin <- sapply(Frag, function(x){
    getAllele(LocusDBF, loci, x)
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
  plot(Frag, type = "n", axes = FALSE, xlab = "Number of observations", 
       ylab = "Allele size (bp)", main = loci, ylim = c(ymin,ymax))
  par(cex = psize)
  points(Frag, col = Color.vect, pch = pch)
  par(cex = 1)
  axis(1, pos = min(Bin) - 1)
  par(cex = ytsize)
  axis(2, pos = 0, las = 2, at = as.numeric(levels(as.factor(Bin))))
  par(cex = 1)
}