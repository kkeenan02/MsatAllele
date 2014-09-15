OrderByLocus <- function(DataBase, marker){
  # LocusDB <- DataBase[DataBase$Marker == marker, ]
  o <- order(DataBase$Fragment[DataBase$Marker == marker])
  out <- data.frame(Sample = DataBase$Sample[o],
                    Read = DataBase$Fragment[o])
  return(out)
}