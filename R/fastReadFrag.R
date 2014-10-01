#' A rewrite of the read.frag.sizes function from the MsatAllele package
#' 
#' Kevin Keenan, 2014

fastReadFrag <- function(in.file, date, plate, long = FALSE){
  GMdata <- read.table(file = in.file, sep = "\t", header = TRUE)
  if(!long){
    TAB <- apply(GMdata, 1, function(x){
      if(is.na(x[4])){
        return(c(NA, NA, NA, NA, NA))
      } else if(x[4] == x[5]){
        return(c(x[c(3,1,4)], date, plate))
      } else {
        return(rbind(c(x[c(3,1,4)], date, plate),
                     c(x[c(3,1,5)], date, plate)))
      }
    })
  } else {
    TAB <- apply(GMdata, 1, function(x){
      if(is.na(x[4])){
        return(c(NA, NA, NA, NA, NA))
      } else {
        return(rbind(c(x[c(3,1,4)], date, plate),
                     c(x[c(3,1,5)], date, plate)))
      }
    })
  }
  TAB <- do.call("rbind", TAB)
  MS <- apply(TAB, 1, function(x) any(is.na(x)))
  colnames(TAB) <- c("Marker", "Sample", "Fragment", "Date", "Plate")
  TAB <- as.data.frame(TAB)
  TAB$Sample <- factor(TAB$Sample, levels(GMdata$Sample.Name))
  TAB$Fragment <- as.numeric(levels(TAB$Fragment))[TAB$Fragment]
  TAB <- TAB[!MS, ]
  return(TAB)
}