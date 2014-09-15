#' A rewrite of the read.frag.sizes function from the MsatAllele package
#' 
#' Kevin Keenan, 2014

fastReadFrag <- function(in.file, date, plate){
  GMdata <- read.table(file = in.file, sep = "\t", header = TRUE,
                       stringsAsFactors = FALSE)
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
  TAB <- do.call("rbind", TAB)
  MS <- apply(TAB, 1, function(x) any(is.na(x)))
  TAB <- TAB[!MS, ]
  colnames(TAB) <- c("Marker", "Sample", "Fragment", "Date", "Plate")
  TAB <- as.data.frame(TAB)
  TAB$Fragment <- as.numeric(TAB$Fragment)
  return(TAB)
}