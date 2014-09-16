getAllele <- function(fragRef, frag){
  dat <- getAll(fragRef, frag)
  j <- dat[[1]]
  Checkrange <- dat[[2]]
  Lcheck <- length(Checkrange)
  if(j < Lcheck - j){
    Allrange <- Checkrange[(j+1):Lcheck]
  } else {
    Allrange <- Checkrange[1:j] 
  }
  Allele.code <- round(median(Allrange), digits = 0)
  if(is.na(Allele.code)){
    return(round(frag))
  } else {
    return(Allele.code)
  }
}
