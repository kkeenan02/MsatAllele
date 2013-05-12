\name{DefBinLim}
\alias{DefBinLim}
\title{ Lets the used define bin limits}
\description{
  This function lets the user define bin limits for a given locus by clicking on top of 
  an \code{\link{AlleleCum}} graph
}
\usage{
DefBinLim(marker,append2=NULL,alleles)
}
\arguments{
  \item{marker}{ The name of the locus in the open \code{\link{AlleleCum}} graph}
  \item{append2}{ A data.frame like the one produced by this function to where further bin limits may be appended }
  \item{alleles}{ A numerical vector containing the allele labels for the alleles you want to provide limits}

}
\details{
  Upon opening an AlleleCum graph and not being happy with MsatAllele automatic bin definition you can  
  use this function to set bin limits to each allele. You need to set the lower limits for each bin in the  
  AlleleCum (not only for those you want to change). If the locus has many alleles, making it hard to  
  click on the lower limits of each allele, you can use this funtion sequentially making use of the parameter  
  append2,and the ymin and ymax parameters in \code{\link{AlleleCum}} function. Remember to set append2 to the 
  same object created in the first call to this function. Also remember to set the alleles argument to the 
  new set of alleles. 
}
\value{
  A data.frame with three columns, marker, alleles (the alleles argument provided) and LowerBin containig the lower
  bin limits set by the user
}
\author{ Filipe Alberto }
\references{
Alberto F. MsatAllele_1.0: an R package to visualize the binning of microsatellite alleles Journal of Heredity. 100(3):394,397
}

\seealso{  \code{\link{AlleleCum}} }
\examples{
\donttest{

data(DBase)
AlleleCum(DBase,"BC-4")
alleles.BC4<-c(222,224,226,228,230,232,234,236,240,242,244,247)
DefBinLim(DBase,append2=NULL,alleles=alleles.BC4)

}
}
\keyword{ manip }
