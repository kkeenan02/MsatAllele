\name{STRandOUT}
\alias{STRandOUT}
\docType{data}
\title{ A table like the one exported in the Strand .ah file}
\description{
  A table like the one exported in the Strand .ah file
}
\usage{data(STRandOUT)}
\format{
  A data frame with 1440 observations on the following 10 variables.
  \describe{   
  \item{\code{V1}}{A column with both sample code and number}
  \item{\code{V2}}{The sample number, not sure what this means in the sequencer structure}
  \item{\code{V3}}{A factor with the colors use for each marker}
  \item{\code{V4}}{A factor with sample names}
  \item{\code{V5}}{A factor with marker names}
  \item{\code{V6}}{fragment sizes for the first allele scored}
  \item{\code{V7}}{fragment sizes for the second allele scored, if any (i.e. homozygotes have nothing here)}
  \item{\code{V8}}{Rounded allele codes, using the weird Strand algorithm}
  \item{\code{V9}}{a logical vector}
  \item{\code{V10}}{a logical vector}  
  }
}
\details{
  
}
\source{
}
\author{ Filipe Alberto}
\references{
Alberto F. MsatAllele_1.0: an R package to visualize the binning of microsatellite alleles Journal of Heredity. 100(3):394,397
}
\examples{data(STRandOUT)
	transDB(STRandOUT,"25-2-2008","Plate1-MX4MX5")
}
\keyword{datasets}
