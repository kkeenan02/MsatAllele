\name{InputFile}
\alias{InputFile}
\docType{data}
\title{ A data matrix similar to the tab delimited text file such as needed to data input}
\description{
  A data table with the following fields: 1st column-sample name; 2nd column-Panel or test name 
  (specific combination of loci in that sample); 3rd column-Locus name; 4th column-fragment 
  size for first allele; 5th column-fragment size for 2nd allele.
}
\usage{data(InputFile)}
\format{
  A data frame with 480 observations on the following 5 variables.
  \describe{
   \item{\code{Sample.Name}}{Sample names}
   \item{\code{Panel}}{The panel or test (a group of loci used to genotype the sample }
   \item{\code{Marker}}{The loci's vector}
   \item{\code{Size.1}}{Fragment size for the first allele scored}
   \item{\code{Size.2}}{Fragment size for the second allele scored, homozygotes have the same value has the preceding item}
  }
}
\details{
  This table format can be easily generated from output files from peak calling software
}
\author{ Filipe Alberto }
\references{
Alberto F. MsatAllele_1.0: an R package to visualize the binning of microsatellite alleles Journal of Heredity. 100(3):394,397
}
\examples{
data(InputFile)
}
\keyword{datasets}
