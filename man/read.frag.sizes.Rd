\name{read.frag.sizes}
\alias{read.frag.sizes}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Reads tab delimited text files with input fragment sizes creates a R data base }
\description{
  Reads input text files with continuous fragment size data and creates an R data base.
}
\usage{
read.frag.sizes(in.file, date, plate)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{in.file}{ The name of the input file }
  \item{date}{ A relevant date to be written on the data base }
  \item{plate}{ A sample plate code to be written on the data base }
}
\details{
  This functions reads tab delimited files with the following fields: 1st column-sample name;
  2nd column-Panel or test name (specific combination of loci in that sample); 3rd column-Locus name; 
  4th column-fragment size for first allele; 5th column-fragment size for 2nd allele.
}
\value{
 \item{Marker}{Genetic marker code}
 \item{Sample}{Sample code} 
 \item{Read}{Fragment size} 
 \item{Date}{Date} 
 \item{Plate}{Plate code} 
}
\author{ Filipe Alberto}
\references{
Alberto F. MsatAllele_1.0: an R package to visualize the binning of microsatellite alleles Journal of Heredity. 100(3):394,397
}

\seealso{ \code{\link{read.ah.file}}, \code{\link{AlleleCum}}, \code{\link{subdataBase}} }
\examples{
data(InputFile)
write.table(InputFile,"Input file.txt",sep="\t",na="",quote=FALSE,col.names=FALSE,row.names=FALSE)
read.frag.sizes("Input file.txt","25-2-2008","M1MpMetapop")
}

\keyword{ IO }
