\name{subdataBase}
\alias{subdataBase}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Sorts the data base for a given locus and a restricted range }
\description{
  This function can be used to see the information for the fragments observed in a given the range of a
  particular loci. It is usefull to trace back samples with strange sizes to their original Strand files
}
\usage{
subdataBase(DataBase, marker, MIN=min(DataBase[,3]), MAX=max(DataBase[,3]))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{DataBase}{ A data base with loci and fragment size information }
  \item{marker}{ The locus to be sorted }
  \item{MIN}{ The lower limit of the restricted range }
  \item{MAX}{ The upper limit of the restricted range }
}
\details{
  Returns a list of observations from the data base for a given locus and range defined by the MIN and MAX 
  allele sizes arguments, with default values equal to the minimum and maximum observations recorded in the data base
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{DFrow}{The row.name information for that fragment in the data base}
  \item{Sample }{The Sample code}
  \item{Reading }{The fragment size}
  \item{Gel }{The Strand file for that fragment }
}

\author{ Filipe Alberto}
\references{
Alberto F. MsatAllele_1.0: an R package to visualize the binning of microsatellite alleles Journal of Heredity. 100(3):394,397
}

\seealso{\code{\link{AlleleHist}}, \code{\link{AlleleCum}}  }
\examples{data(DBase)
	subdataBase(DBase,"BC-4",228,232)

}
\keyword{ manip }
