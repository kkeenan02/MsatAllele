\name{OrderByLocus}
\alias{OrderByLocus}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ A function to order the fragment size vector for a given locus }
\description{
  Used by other functions to sort the fragment sizes
}
\usage{
OrderByLocus(DataBase, marker)
}
\arguments{
  \item{DataBase}{ A data base with loci and fragment size information }
  \item{marker}{ The locus to be sorted }
}
\value{
  An ordered fragment size vector for a given locus 
}
\author{ Filipe Alberto}

\references{
Alberto F. MsatAllele_1.0: an R package to visualize the binning of microsatellite alleles Journal of Heredity. 100(3):394,397
}
\seealso{  \code{\link{subdataBase}}, \code{\link{AlleleHist}} }
\examples{data(DBase)
OrderByLocus(DBase,"BC-4")
}

\keyword{ manip }
