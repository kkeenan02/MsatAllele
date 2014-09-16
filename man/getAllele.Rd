\name{getAllele}
\alias{getAllele}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Gets the allele code from an electrophoresis fragment size }
\description{
  Rounds a fragment size with two decimal of a bp to an integer allele code
}
\usage{
getAllele(fragRef, frag, limit)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fragRef}{A vector of fragments for a given locus}
  \item{frag}{A double, which is the fragment to be assigned to a bin}
  \item{limit}{Bin limits}
}
\details{
  The algorithm implemented searchs the database for all observed fragments within the interval set by
  -0.8 to + 0.8 bp of the fragment. It proceeds to detect breaks in the distribution of observed distribution
  to find the distribution where the fragment belongs. To get the allele code the median of that distribution
  is rounded to an iteger value
}
\value{
  An integer allele code
}
\author{Kevin Keenan}
\references{
Alberto F. MsatAllele_1.0: an R package to visualize the binning of microsatellite alleles Journal of Heredity. 100(3):394,397
}

\seealso{  \code{\link{write.PG.file.loc}} }
\examples{


data(DBase)
fragRef <- DBase$Fragment[DBase$Marker == "BC-16"]
getAllele(fragRef, 142.54, 0.8)
}


\keyword{ manip }
