\name{bin.limits}
\alias{bin.limits}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Writes the limits of bin classes and rounded bin codes }
\description{
  Gets the bin information necessary to plot bin limits on 
  the histogram of fragment sizes distribution
}
\usage{
bin.limits(DBase, locus)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{DBase}{ A data base with loci and fragment size information  }
  \item{locus}{ The relevant locus }
}

  
\value{
A list with components
\item{limits }{The limits of each bin in bp with two decimals cases}
  \item{bin }{The bin codes}
 
}
\author{ Filipe Alberto }
\references{
Alberto F. MsatAllele_1.0: an R package to visualize the binning of microsatellite alleles Journal of Heredity. 100(3):394,397
}

\seealso{  \code{\link{write.PG.file.loc}} }
\examples{
data(DBase)
bin.limits(DBase,"BC-4")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
