\name{getpoints}
\alias{getpoints}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Interacts with AlleleCum graph to list observations }
\description{
  This function interacts with the graph created by AlleleCum, the user needs to 
  click on the y axis intersections (allele size) of that graph to define the minimum 
  and maximum values of the range of observations to list.
}
\usage{
getpoints(Database, marker)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Database}{ A data base with loci and fragment size information }
  \item{marker}{ The locus to be sorted }
}
\details{
  The user need to call the graphical display of \code{\link{subdataBase}} first and 
  then use getpoints. You need to click select first the minimum value for the range of fragment
  sizes to list, by clicking somewhere on the plot area intersecting that y axis value,
  and then reposition the mouse cursor to the maximum limit value to list.
}
\value{
  \item{DFrow}{The row.name information for that fragment in the data base}
  \item{Sample }{The Sample code}
  \item{Reading }{The fragment size}
  \item{Gel }{The Strand file for that fragment }
}
\author{ Filipe Alberto }
\references{
Alberto F. MsatAllele_1.0: an R package to visualize the binning of microsatellite alleles Journal of Heredity. 100(3):394,397
}
\seealso{ \code{\link{subdataBase}}, \code{\link{AlleleCum}}}
\examples{
data(DBase)
AlleleCum(DBase,"BC-3")
getpoints(DBase,"BC-3")

}

\keyword{ manip }
