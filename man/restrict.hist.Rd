\name{restrict.hist}
\alias{restrict.hist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Plots an histogram of a restricted range of microsatellite fragment sizes }
\description{
   Plots an histogram of a restricted range microsatellite fragment sizes (i.e. zooming) 
   obtained after the scoring of electrophoresis results
   }
\usage{
restrict.hist(DataBase, marker, MIN, MAX, limits=0.01, ticks=1 )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{DataBase}{ A data base with loci and fragment size information}
  \item{marker}{ The locus to be plotted }
  \item{MIN}{ The lower limit of the restricted range }
  \item{MAX}{ The upper limit of the restricted range  }
  \item{ticks}{ The interval between two consecutive tick marks on the x axis }
  \item{limits}{ The width of histogram classes }

}
\details{
  Allows zooming up on the distribution of fragment sizes for a given locus. It is usefull
  to check closelly for 1bp differentiated alleles, allowing to decide wwhich MIN and MAX to 
  use on test.table and subdataBase functions
}
\value{
    A restricted range histogram 

}
\author{ Filipe Alberto}
\references{
Alberto F. MsatAllele_1.0: an R package to visualize the binning of microsatellite alleles Journal of Heredity. 100(3):394,397
}
 
\seealso{  \code{\link{AlleleHist}}, \code{\link{subdataBase}}}
\examples{data(DBase)
	restrict.hist(DBase,"BC-4",228,229,0.01,1)

}

\keyword{ hplot }
