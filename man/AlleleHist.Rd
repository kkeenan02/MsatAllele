\name{AlleleHist}
\alias{AlleleHist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Plots an histogram of microsatellite fragment sizes}
\description{
  Plots an histogram of microsatellite fragment sizes obtained after the scoring of electrophoresis results
  
  }
\usage{
AlleleHist(DataBase, marker, limits=0.01, ticks=1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{DataBase}{ A data base with loci and fragment size information }
  \item{marker}{ The locus to be plotted}
  \item{limits}{ The width of histogram classes }
  \item{ticks}{ The interval between two consecutive tick marks on the x axis }
}
\details{
  The generated plot is usefull to analyze the total variation of fragment sizes obtained for a 
  given locus. It will allow the detection of range were 1bp mutation may have occurred
}
\value{
  An histogram with the frequency of the fragment sizes scored for a given locus in the data Base
}
\author{ Filipe Alberto}

\references{
Alberto F. MsatAllele_1.0: an R package to visualize the binning of microsatellite alleles Journal of Heredity. 100(3):394,397
}
\seealso{  \code{\link{read.ah.file}}, \code{\link{restrict.hist}} }
\examples{
data(DBase)
AlleleHist(DBase,"BC-4",0.01,1)
}

\keyword{ hplot }
