\name{mark.bins}
\alias{mark.bins}
\title{ Adds bin limits to histograms of fragment size distribution }
\description{
  plots bin limits on top of the histogram of observed fragment sizes for 
  a given locus
}
\usage{
mark.bins(bins, text.size = 1, yscale = 5, offtext = 0.5)
}
\arguments{
  \item{bins}{ A list of the type returned by \code{\link{bin.limits}} function }
  \item{text.size}{ The size of text showing the bin codes }
  \item{yscale}{ The height of the y axis }
  \item{offtext}{ The offset tvalue for the text }
}

\value{
  Lines and bin codes are added to the plot
}
\author{ Filipe Alberto}
\references{
Alberto F. MsatAllele_1.0: an R package to visualize the binning of microsatellite alleles Journal of Heredity. 100(3):394,397
}
\examples{

data(DBase)
bins<-bin.limits(DBase,"BC-4")
mark.bins(bins)
}

\keyword{ aplot }
