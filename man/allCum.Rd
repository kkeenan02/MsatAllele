\name{allCum}
\alias{allCum}
\title{ Plots a cumulative allele size distribution }
\description{
  This function plots a cumulative allele size distribution with number of observations on the x axis
  and allele size on the y axis. Each alternative allele bin is colored differently 
}
\usage{
allCum(DataBase, loci, ymin = NULL, ymax = NULL, c1 = "blue", 
       c2 = "orange", ytsize = 1, psize = 1,pch=1, limit = 0.8)
}
\arguments{
  \item{DataBase}{ A data base with loci and fragment size information}
  \item{loci}{ The locus to be plotted }
  \item{ymin}{ The minimum value in the allele size scale to plot}
  \item{ymax}{ The maximum value in the allele size scale to plot}
  \item{c1}{ color for the first allele and every other following ones  }
  \item{c2}{ color for the second allele and every other following ones }
  \item{ytsize}{ Text size for the y axis in proportion of the default value of one }
  \item{psize}{ Point size fin proportion of the default value of one }
  \item{pch}{ Type of plotting symbol }
  \item{limit}{Bin limits}

}
\value{
  The function plots a cumulative allele size distribution for a given locus. The y scale can be restricted to a given interval of allele size.
  This plot is similar to the plots implemented in Flexibin and Allelogram programs
}
\author{ Filipe Alberto }
\references{
Alberto F. MsatAllele_1.0: an R package to visualize the binning of microsatellite alleles Journal of Heredity. 100(3):394,397
}

\seealso{  \code{\link{AlleleHist}}, \code{\link{restrict.hist}} }
\examples{

data(DBase)
allCum(DBase,"BC-4", limit = 0.8)
}
\keyword{ hplot }
