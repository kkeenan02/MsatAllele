\name{BinStat}
\alias{BinStat}

\title{ Summary bin statistics }
\description{
  Writes a data frame with summary statistics for each bin of a given locus
}
\usage{
BinStat(DataBase, loci)
}
\arguments{
  \item{DataBase}{ A data base with loci and fragment size information }
  \item{loci}{ The locus of interest }
}
\details{
  This function writes to a table bin statistics: Bin code, number of observations, minimum size, maximum size,
   range, standard deviation, mean and median
}
\value{
  A data frame with bin summary statistics
}
\author{ Filipe Alberto }
\references{
Alberto F. MsatAllele_1.0: an R package to visualize the binning of microsatellite alleles Journal of Heredity. 100(3):394,397
}

\examples{
data(DBase)
BinStat(DBase,"BC-4")
}

\keyword{ univar }
