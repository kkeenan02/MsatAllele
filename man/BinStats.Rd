\name{BinStats}
\alias{BinStats}

\title{ Summary bin statistics }
\description{
  Writes a data frame with summary statistics for each bin of a given locus
}
\usage{
BinStats(DataBase, loci, limit)
}
\arguments{
  \item{DataBase}{ A data base with loci and fragment size information }
  \item{loci}{ The locus of interest }
  \item{limit}{Bin limits}
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
BinStats(DBase,"BC-4", 0.8)
}

\keyword{ univar }