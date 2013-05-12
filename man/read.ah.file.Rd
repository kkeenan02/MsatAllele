\name{read.ah.file}
\alias{read.ah.file}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Reads .ah files from Strand and creates a R data base }
\description{
  Reads .ah files from Strand and creates a R data base object with scored microsatellite fragment sizes
}
\usage{
read.ah.file(ahfile, date, plate)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ahfile}{ A export .ah file from Strand }
  \item{date}{ A relevant date to be written on the data base}
  \item{plate}{ A sample plate code to be written on the data base }
}
\details{
  The Strand softare exports scored fragment sizes in a .ah file. This function will read those files
  and create the R data base that will be used by this package
}
\value{
  \item{Marker}{Genetic marker code}
  \item{Sample}{Sample code}
  \item{Read}{Fragment size}
  \item{Date}{Date}
  \item{Plate}{Strand plate file}
}
\author{ Filipe Alberto}
\references{
Alberto F. MsatAllele_1.0: an R package to visualize the binning of microsatellite alleles Journal of Heredity. 100(3):394,397
}
\seealso{ \code{\link{AlleleHist}}, \code{\link{restrict.hist}}, \code{\link{subdataBase}}}
\examples{

data(STRandOUT)
write.table(STRandOUT,"ah file simulated.txt",sep="\t",na="",quote=FALSE,col.names=FALSE,
            row.names=FALSE)
read.ah.file("ah file simulated.txt","25-2-2008","P1MX4MX5")

}

\keyword{ IO }
