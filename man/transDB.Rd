\name{transDB}
\alias{transDB}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Creates a data base object in the form used by other functions in the package}
\description{
  Creates a data base from an R object with structure identical to the .ah Strand export file
}
\usage{
transDB(STRandOUT, data, corrida)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{STRandOUT}{ an R object with structure identical to the .ah Strand export file }
  \item{data}{ the date entry field to be be written in the data base }
  \item{corrida}{ the plate code entry field to be be written in the data base }
}
\details{
  It is used internally to the read.ah.file function
}
\value{
  
   \item{Marker}{Genetic marker code}
   \item{Sample}{Sample code}
   \item{Read}{Fragment size}
   \item{Date}{Date}
   \item{Plate}{Strand plate file}



}
\author{Filipe Alberto}
\references{
 Alberto F. MsatAllele_1.0: an R package to visualize the binning of microsatellite alleles Journal of Heredity. 100(3):394,397
}
\examples{data(STRandOUT)
	transDB(STRandOUT,"25-2-2008","P1MX4MX5")
}

\keyword{manip}
