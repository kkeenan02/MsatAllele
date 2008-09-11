\name{MsatAllele-package}
\alias{MsatAllele-package}
\alias{MsatAllele}
\docType{package}
\title{
Visualizes and scores microsatellite alleles from electrophoresis fragment sizes}
\description{
The package contains functions to: 1) load ah files from Strand software and build a R data 
	base; 2) plot histograms of the fragment sizes obtained for each locus, and restricted range 
	histograms; 3) sort the data base for a given locus and range, allowing to easily  trace back 
	particular samples to its original Strand file; 4) write a population genetics input file from the data base.
}
\details{
\tabular{ll}{
Package: \tab MsatAllele\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2008-03-03\cr
License: \tab GPL (>=2)\cr
}

The package contains functions to: 1) load ah files from Strand software and build a R data 
	base; 2) plot histograms of the fragment sizes obtained for each locus, and restricted range 
	histograms; 3) sort the data base for a given locus and range, allowing to easily  trace back  
	particular samples to its original Strand file; 4) write a population genetics input file from the data base.
}
\author{
Filipe Alberto

Maintainer: Filipe Alberto <falberto@ualg.pt>

}
\references{
Alberto F. MsatAllele_1.0: an R package to visualize the binning of microsatellite alleles Journal of Heredity. 100(3):394,397
}

\keyword{ package }
\seealso{

}
\examples{

data(DBase)
AlleleHist(DBase,"BC-4",0.01,1)
subdataBase(DBase,"BC-4",227,233)
}
