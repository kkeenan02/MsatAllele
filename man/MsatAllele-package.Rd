\name{MsatAllele-package}
\alias{MsatAllele-package}
\alias{MsatAllele}
\docType{package}
\title{
Visualizes and scores microsatellite alleles from electrophoresis fragment sizes}
\description{
	The package contains functions to: 1) load ah files from Strand software and build a R data base; 2) plot different
	graphical displays of the fragment sizes obtained for each locus and their bins; 3) interact with graphs to sort the data
	base for a given locus and range, allowing to easily trace back a particular group of samples to its original Strand file; bin
	fragment size data into population genetics files. 5) From version 1.04 on, the user can set manually bin limits
	and write files combining the manual and automatic methods to bin alleles.
}
\details{
\tabular{ll}{
Package: \tab MsatAllele\cr
Type: \tab Package\cr
Version: \tab 1.04\cr
Date: \tab 2013-05-12\cr
License: \tab GPL (>=2)\cr
}

The package contains functions to: 1) load ah files from Strand software and build a R data 
	base; 2) plot different graphical displays of the fragment sizes obtained for each
	locus and their bins;  3) sort the data base for a given locus and range, allowing to easily  trace back  
	particular samples to its original Strand file; 4) write a population genetics input file from the data base.
	5) Capacity to define bins manually is a new feature since MsatAllele 1.04
}
\author{
Filipe Alberto

Maintainer: Filipe Alberto <albertof@uwm.edu>

}
\references{
Alberto F. MsatAllele_1.0: an R package to visualize the binning of microsatellite alleles Journal of Heredity. 100(3):394,397
}

\keyword{ package }

\examples{

data(DBase)
AlleleHist(DBase,"BC-4",0.01,1)
subdataBase(DBase,"BC-4",227,233)
}
