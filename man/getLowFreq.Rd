\name{getLowFreq}
\alias{getLowFreq}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
This function allows users to lookup the allele database for individuals with allele that occur at low frequencies. The function is useful for highlighting individuals that may possess artifact alleles.
}
\description{
The function searches through the allele database for individuals with alleles that occur fewer than n times across all individuals.
}
\usage{
getLowFreq(database, marker, n, limit = 0.8)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{database}{The database}
  \item{marker}{The microsatellite marker of interest}
  \item{n}{The minimum number of allele observation}
  \item{limit}{Bin limit}
}

\value{
A database of all individuals with alleles that occur fewer than n times for a given marker
}
\references{
Alberto F. MsatAllele_1.0: an R package to visualize the binning of microsatellite alleles Journal of Heredity. 100(3):394,397
}
\author{
Kevin Keenan
}