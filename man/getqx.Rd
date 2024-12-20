% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Q2q.R
\name{getqx}
\alias{getqx}
\title{getqx}
\usage{
getqx(Qx, nag)
}
\arguments{
\item{Qx}{Five-ages mortality rates which can be a vector created using combine function 'c()' or a column of a numerical matrix.}

\item{nag}{number of age groups.}
}
\value{
qx : a vector of age-specific mortality rates.

lx : a vector of the age evolution of the population of survivors.

dx : a vector of the theoretical distribution of deaths by age.

qxl : a vector of the age-specific mortality rates interpolated using the Lagrange method.

qxk : a vector of the age-specific mortality rates interpolated using the Karup-King method.

junct_age : the age where qxk and qxl have been joined.
}
\description{
It enables interpolating age-specific mortality rates from an annual life-table.
}
\examples{
getqx(Qx=LT[,8], nag=17)

}
\author{
Farid FLICI
}
