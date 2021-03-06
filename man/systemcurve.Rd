% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/systemcurve.R
\name{systemcurve}
\alias{systemcurve}
\title{Creates a system curve for a piping system using the static head and 
a coefficient.}
\usage{
systemcurve(hs = NULL, K = NULL, units = c("SI", "Eng"))
}
\arguments{
\item{hs}{Numeric value of the static head [\eqn{m}{m} or \eqn{ft}{ft}]}

\item{K}{Numeric value of the coefficient in the equation \eqn{h = hs + {K}{Q}^2}
where Q has units of \eqn{m^3 s^{-1}}{m^3/s} or \eqn{ft^3 s^{-1}}{ft^3/s}}

\item{units}{character vector that contains the system of units [options are
\code{SI} for International System of Units and \code{Eng} for English (US customary)
units.}
}
\value{
Returns an object of class \code{systemcurve} consisting of a list including:
\itemize{
  \item curve - a function defining the system curve
  \item eqn - a character vector of the equation for the curve
  \item units - the units system passed to the function
}
}
\description{
Creates a system curve for a piping system using the static head and 
a coefficient.
}
\examples{

#Input in Eng units. Coefficient can be calculated manually or using 
#other package functions for friction loss in a pipe system using \eqn{Q=1}
ans <- darcyweisbach(Q = 1,D = 20/12, L = 3884, ks = 0.0005, nu = 1.23e-5, units = "Eng")
systemcurve(hs = 30, K = ans$hf, units = "Eng")

}
\author{
Ed Maurer
}
