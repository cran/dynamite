% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ndraws.R
\name{ndraws.dynamitefit}
\alias{ndraws.dynamitefit}
\alias{ndraws}
\title{Return the Number of Posterior Draws of a \code{dynamitefit} Object}
\usage{
\method{ndraws}{dynamitefit}(x)
}
\arguments{
\item{x}{[\code{dynamitefit}]\cr The model fit object.}
}
\value{
Number of posterior draws as a single \code{integer} value.
}
\description{
Return the Number of Posterior Draws of a \code{dynamitefit} Object
}
\examples{
ndraws(gaussian_example_fit)

}
