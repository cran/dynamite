% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.R
\name{plot.dynamitefit}
\alias{plot.dynamitefit}
\title{Traceplots and Density Plots of a \code{dynamitefit} Object}
\usage{
\method{plot}{dynamitefit}(x, parameters = NULL, type = NULL, responses = NULL, ...)
}
\arguments{
\item{x}{[\code{dynamitefit}]\cr The model fit object.}

\item{parameters}{[\code{charecter()}]\ Parameter name(s) for which the plots
should be drawn. Possible options can be found with function
\code{get_parameter_names()}. Default is all parameters of specific type for all
responses, which can lead to too crowded figure.}

\item{type}{[\code{character(1)}]\cr Type of the parameter for which the plots
should be drawn. Possible options can be found with function
\code{get_parameter_types()}. Ignored if the argument \code{parameters} is supplied.}

\item{responses}{[\code{character()}]\cr Response(s) for which the plots should
be drawn. Possible options are \code{unique(x$priors$response)}. Default is
all responses. Ignored if the argument \code{parameters} is supplied.}

\item{...}{Not used..}
}
\value{
A \code{ggplot} object.
}
\description{
Produces the traceplots and the density plots of the model parameters.
}
\examples{
plot(gaussian_example_fit, type = "beta")

}
