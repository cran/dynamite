% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.R
\name{plot_nus}
\alias{plot_nus}
\title{Plot Random effects of a Dynamite Model}
\usage{
plot_nus(x, parameters = NULL, responses = NULL, level = 0.05, groups = NULL)
}
\arguments{
\item{x}{[\code{dynamitefit}]\cr The model fit object}

\item{parameters}{[\code{charecter()}]\ Parameter name(s) for which the plots
should be drawn. Possible options can be found with function
\code{get_parameter_names(types = "delta")}.}

\item{responses}{[\code{character()}]\cr Response(s) for which the coefficients
should be drawn. Possible options are elements of
\code{unique(x$priors$response)}, and the default is this whole vector.}

\item{level}{[\code{numeric(1)}]\cr Level for posterior intervals.
Default is 0.05, leading to 90\% intervals.}

\item{groups}{Group name(s) for which the plots should be drawn.
Default is all groups.}
}
\value{
A \code{ggplot} object.
}
\description{
Note that as this function tries to draw a plot containing effects of all
groups, the plot will become messy with large number of groups.
}
\examples{
data.table::setDTthreads(1) # For CRAN
plot_nus(gaussian_example_fit)

}
\seealso{
Drawing plots
\code{\link{plot.dynamitefit}()},
\code{\link{plot_betas}()},
\code{\link{plot_deltas}()},
\code{\link{plot_lambdas}()},
\code{\link{plot_psis}()}
}
\concept{plotting}
