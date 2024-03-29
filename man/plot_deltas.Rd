% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.R
\name{plot_deltas}
\alias{plot_deltas}
\title{Plot Time-varying Regression Coefficients of a Dynamite Model}
\usage{
plot_deltas(
  x,
  parameters = NULL,
  responses = NULL,
  level = 0.05,
  alpha = 0.5,
  scales = c("fixed", "free"),
  include_alpha = TRUE
)
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

\item{alpha}{[\code{numeric(1)}]\cr Opacity level for \code{geom_ribbon}.
Default is 0.5.}

\item{scales}{[\code{character(1)}] Should y-axis of the panels be \code{"fixed"}
(the default) or \code{"free"}? See \code{\link[ggplot2:facet_wrap]{ggplot2::facet_wrap()}}.}

\item{include_alpha}{[\code{logical(1)}]\cr If \code{TRUE} (default), plots also
the time-varying alphas if such parameters exists in the model.}
}
\value{
A \code{ggplot} object.
}
\description{
Plot Time-varying Regression Coefficients of a Dynamite Model
}
\examples{
data.table::setDTthreads(1) # For CRAN
plot_deltas(gaussian_example_fit, level = 0.025, scales = "free") +
  ggplot2::theme_minimal()

}
\seealso{
Drawing plots
\code{\link{plot.dynamitefit}()},
\code{\link{plot_betas}()},
\code{\link{plot_lambdas}()},
\code{\link{plot_nus}()},
\code{\link{plot_psis}()}
}
\concept{plotting}
