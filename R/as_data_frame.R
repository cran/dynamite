#' Extract Samples From a `dynamitefit` Object as a Data Frame
#'
#' Provides a `data.frame` representation of the posterior samples of the model
#' parameters.
#'
#' The arguments `responses` and `types` can be used to extract only a subset
#' of the model parameters (i.e., only certain types of parameters related to a
#' certain response variable).
#'
#' Potential values for the `types` argument are:
#'
#'  * `alpha`\cr Intercept terms (time-invariant or time-varying).
#'  * `beta`\cr Time-invariant regression coefficients.
#'  * `delta`\cr Time-varying regression coefficients.
#'  * `nu`\cr Group-level random effects.
#'  * `lambda`\cr Factor loadings.
#'  * `psi`\cr Latent factors.
#'  * `tau`\cr Standard deviations of the spline coefficients of `delta`.
#'  * `tau_alpha`\cr Standard deviations of the spline coefficients of
#'    time-varying `alpha`.
#'  * `xi`\cr Common time-varying shrinkage factor for splines.
#'  * `sigma_nu`\cr Standard deviations of the random effects `nu`.
#'  * `corr_nu`\cr Pairwise within-group correlations of random effects `nu`.
#'     Samples of the full correlation matrix can be extracted manually as
#'     `rstan::extract(fit$stanfit, pars = "corr_matrix_nu")` if necessary.
#'  * `sigma_lambda`\cr Standard deviations of the latent factor loadings
#'    `lambda`.
#'  * `tau_psi`\cr Standard deviations of the the spline coefficients of `psi`.
#'  * `corr_psi`\cr Pairwise correlations of the latent factors.
#'     Samples of the full correlation matrix can be extracted manually as
#'     `rstan::extract(fit$stanfit, pars = "corr_matrix_psi")` if necessary.
#'  * `sigma`\cr Standard deviations of gaussian responses.
#'  * `phi`\cr Dispersion parameters of negative binomial responses.
#'  * `omega`\cr Spline coefficients of the regression coefficients `delta`.
#'  * `omega_alpha`\cr Spline coefficients of time-varying `alpha`.
#'  * `omega_psi`\cr Spline coefficients of the latent factors `psi`.
#'
#' @export
#' @param x \[`dynamitefit`]\cr The model fit object.
#' @param row.names Ignored.
#' @param optional Ignored.
#' @param parameters \[`character()`]\cr Parameter(s) for which the samples
#'   should be extracted. Possible options can be found with function
#'   `get_parameter_names()`. Default is all parameters of specific type for all
#'   responses.
#' @param responses \[`character()`]\cr Response(s) for which the samples
#'   should be extracted. Possible options are elements of
#'   `unique(x$priors$response)`, and the default is this entire vector.
#'    Ignored if the argument `parameters` is supplied.
#' @param types \[`character()`]\cr Type(s) of the parameters for which the
#'   samples should be extracted. See details of possible values. Default is
#'   all values listed in details except spline coefficients `omega`,
#'   `omega_alpha`, and `omega_psi`. See also [dynamite::get_parameter_types()].
#'    Ignored if the argument `parameters` is supplied.
#' @param summary \[`logical(1)`]\cr If `TRUE`, returns posterior
#'   mean, standard deviation, and posterior quantiles (as defined by the
#'   `probs` argument) for all parameters. If `FALSE` (default), returns the
#'   posterior samples instead.
#' @param probs \[`numeric()`]\cr Quantiles of interest. Default is
#'   `c(0.05, 0.95)`.
#' @param include_fixed \[`logical(1)`]\cr If `TRUE` (default), time-varying
#'   parameters for `1:fixed` time points are included in the output as `NA`
#'   values. If `FALSE`, fixed time points are omitted completely
#'   from the output.
#' @param ... Ignored.
#' @return A `tibble` containing either samples or summary statistics of the
#'   model parameters in a long format. For a wide format, see
#'   [dynamite::as_draws()].
#' @examples
#' results <- as.data.frame(
#'   gaussian_example_fit,
#'   responses = "y",
#'   types = "beta",
#'   summary = FALSE
#' )
#'
#' #' # Basic summaries can be obtained automatically with summary = TRUE:
#' as.data.frame(
#'   gaussian_example_fit,
#'   responses = "y",
#'   types = "beta",
#'   summary = TRUE
#' )
#'
#' #' # Time-varying coefficients delta
#' as.data.frame(gaussian_example_fit,
#'   responses = "y",
#'   types = "delta",
#'   summary = TRUE
#' )
#'
#' if (requireNamespace("dplyr") &&
#'   requireNamespace("tidyr") &&
#'   base::getRversion() >= "4.1.0") {
#'
#'   results |>
#'     dplyr::group_by(parameter) |>
#'     dplyr::summarise(mean = mean(value), sd = sd(value))
#'
#'   # Compute MCMC diagnostics via posterior package
#'   # For this we need to first convert to wide format
#'   # and then to draws_df object
#'   results |>
#'     dplyr::select(parameter, value, .iteration, .chain) |>
#'     tidyr::pivot_wider(values_from = value, names_from = parameter) |>
#'     posterior::as_draws() |>
#'     posterior::summarise_draws()
#'
#'   as.data.frame(gaussian_example_fit,
#'     responses = "y", types = "delta", summary = FALSE
#'   ) |>
#'     dplyr::select(parameter, value, time, .iteration, .chain) |>
#'     tidyr::pivot_wider(
#'       values_from = value,
#'       names_from = c(parameter, time),
#'       names_sep = "_t="
#'     ) |>
#'     posterior::as_draws() |>
#'     posterior::summarise_draws()
#' }
#'
as.data.frame.dynamitefit <- function(x, row.names = NULL, optional = FALSE,
                                      parameters = NULL, responses = NULL,
                                      types = NULL,
                                      summary = FALSE, probs = c(0.05, 0.95),
                                      include_fixed = TRUE, ...) {
  out <- as.data.table.dynamitefit(
    x,
    keep.rownames = FALSE,
    row.names,
    optional,
    parameters,
    responses,
    types,
    summary,
    probs,
    include_fixed,
    ...
  )
  tibble::tibble(data.table::setDF(out))
}
