#' Traceplots and Density Plots for a `dynamitefit` Object
#'
#' Produces the traceplots and the density plots of the model parameters.
#'
#' @export
#' @family plotting
#' @param x \[`dynamitefit`]\cr The model fit object.
#' @param parameters \[`charecter()`]\ Parameter name(s) for which the plots
#'   should be drawn. Possible options can be found with the function
#'   [dynamite::get_parameter_names()]. The default is all parameters ofa
#'   specific type for all responses, which can lead to too crowded a plot.
#' @param type \[`character(1)`]\cr Type of the parameter for which the plots
#'   should be drawn. Possible options can be found with the function
#'   [dynamite::get_parameter_types()]. Ignored if the argument `parameters`
#'   is supplied.
#' @param responses \[`character()`]\cr Response(s) for which the plots should
#'   be drawn. Possible options are `unique(x$priors$response)`. Default is
#'   all responses. Ignored if the argument `parameters` is supplied.
#' @param ... Not used..
#' @return A `ggplot` object.
#' @srrstats {BS6.1, RE6.0, RE6.1, BS6.2, BS6.3, BS6.5} Implements the `plot`
#' method. Further plots can be easily constructed with the help of `as_draws`
#' combined with `ggplot2`, for example.
#' @examples
#' data.table::setDTthreads(1) # For CRAN
#' plot(gaussian_example_fit, type = "beta")
#'
plot.dynamitefit <- function(x, parameters = NULL, type = NULL,
  responses = NULL, ...) {
  stopifnot_(
    !is.null(parameters) || !is.null(type),
    "Both arguments {.arg parameters} and {.arg type} are missing, you should
    supply one of them."
  )
  stopifnot_(
    is.null(type) || checkmate::test_string(x = type, na.ok = FALSE),
    "Argument {.arg type} must be a single {.cls character} string."
  )
  out <- suppressWarnings(
    as_draws_df.dynamitefit(x, parameters = parameters,
      responses = responses, types = type)
  )
  # avoid NSE notes from R CMD check
  parameter <- density <- .iteration <- .chain <- NULL
  vars <- setdiff(names(out), c(".chain", ".iteration", ".draw"))
  p_list <- vector(mode = "list", length = 2L * length(vars))
  for (i in seq_along(vars)) {
    v <- vars[i]
    d <- out[, c(v, ".chain", ".iteration", ".draw")]
    p_list[[2L * (i - 1L) + 1L]] <- ggplot2::ggplot(
      d,
      ggplot2::aes(x = !!rlang::sym(v), y = ggplot2::after_stat(density))
    ) +
      ggplot2::geom_density() +
      ggplot2::labs(x = v, y = "") +
      ggplot2::scale_x_continuous(expand = c(0.0, 0.0))
    p_list[[2L * i]] <- ggplot2::ggplot(
      d,
      ggplot2::aes(
        x = .iteration,
        y = !!rlang::sym(v),
        linetype = factor(.chain)
      )
    ) +
      ggplot2::geom_line() +
      ggplot2::labs(x = "", y = v) +
      ggplot2::guides(linetype = ggplot2::guide_legend(title = "Chain"))
  }
  patchwork::wrap_plots(
    p_list,
    ncol = 2L,
    nrow = length(vars),
    byrow = TRUE
  )
}

#' Plot Time-varying Regression Coefficients of a Dynamite Model
#'
#' @export
#' @family plotting
#' @param x \[`dynamitefit`]\cr The model fit object
#' @param parameters \[`charecter()`]\ Parameter name(s) for which the plots
#'   should be drawn. Possible options can be found with function
#'   `get_parameter_names(types = "delta")`.
#' @param responses \[`character()`]\cr Response(s) for which the coefficients
#'   should be drawn. Possible options are elements of
#'   `unique(x$priors$response)`, and the default is this whole vector.
#    Ignored if the argument `parameters` is supplied.
#' @param level \[`numeric(1)`]\cr Level for posterior intervals.
#'   Default is 0.05, leading to 90% intervals.
#' @param alpha \[`numeric(1)`]\cr Opacity level for `geom_ribbon`.
#'   Default is 0.5.
#' @param scales \[`character(1)`] Should y-axis of the panels be `"fixed"`
#'   (the default) or `"free"`? See [ggplot2::facet_wrap()].
#' @param include_alpha \[`logical(1)`]\cr If `TRUE` (default), plots also
#'   the time-varying alphas if such parameters exists in the model.
#' @return A `ggplot` object.
#' @srrstats {G2.3a} Uses match.arg.
#' @srrstats {BS6.1, RE6.0, RE6.1, BS6.3} Implements the `plot` method.
#' @examples
#' data.table::setDTthreads(1) # For CRAN
#' plot_deltas(gaussian_example_fit, level = 0.025, scales = "free") +
#'   ggplot2::theme_minimal()
#'
plot_deltas <- function(x, parameters = NULL, responses = NULL, level = 0.05,
                        alpha = 0.5, scales = c("fixed", "free"),
                        include_alpha = TRUE) {
  stopifnot_(
    checkmate::test_character(
      x = parameters,
      any.missing = FALSE,
      min.len = 1L,
      null.ok = TRUE
    ),
    "Argument {.arg parameters} must be a {.cls character} vector."
  )
  stopifnot_(
    checkmate::test_number(
      x = level,
      lower = 0.0,
      upper = 1.0,
      na.ok = FALSE,
    ),
    "Argument {.arg level} must be a single
     {.cls numeric} value between 0 and 1."
  )
  stopifnot_(
    checkmate::test_number(
      x = alpha,
      lower = 0.0,
      upper = 1.0,
      na.ok = FALSE,
    ),
    "Argument {.arg alpha} must be a single
     {.cls numeric} value between 0 and 1."
  )
  if (!is.null(parameters)) {
    delta_names <- get_parameter_names(x, types = "delta")
    found_pars <- parameters %in% delta_names
    stopifnot_(all(found_pars),
      c(
        "Parameter{?s} {.var {parameters[!found_pars]}} not found or
        {?it is/they are} of wrong type:",
        `i` = 'Use {.fun get_parameter_names} with {.arg types = "delta"} to
        check suitable parameter names.'
      )
    )
  }
  coefs <- coef.dynamitefit(
    x,
    parameters = parameters,
    type = "delta",
    responses = responses,
    probs = c(level, 1 - level),
    include_alpha = include_alpha
  )
  stopifnot_(
    nrow(coefs) > 0L,
    "The model does not contain varying coefficients delta."
  )
  scales <- onlyif(is.character(scales), tolower(scales))
  scales <- try(match.arg(scales, c("fixed", "free")), silent = TRUE)
  stopifnot_(
    !inherits(scales, "try-error"),
    "Argument {.arg scales} must be either {.val fixed} or {.val free}."
  )
  title <- paste0(
    "Posterior mean and ",
    100 * (1 - 2 * level),
    "% intervals of the time-varying coefficients"
  )
  # avoid NSE notes from R CMD check
  time <- mean <- category <- parameter <- NULL
  if (any(!is.na(coefs$category))) {
    p <- ggplot2::ggplot(
      coefs,
      ggplot2::aes(
        time,
        mean,
        colour = category,
        fill = category
      )
    )
  } else {
    p <- ggplot2::ggplot(coefs, ggplot2::aes(time, mean))
  }
  p +
    ggplot2::geom_ribbon(
      ggplot2::aes(
        ymin = !!rlang::sym(paste0("q", 100 * level)),
        ymax = !!rlang::sym(paste0("q", 100 * (1 - level)))
      ),
      alpha = alpha,
      na.rm = TRUE
    ) +
    ggplot2::geom_line(na.rm = TRUE) +
    ggplot2::scale_x_continuous(limits = range(coefs$time)) +
    ggplot2::facet_wrap("parameter", scales = scales) +
    ggplot2::labs(title = title, x = "Time", y = "Value")
}

#' Plot Time-invariant Regression Coefficients of a Dynamite Model
#'
#' @export
#' @family plotting
#' @inheritParams plot_deltas
#' @param parameters \[`charecter()`]\ Parameter name(s) for which the plots
#'   should be drawn. Possible options can be found with function
#'   `get_parameter_names(types = "beta")`.
#' @param include_alpha \[`logical(1)`]\cr If `TRUE` (default), plots also
#'   the time-invariant alphas if such parameters exists in the model.
#' @return A `ggplot` object.
#' @srrstats {BS6.1, RE6.0, RE6.1, BS6.3} Implements the `plot` method.
#' @examples
#' data.table::setDTthreads(1) # For CRAN
#' plot_betas(gaussian_example_fit, level = 0.1)
#'
plot_betas <- function(x, parameters = NULL, responses = NULL, level = 0.05,
                       include_alpha = TRUE) {
  stopifnot_(
    checkmate::test_character(
      x = parameters,
      any.missing = FALSE,
      min.len = 1L,
      null.ok = TRUE
    ),
    "Argument {.arg parameters} must be a {.cls character} vector."
  )
  stopifnot_(
    checkmate::test_number(
      x = level,
      lower = 0.0,
      upper = 1.0,
      na.ok = FALSE,
    ),
    "Argument {.arg level} must be a single
     {.cls numeric} value between 0 and 1."
  )
  if (!is.null(parameters)) {
    beta_names <- get_parameter_names(x, types = "beta")
    found_pars <- parameters %in% beta_names
    stopifnot_(all(found_pars),
      c(
        "Parameter{?s} {.var {parameters[!found_pars]}} not found or
        {?it is/they are} of wrong type:",
        `i` = 'Use {.fun get_parameter_names} with {.arg types = "beta"} to
        check suitable parameter names.'
      )
    )
  }
  coefs <- coef.dynamitefit(
    x,
    parameters = parameters,
    type = "beta",
    responses = responses,
    probs = c(level, 1 - level),
    include_alpha = include_alpha
  )
  stopifnot_(
    nrow(coefs) > 0L,
    "The model does not contain fixed coefficients beta."
  )
  title <- paste0(
    "Posterior mean and ",
    100 * (1 - 2 * level),
    "% intervals of the time-invariant coefficients"
  )
  # avoid NSE notes from R CMD check
  time <- mean <- category <- parameter <- NULL
  if (any(!is.na(coefs$category))) {
    p <- ggplot2::ggplot(
      coefs,
      ggplot2::aes(
        mean,
        parameter,
        colour = category,
        group = category
      )
    )
  } else {
    p <- ggplot2::ggplot(coefs, ggplot2::aes(mean, parameter))
  }
  p +
    ggplot2::geom_pointrange(
      ggplot2::aes(
        xmin = !!rlang::sym(paste0("q", 100 * level)),
        xmax = !!rlang::sym(paste0("q", 100 * (1 - level)))
      ),
      position = ggplot2::position_dodge(0.5)
    ) +
    ggplot2::labs(title = title, x = "Value", y = "Parameter")
}

#' Plot Random effects of a Dynamite Model
#'
#' Note that as this function tries to draw a plot containing effects of all
#' groups, the plot will become messy with large number of groups.
#'
#' @export
#' @family plotting
#' @inheritParams plot_deltas
#' @param groups Group name(s) for which the plots should be drawn.
#'   Default is all groups.
#' @return A `ggplot` object.
#' @srrstats {BS6.1, RE6.0, RE6.1, BS6.3} Implements the `plot` method.
#' @examples
#' data.table::setDTthreads(1) # For CRAN
#' plot_nus(gaussian_example_fit)
#'
plot_nus <- function(x, parameters = NULL, responses = NULL, level = 0.05,
  groups = NULL) {
  stopifnot_(
    checkmate::test_character(
      x = parameters,
      any.missing = FALSE,
      min.len = 1L,
      null.ok = TRUE
    ),
    "Argument {.arg parameters} must be a {.cls character} vector."
  )
  stopifnot_(
    checkmate::test_number(
      x = level,
      lower = 0.0,
      upper = 1.0,
      na.ok = FALSE,
    ),
    "Argument {.arg level} must be a single
     {.cls numeric} value between 0 and 1."
  )
  if (!is.null(parameters)) {
    nu_names <- get_parameter_names(x, types = "nu")
    found_pars <- parameters %in% nu_names
    stopifnot_(all(found_pars),
      c(
        "Parameter{?s} {.var {parameters[!found_pars]}} not found or
        {?it is/they are} of wrong type:",
        `i` = 'Use {.fun get_parameter_names} with {.arg types = "nu"} to
        check suitable parameter names.'
      )
    )
  }
  coefs <- try(
    coef.dynamitefit(
      x,
      parameters = parameters,
      type = "nu",
      responses = responses,
      probs = c(level, 1 - level)
    ),
    silent = TRUE
  )
  stopifnot_(
    !inherits(coefs, "try-error"),
    "The model does not contain random effects nu."
  )
  coefs <- ifelse_(
    is.null(groups),
    coefs,
    coefs[coefs$group %in% groups, ]
  )
  # avoid NSE notes from R CMD check
  mean <- parameter <- NULL
  coefs$parameter <- glue::glue("{coefs$parameter}_{coefs$category}_{coefs$group}")
  # remove NAs from parameters which are not category specific
  coefs$parameter <- gsub("_NA", "", coefs$parameter)
  coefs$parameter <- factor(coefs$parameter, levels = coefs$parameter)
  title <- paste0(
    "Posterior mean and ",
    100 * (1 - 2 * level),
    "% intervals of the random intercepts"
  )
  ggplot2::ggplot(coefs, ggplot2::aes(mean, parameter)) +
    ggplot2::geom_pointrange(
      ggplot2::aes(
        xmin = !!rlang::sym(paste0("q", 100 * level)),
        xmax = !!rlang::sym(paste0("q", 100 * (1 - level)))
      )
    ) +
    ggplot2::labs(title = title, x = "Value", y = "Parameter")
}
#' Plot Factor Loadings of a Dynamite Model
#'
#' @export
#' @family plotting
#' @inheritParams plot_deltas
#' @return A `ggplot` object.
#' @srrstats {BS6.1, RE6.0, RE6.1, BS6.3} Implements the `plot` method.
#'
plot_lambdas <- function(x, responses = NULL, level = 0.05) {
  stopifnot_(
    checkmate::test_number(
      x = level,
      lower = 0.0,
      upper = 1.0,
      na.ok = FALSE,
    ),
    "Argument {.arg level} must be a single
     {.cls numeric} value between 0 and 1."
  )
  coefs <- try(
    coef.dynamitefit(
      x,
      type = "lambda",
      responses = responses,
      probs = c(level, 1 - level)
    ),
    silent = TRUE
  )
  stopifnot_(
    !inherits(coefs, "try-error"),
    "The model does not contain latent factor psi."
  )
  # avoid NSE notes from R CMD check
  time <- mean <- parameter <- NULL
  coefs$parameter <- glue::glue("{coefs$parameter}_{coefs$group}")
  coefs$parameter <- factor(coefs$parameter, levels = coefs$parameter)
  title <- paste0(
    "Posterior mean and ",
    100 * (1 - 2 * level),
    "% intervals of the latent factor loadings"
  )
  ggplot2::ggplot(coefs, ggplot2::aes(mean, parameter)) +
    ggplot2::geom_pointrange(ggplot2::aes(
      xmin = !!rlang::sym(paste0("q", 100 * level)),
      xmax = !!rlang::sym(paste0("q", 100 * (1 - level)))
    )) +
    ggplot2::labs(title = title, x = "Value", y = "Parameter")
}
#' Plot Latent Factors of a Dynamite Model
#'
#' @export
#' @family plotting
#' @param x \[`dynamitefit`]\cr The model fit object
#' @param responses  \[`character()`]\cr Response(s) for which the coefficients
#'   should be drawn. Possible options are elements of
#'   `unique(x$priors$response)`, and the default is this whole vector.
#' @param level \[`numeric(1)`]\cr Level for posterior intervals.
#'   Default is 0.05, leading to 90% intervals.
#' @param alpha \[`numeric(1)`]\cr Opacity level for `geom_ribbon`.
#'   Default is 0.5.
#' @param scales \[`character(1)`] Should y-axis of the panels be `"fixed"`
#'   (the default) or `"free"`? See [ggplot2::facet_wrap()].
#' @return A `ggplot` object.
#' @srrstats {G2.3a} Uses match.arg.
#' @srrstats {BS6.1, RE6.0, RE6.1, BS6.3} Implements the `plot` method.
#'
plot_psis <- function(
    x, responses = NULL, level = 0.05, alpha = 0.5,
    scales = c("fixed", "free")) {
  stopifnot_(
    checkmate::test_number(
      x = level,
      lower = 0.0,
      upper = 1.0,
      na.ok = FALSE,
    ),
    "Argument {.arg level} must be a single
     {.cls numeric} value between 0 and 1."
  )
  stopifnot_(
    checkmate::test_number(
      x = alpha,
      lower = 0.0,
      upper = 1.0,
      na.ok = FALSE,
    ),
    "Argument {.arg alpha} must be a single
     {.cls numeric} value between 0 and 1."
  )
  coefs <- coef.dynamitefit(
    x,
    type = "psi",
    responses = responses,
    probs = c(level, 1 - level)
  )
  stopifnot_(
    nrow(coefs) > 0L,
    "The model does not contain latent factor psi."
  )
  scales <- onlyif(is.character(scales), tolower(scales))
  scales <- try(match.arg(scales, c("fixed", "free")), silent = TRUE)
  stopifnot_(
    !inherits(scales, "try-error"),
    "Argument {.arg scales} must be either {.val fixed} or {.val free}."
  )
  title <- paste0(
    "Posterior mean and ",
    100 * (1 - 2 * level),
    "% intervals of the latent factors"
  )
  # avoid NSE notes from R CMD check
  time <- mean <- category <- NULL
  if (any(!is.na(coefs$category))) {
    p <- ggplot2::ggplot(
      coefs,
      ggplot2::aes(
        time,
        mean,
        colour = category,
        fill = category
      )
    )
  } else {
    p <- ggplot2::ggplot(coefs, ggplot2::aes(time, mean))
  }
  p +
    ggplot2::geom_ribbon(
      ggplot2::aes(
        ymin = !!rlang::sym(paste0("q", 100 * level)),
        ymax = !!rlang::sym(paste0("q", 100 * (1 - level)))
      ),
      alpha = alpha
    ) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap("parameter", scales = scales) +
    ggplot2::labs(title = title, x = "Time", y = "Value")
}
