## -----------------------------------------------------------------------------
#| label: srr
#| eval: false
#| echo: false
# #' @srrstats {BS1.2b} The package contains a vignette.


## -----------------------------------------------------------------------------
#| label: setup
#| echo: false
#| warning: false
library("dynamite")
library("ggplot2")
suppressPackageStartupMessages(library("dplyr"))
theme_set(theme_bw())
options(dplyr.summarise.inform = FALSE)
options(crayon.enabled = FALSE)
set.seed(0)
data.table::setDTthreads(1) # For CRAN
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)


## -----------------------------------------------------------------------------
#| label: dynamiteinstall
#| echo: true
#| eval: false
# install.packages("dynamite")
# library("dynamite")


## -----------------------------------------------------------------------------
#| label: multichannelhead
head(multichannel_example)


## -----------------------------------------------------------------------------
#| label: multichannelformula
multi_formula <- obs(g ~ lag(g) + lag(logp), family = "gaussian") +
  obs(p ~ lag(g) + lag(logp) + lag(b), family = "poisson") +
  obs(b ~ lag(b) * lag(logp) + lag(b) * lag(g), family = "bernoulli") +
  aux(numeric(logp) ~ log(p + 1) | init(0))


## -----------------------------------------------------------------------------
#| label: multichannelfitactual
#| echo: false
#| eval: true
multichannel_fit <- multichannel_example_fit

## -----------------------------------------------------------------------------
#| label: multichannelfit
#| echo: true
#| eval: false
# # Low number of iterations for CRAN
# multichannel_fit <- dynamite(
#   dformula = multi_formula,
#   data = multichannel_example, time = "time", group = "id",
#   chains = 1, cores = 1, iter = 2000, warmup = 1000,
#   init = 0, thin = 5, refresh = 0
# )


## -----------------------------------------------------------------------------
#| label: fig-multichannelbetas
#| out.width: "100%"
#| fig.width: 8
#| fig.height: 4
#| fig.align: "center"
#| fig.cap: "Posterior means and 90\\% posterior intervals of the time-invariant coefficients for the multivariate model."
library("ggplot2")
theme_set(theme_bw())
plot(multichannel_fit, types = "beta") +
  labs(title = "")


## -----------------------------------------------------------------------------
#| label: multichannelnewdata
multichannel_newdata <- multichannel_example |>
  mutate(across(g:b, ~ ifelse(time > 5, NA, .x)))


## -----------------------------------------------------------------------------
#| label: multichannelnewdatacausal
new0 <- multichannel_newdata |>
  mutate(b = ifelse(time == 5, 0, b))
pred0 <- predict(multichannel_fit, newdata = new0, type = "mean")
new1 <- multichannel_newdata |>
  mutate(b = ifelse(time == 5, 1, b))
pred1 <- predict(multichannel_fit, newdata = new1, type = "mean")


## -----------------------------------------------------------------------------
#| label: multichannelpredictions
head(pred0, n = 10) |>
  round(3)


## -----------------------------------------------------------------------------
#| label: multichannelcausalposterior
sumr <- list(b0 = pred0, b1 = pred1) |>
  bind_rows(.id = "case") |>
  group_by(case, .draw, time) |>
  summarize(mean_t = mean(g_mean)) |>
  group_by(case, time) |>
  summarize(
    mean = mean(mean_t),
    q5 = quantile(mean_t, 0.05, na.rm = TRUE),
    q95 = quantile(mean_t, 0.95, na.rm = TRUE)
  )


## -----------------------------------------------------------------------------
#| label: multichannelpredictfuns
pred0b <- predict(
  multichannel_fit, newdata = new0, type = "mean",
  funs = list(g = list(mean_t = mean))
)$simulated
pred1b <- predict(
  multichannel_fit, newdata = new1, type = "mean",
  funs = list(g = list(mean_t = mean))
)$simulated
sumrb <- list(b0 = pred0b, b1 = pred1b) |>
  bind_rows(.id = "case") |>
  group_by(case, time) |>
  summarize(
    mean = mean(mean_t_g),
    q5 = quantile(mean_t_g, 0.05, na.rm = TRUE),
    q95 = quantile(mean_t_g, 0.95, na.rm = TRUE)
  )


## -----------------------------------------------------------------------------
#| label: fig-multichannelvisual
#| out.width: "100%"
#| fig.width: 8
#| fig.height: 3.5
#| fig.align: "center"
#| fig.cap: "Expected causal effects of interventions $do(b_5 = 0)$ and $do(b_5 = 1)$ on $g_t$. The black lines show the posterior means and the gray areas show 90\\% posterior intervals."
ggplot(sumr, aes(time, mean)) +
  geom_ribbon(aes(ymin = q5, ymax = q95), alpha = 0.5) +
  geom_line(na.rm = TRUE) +
  scale_x_continuous(n.breaks = 10) +
  facet_wrap(~ case)


## -----------------------------------------------------------------------------
#| label: multichannelcausaldiff
sumr_diff <- list(b0 = pred0, b1 = pred1) |>
  bind_rows(.id = "case") |>
  group_by(.draw, time) |>
  summarize(
    mean_t = mean(g_mean[case == "b1"] - g_mean[case == "b0"])
  ) |>
  group_by(time) |>
  summarize(
    mean = mean(mean_t),
    q5 = quantile(mean_t, 0.05, na.rm = TRUE),
    q95 = quantile(mean_t, 0.95, na.rm = TRUE)
  )


## -----------------------------------------------------------------------------
#| label: fig-multichannelcausaldiffplot
#| out.width: "100%"
#| fig.width: 8
#| fig.height: 3.5
#| fig.align: "center"
#| fig.cap: "Difference between the expected causal effects $E(g_t | do(b_5 = 1)) - E(g_t | do(b_5 = 0))$. The black line shows the posterior mean and the gray area shows a 90\\% posterior interval."
ggplot(sumr_diff, aes(time, mean)) +
  geom_ribbon(aes(ymin = q5, ymax = q95), alpha = 0.5) +
  geom_line(na.rm = TRUE) +
  scale_x_continuous(n.breaks = 10)


## -----------------------------------------------------------------------------
#| label: modeldef
dform <- obs(y ~ lag(x), family = "gaussian") +
  obs(x ~ z, family = "poisson")


## -----------------------------------------------------------------------------
#| eval: false
#| echo: true
# obs(usage ~ -1 + law + random(~1) + varying(~1), family = "beta") +
# obs(fatalities ~ usage + densurb + densrur +
#   bac08 + mlda21 + lim65 + lim70p + income10000 + unemp + fueltax +
#   random(~1) + offset(log_miles), family = "negbin")


## -----------------------------------------------------------------------------
#| label: modeldefprint
print(dform)


## -----------------------------------------------------------------------------
#| eval: false
#| echo: true
# obs(y ~ x, family = "gaussian") +
#   obs(x ~ z, family = "poisson")


## -----------------------------------------------------------------------------
#| eval: false
#| echo: true
# obs(y ~ z, family = "gaussian") +
#   obs(x ~ z, family = "poisson") +
#   lags(k = 1)


## -----------------------------------------------------------------------------
#| eval: false
#| echo: true
# obs(y ~ z + lag(y, k = 1) + lag(x, k = 1), family = "gaussian") +
#   obs(x ~ z + lag(y, k = 1) + lag(x, k = 1), family = "poisson")


## -----------------------------------------------------------------------------
#| eval: false
#| echo: true
# obs(x ~ z + varying(~ -1 + w), family = "poisson")


## -----------------------------------------------------------------------------
#| eval: false
#| echo: true
# obs(x ~ -1 + z + varying(~ w), family = "poisson")


## -----------------------------------------------------------------------------
#| eval: false
#| echo: true
# obs(x ~ z + varying(~ -1 + w), family = "poisson")
# obs(x ~ -1 + fixed(~ z) + varying(~ -1 + w), family = "poisson")
# obs(x ~ fixed(~ z) + varying(~ -1 + w), family = "poisson")


## -----------------------------------------------------------------------------
#| label: modeldefinterceptwarning
#| warning: true
#| results: "hide"
obs(y ~ 1 + varying(~1), family = "gaussian")


## -----------------------------------------------------------------------------
#| eval: false
#| echo: true
# obs(x ~ z + random(~1 + z), family = "gaussian")


## -----------------------------------------------------------------------------
#| eval: false
#| echo: true
# obs(usage ~ -1 + law + random(~1) + varying(~1), family = "beta")


## -----------------------------------------------------------------------------
#| eval: false
#| echo: true
# obs(c(y1, y2, y3) ~ x, family = "mvgaussian")


## -----------------------------------------------------------------------------
#| eval: false
#| echo: true
# obs(c(y1, y2, y3) ~ 1 | x | lag(y1), family = "mvgaussian")


## -----------------------------------------------------------------------------
#| eval: false
#| echo: true
# obs(y ~ z + trials(n), family = "binomial") +
#   obs(x ~ z + offset(w), family = "poisson")


## -----------------------------------------------------------------------------
#| eval: false
#| echo: true
# obs(y ~ lag(log1x), family = "gaussian") +
#   obs(x ~ z, family = "poisson") +
#   aux(numeric(log1x) ~ log(1 + x) | init(0))


## -----------------------------------------------------------------------------
#| label: modeldefauxtypewarning
#| warning: true
#| results: "hide"
aux(log1x ~ log(1 + x) | init(0))


## -----------------------------------------------------------------------------
#| eval: false
#| echo: true
# obs(y ~ lag(log1x), family = "gaussian") +
#   obs(x ~ z, family = "poisson") +
#   aux(numeric(log1x) ~ log(1 + x) | past(log(z)))


## -----------------------------------------------------------------------------
#| label: fig-multichanneldagplot
#| fig.show: "hold"
#| out.width: "100%"
#| fig.cap: "DAGs for the multivariate model created using the `plot()` method for `\"dynamitefit\"` objects. Panel (a) shows the model structure including the auxiliary response variable `logp` while panel (b) shows the model structure where the auxiliary variable is not included. The latter DAG is obtained via a functional projection where the parents of `logp` become the parents of the children of `logp` and `logp` is removed from the graph at each timepoint."
#| fig.subcap:
#|   - ""
#|   - ""
plot(multi_formula)
plot(multi_formula, show_auxiliary = FALSE)


## -----------------------------------------------------------------------------
cat(plot(multi_formula, show_auxiliary = FALSE, tikz = TRUE))


## -----------------------------------------------------------------------------
#| eval: false
#| echo: true
# dynamite(
#   dformula, data, time, group = NULL, priors = NULL, backend = "rstan",
#   verbose = TRUE, verbose_stan = FALSE, stanc_options = list("O0"),
#   threads_per_chain = 1L, grainsize = NULL, custom_stan_model = NULL,
#   debug = NULL, ...
# )


## -----------------------------------------------------------------------------
#| eval: false
#| echo: true
# dynamite(
#   dformula = obs(x ~ varying(~ -1 + w), family = "poisson") +
#     splines(df = 10),
#   data = d, time = "year", group = "id",
#   chains = 2, cores = 2
# )


## -----------------------------------------------------------------------------
#| label: priordefs
get_priors(gaussian_example_fit)


## -----------------------------------------------------------------------------
#| label: gaussianexampleprint
print(gaussian_example_fit)


## -----------------------------------------------------------------------------
#| label: mcmcdiag
mcmc_diagnostics(gaussian_example_fit)


## -----------------------------------------------------------------------------
#| eval: false
#| echo: true
# as.data.frame.dynamitefit(
#   x, keep.rownames, row.names = NULL, optional = FALSE, types = NULL,
#   parameters = NULL, responses = NULL, times = NULL, groups = NULL,
#   summary = FALSE, probs = c(0.05, 0.95), include_fixed = TRUE, ...
# )


## -----------------------------------------------------------------------------
#| label: parameterposteriors
as.data.frame(
  gaussian_example_fit,
  responses = "y", types = "beta", summary = TRUE
)


## -----------------------------------------------------------------------------
#| label: gaussianmodelcode
cat(get_code(gaussian_example_fit, blocks = "parameters"))


## -----------------------------------------------------------------------------
#| eval: false
#| echo: true
# plot.dynamitefit(
#   x, plot_type = c("default", "trace", "dag"), types = NULL,
#   parameters = NULL, responses = NULL, groups = NULL, times = NULL,
#   level = 0.05, alpha = 0.5, facet = TRUE, scales = c("fixed", "free"),
#   n_params = NULL, ...
# )


## -----------------------------------------------------------------------------
#| label: fig-parameterposteriorplot
#| out.width: "100%"
#| fig.width: 10
#| fig.height: 4
#| fig.align: "center"
#| fig.cap: "Posterior means (black lines) and 90\\% posterior intervals (gray areas) for the time-varying coefficients for the response variable `y` in the `gaussian_example_fit` model. The panels from left to right show the time-varying intercept for `y`, the time-varying effect of `x` on `y`, and the time-varying effect of `lag(y)` (the previous time-point) on `y`."
plot(
  gaussian_example_fit,
  types = c("alpha", "delta"), scales = "free"
) +
  labs(title = "")


## -----------------------------------------------------------------------------
#| label: fig-gaussiantrace
#| out.width: "100%"
#| fig.width: 9
#| fig.height: 4
#| fig.align: "center"
#| fig.cap: "Marginal posterior density and traceplot of the MCMC chains of the time-invariant regression coefficient `beta_y_z` of `z` for the response variable `y` in the `gaussian_example_fit` model."
plot(gaussian_example_fit, plot_type = "trace", types = "beta")


## -----------------------------------------------------------------------------
#| eval: false
#| echo: true
# predict.dynamitefit(
#   object, newdata = NULL, type = c("response", "mean", "link"),
#   funs = list(), impute = c("none", "locf", "nocb"),
#   new_levels = c("none", "bootstrap", "gaussian", "original"),
#   global_fixed = FALSE, n_draws = NULL, thin = 1,
#   expand = TRUE, df = TRUE, ...
# )


## -----------------------------------------------------------------------------
#| label: fig-gaussianpred
#| out.width: "100%"
#| fig.width: 8
#| fig.height: 5
#| fig.align: "center"
#| fig.cap: "Posterior predictive samples for the first 4 groups of the `gaussian_example` data. Lines in red represent the observed values."
pred <- predict(gaussian_example_fit, n_draws = 50)
pred |>
  dplyr::filter(id < 5) |>
  ggplot(aes(time, y_new, group = .draw)) +
  geom_line(alpha = 0.5) +
  geom_line(aes(y = y), colour = "tomato") +
  facet_wrap(~ id)


## -----------------------------------------------------------------------------
#| label: gaussianpredfuns
pred_funs <- predict(
  gaussian_example_fit,
  funs = list(y = list(mean = mean, sd = sd))
)
head(pred_funs$simulated)


## -----------------------------------------------------------------------------
#| label: gaussianpredfunstypemean
pred_funs_mean <- predict(
  gaussian_example_fit,
  type = "mean",
  funs = list(y = list(mean = mean, sd = sd))
)
head(pred_funs_mean$simulated)

