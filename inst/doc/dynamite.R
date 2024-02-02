## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----srr, eval = FALSE, echo = FALSE------------------------------------------
#  #' @srrstats {BS1.2b} The package contains a vignette.

## ----setup, echo = FALSE, warning=FALSE---------------------------------------
library("dynamite")
library("ggplot2")
suppressPackageStartupMessages(library("dplyr"))
theme_set(theme_bw())
options(dplyr.summarise.inform = FALSE)
options(crayon.enabled = FALSE)
options(dplyr.summarise.inform = FALSE)
set.seed(0)
data.table::setDTthreads(1) # For CRAN

## ----dynamiteinstall, echo=TRUE, eval=FALSE-----------------------------------
#  install.packages("dynamite")
#  library("dynamite")

## ----multichannelhead, echo=TRUE, eval=TRUE-----------------------------------
head(multichannel_example)

## ----multichannelformula, echo=TRUE, eval=TRUE--------------------------------
multi_formula <- obs(g ~ lag(g) + lag(logp), family = "gaussian") +
  obs(p ~ lag(g) + lag(logp) + lag(b), family = "poisson") +
  obs(b ~ lag(b) * lag(logp) + lag(b) * lag(g), family = "bernoulli") +
  aux(numeric(logp) ~ log(p + 1))

## ----multichannelfit, echo=TRUE, eval=FALSE-----------------------------------
#  multichannel_example_fit <- dynamite(
#    multi_formula, data = multichannel_example,
#    time = "time", group = "id",
#    chains = 1, cores = 1, iter = 2000, warmup = 1000, init = 0, refresh = 0,
#    thin = 5, save_warmup = FALSE)

## ----eval = FALSE-------------------------------------------------------------
#  multichannel_example_fit <- update(multichannel_example_fit,
#    iter = 2000,
#    warmup = 1000,
#    chains = 4,
#    cores = 4,
#    refresh = 0,
#    save_warmup = FALSE
#  )

## ----eval = TRUE, echo = FALSE------------------------------------------------
fit <- dynamite(
  multi_formula, data = multichannel_example, 
  time = "time", group = "id", debug = list(no_compile = TRUE))

## ----multichannelbetas, echo=TRUE, eval=TRUE, fig.width=7, fig.height=4, fig.cap="Posterior means and 90% intervals of the time-invariant coefficients for the multichannel model."----
library("ggplot2")
theme_set(theme_bw())
plot_betas(multichannel_example_fit) + labs(title = "")

## ----multichannelnewdata, echo=TRUE, eval=TRUE--------------------------------
multichannel_newdata <- multichannel_example |>
  mutate(across(g:b, ~ ifelse(time > 5, NA, .x)))

## ----multichannelnewdata_causal, echo=TRUE, eval=TRUE-------------------------
new0 <- multichannel_newdata |> mutate(b = ifelse(time == 5, 0, b))
pred0 <- predict(multichannel_example_fit, newdata = new0, type = "mean")
new1 <- multichannel_newdata |> mutate(b = ifelse(time == 5, 1, b))
pred1 <- predict(multichannel_example_fit, newdata = new1, type = "mean")

## ----multichannelpredictions, echo=TRUE, eval=TRUE----------------------------
head(pred0, n = 10) |> round(3)

## ----multichannelcausalposterior, echo=TRUE, eval=TRUE------------------------
sumr <- bind_rows(list(b0 = pred0, b1 = pred1), .id = "case") |>
  group_by(case, .draw, time) |>
  summarise(mean_t = mean(g_mean)) |>
  group_by(case, time) |>
  summarise(
    mean = mean(mean_t),
    q5 = quantile(mean_t, 0.05, na.rm = TRUE),
    q95 = quantile(mean_t, 0.95, na.rm = TRUE)
  )

## ----multichannelpredictfuns, echo=TRUE, eval=TRUE----------------------------
pred0b <- predict(
  multichannel_example_fit, newdata = new0, type = "mean",
  funs = list(g = list(mean_t = mean))
)$simulated
pred1b <- predict(
  multichannel_example_fit, newdata = new1, type = "mean",
  funs = list(g = list(mean_t = mean))
)$simulated
sumrb <- bind_rows(
  list(b0 = pred0b, b1 = pred1b), .id = "case") |>
  group_by(case, time) |>
  summarise(
    mean = mean(mean_t_g),
    q5 = quantile(mean_t_g, 0.05, na.rm = TRUE),
    q95 = quantile(mean_t_g, 0.95, na.rm = TRUE)
  )

## ----multichannelvisual, echo=TRUE, eval=TRUE, warning=FALSE, fig.width=7, fig.height=3.5, fig.cap="Expected causal effects of interventions \\(do(b_5 = 0)\\) and \\(do(b_5 = 1)\\) on \\(g_t\\)."----
ggplot(sumr, aes(time, mean)) +
  geom_ribbon(aes(ymin = q5, ymax = q95), alpha = 0.5) +
  geom_line() +
  scale_x_continuous(n.breaks = 10) +
  facet_wrap(~ case)

## ----multichannelcausaldiff, echo=TRUE, eval=TRUE-----------------------------
sumr_diff <- bind_rows(list(b0 = pred0, b1 = pred1), .id = "case") |>
  group_by(.draw, time) |>
  summarise(
    mean_t = mean(g_mean[case == "b1"] - g_mean[case == "b0"])
  ) |>
  group_by(time) |>
  summarise(
    mean = mean(mean_t),
    q5 = quantile(mean_t, 0.05, na.rm = TRUE),
    q95 = quantile(mean_t, 0.95, na.rm = TRUE)
  )

## ----multichannelcausaldiffplot, echo=TRUE, eval=TRUE, warning=FALSE, fig.width=7, fig.height=3.5, fig.cap="Difference of the expected causal effects \\(E(g_t | do(b_5 = 1)) - E(g_t | do(b_5 = 0)) \\)."----
ggplot(sumr_diff, aes(time, mean)) +
  geom_ribbon(aes(ymin = q5, ymax = q95), alpha = 0.5) +
  geom_line() +
  scale_x_continuous(n.breaks = 10)

## ----modeldef, echo=TRUE, eval=TRUE-------------------------------------------
dform <- obs(y ~ lag(x), family = "gaussian") +
  obs(x ~ z, family = "poisson")

## ----modeldefprint, echo=TRUE, eval=TRUE--------------------------------------
print(dform)

## ----validformula, echo=TRUE, eval=FALSE--------------------------------------
#  obs(y ~ x, family = "gaussian") +
#    obs(x ~ z, family = "poisson")

## ----cyclic_form, echo=TRUE, eval=TRUE, error=TRUE----------------------------
obs(y ~ x, family = "gaussian") +
  obs(x ~ z, family = "poisson") +
  obs(z ~ y, family = "categorical")

## ----globallags, echo=TRUE, eval=FALSE----------------------------------------
#  obs(y ~ z, family = "gaussian") +
#    obs(x ~ z, family = "poisson") +
#    lags(k = 1)

## ----equivalentlags, echo=TRUE, eval=FALSE------------------------------------
#  obs(y ~ z + lag(y, k = 1) + lag(x, k = 1), family = "gaussian") +
#    obs(x ~ z + lag(y, k = 1) + lag(x, k = 1), family = "poisson")

## ----varyingcomponent, echo=TRUE, eval=FALSE----------------------------------
#  obs(x ~ z + varying(~ -1 + w), family = "poisson")

## ----varyingintercept, echo=TRUE, eval=FALSE----------------------------------
#  obs(x ~ -1 + z + varying(~ w), family = "poisson")

## ----fixedcomponent, echo=TRUE, eval=FALSE------------------------------------
#  obs(x ~ z + varying(~ -1 + w), family = "poisson")
#  obs(x ~ -1 + fixed(~ z) + varying(~ -1 + w), family = "poisson")
#  obs(x ~ fixed(~ z) + varying(~ -1 + w), family = "poisson")

## ----modeldefinterceptwarning, echo=TRUE, eval=TRUE---------------------------
dform_warn <- obs(y ~ 1 + varying(~1), family = "gaussian")

## ----splinescomponent, echo=TRUE, eval=FALSE----------------------------------
#  obs(x ~ varying(~ -1 + w), family = "poisson") +
#    splines(df = 10)

## ----randomcomponent, echo=TRUE, eval=FALSE-----------------------------------
#  obs(x ~ z + random(~1 + z), family = "gaussian")

## ----formulawithaux, echo=TRUE, eval=FALSE------------------------------------
#  obs(y ~ lag(log1x), family = "gaussian") +
#    obs(x ~ z, family = "poisson") +
#    aux(numeric(log1x) ~ log(1 + x) | init(0))

## ----pastvalues, echo=TRUE, eval=FALSE----------------------------------------
#  obs(y ~ lag(log1x), family = "gaussian") +
#    obs(x ~ z, family = "poisson") +
#    aux(numeric(log1x) ~ log(1 + x) | past(log(z)))

## ----dynamitefunction, echo=TRUE, eval=FALSE----------------------------------
#  dynamite(
#    dformula, data, time, group = NULL,
#    priors = NULL, backend = "rstan",
#    verbose = TRUE, verbose_stan = FALSE, debug = NULL, ...
#  )

## ----dynamitecall, echo=TRUE, eval=FALSE--------------------------------------
#  dynamite(
#    dformula = obs(x ~ varying(~ -1 + w), family = "poisson") +
#      splines(df = 10),
#    data = d, time = "year", group = "id",
#    chains = 2, cores = 2
#  )

## ----priordefs, echo=TRUE, eval=TRUE------------------------------------------
get_priors(gaussian_example_fit)

## ----gaussian_example_print, echo=TRUE, eval=TRUE-----------------------------
print(gaussian_example_fit)

## ----mcmcdiag, echo=TRUE, eval=TRUE-------------------------------------------
mcmc_diagnostics(gaussian_example_fit)

## ----asdatatablemethod, echo=TRUE, eval=FALSE---------------------------------
#  as.data.frame.dynamitefit(
#    x, row.names = NULL, optional = FALSE, parameters = NULL, responses = NULL,
#    types = NULL, summary = FALSE, probs = c(0.05, 0.95), include_fixed = TRUE, ...
#  )

## ----parameter_posteriors, echo=TRUE, eval=TRUE-------------------------------
as.data.frame(
  gaussian_example_fit,
  responses = "y", types = "beta", summary = TRUE
)

## ----parameterposteriorplot, echo=TRUE, eval=TRUE, warning=FALSE, fig.width=7, fig.height=3.5, fig.cap="Posterior density and traceplots for the time-invariant regression coefficient of `z` in the model for the response variable `y` (parameter `beta_y_z`) in the `gaussian_example_fit` model."----
plot(gaussian_example_fit, responses = "y", type = "beta")

## ----gaussiandeltas, echo=TRUE, eval=TRUE, warning=FALSE, fig.width=7, fig.height=3.5, fig.cap="Posterior mean and 90\\% intervals of the time-varying coefficients for the `gaussian_example_fit` model. The panels from left to right show the time-varying intercept for `y`, the time-varying effect of `x` on `y`, and the time-varying effect of `lag(y)` (the previous time-point) on `y`."----
plot_deltas(gaussian_example_fit) + labs(title = "")

## ----gaussian_model_code, echo=TRUE, eval=TRUE--------------------------------
cat(get_code(gaussian_example_fit, blocks = "parameters"))

## ----predictmethod, echo=TRUE, eval=FALSE-------------------------------------
#  predict.dynamitefit(
#    object, newdata = NULL, type = c("response", "mean", "link"),
#    funs = list(), impute = c("none", "locf"),
#    new_levels = c("none", "bootstrap", "gaussian", "original"),
#    global_fixed = FALSE, n_draws = NULL, expand = TRUE,
#    df = TRUE, ...
#  )

## ----gaussianpred, echo=TRUE, eval=TRUE, fig.width=7, fig.height=4, fig.cap="Posterior predictive samples for the first 4 groups of the `gaussian_example` data. Lines in red represent the observed values."----
pred <- predict(gaussian_example_fit, n_draws = 50)
pred |>
  dplyr::filter(id < 5) |>
  ggplot(aes(time, y_new, group = .draw)) +
  geom_line(alpha = 0.5) +
  geom_line(aes(y = y), colour = "tomato") +
  facet_wrap(~ id)

## ----gaussianpredfuns, echo=TRUE, eval=TRUE-----------------------------------
pred_funs <- predict(
  gaussian_example_fit,
  funs = list(y = list(mean = mean, sd = sd))
)
head(pred_funs$simulated)

## ----gaussianpredmem, echo=TRUE, eval=TRUE------------------------------------
pred_full <- predict(gaussian_example_fit)
format(object.size(pred_full), "Mb")
format(object.size(pred_funs), "Mb")

