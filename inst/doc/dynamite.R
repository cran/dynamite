## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----srr, eval = FALSE, echo = FALSE------------------------------------------
#  #' @srrstats {BS1.2b} The package contains a vignette.

## ----setup, echo = FALSE------------------------------------------------------
library(dynamite)

## ---- echo=TRUE, eval=FALSE---------------------------------------------------
#  obs(y ~ lag(x), family = "gaussian") +
#  obs(x ~ z, family = "poisson")

## ---- echo=TRUE, eval=FALSE---------------------------------------------------
#  obs(y ~ z, family = "gaussian") +
#  obs(x ~ z, family = "poisson") +
#  lags(k = 1)

## ---- echo=TRUE, eval=FALSE---------------------------------------------------
#  obs(y ~ z + lag(x, k = 1), family = "gaussian") +
#  obs(x ~ z + lag(y, k = 1), family = "poisson")

## ---- echo=TRUE, eval=FALSE---------------------------------------------------
#  obs(y ~ lag(log1x), family = "gaussian") +
#  obs(x ~ z, family = "poisson") +
#  aux(numeric(log1x) ~ log(1 + x) | init(0))

## ---- echo=TRUE, eval=FALSE---------------------------------------------------
#  obs(y ~ lag(log1x), family = "gaussian") +
#  obs(x ~ z, family = "poisson") +
#  aux(numeric(log1x) ~ log(1 + x) | past(log(z)))

## ---- echo=TRUE, eval=FALSE---------------------------------------------------
#  dynamite(dformula, data, group = NULL, time, priors = NULL, verbose = TRUE,
#    debug = NULL, ...)
#  

## ---- echo=TRUE, eval=FALSE---------------------------------------------------
#  dynamite(
#    dformula = obs(x ~ varying(~ -1 + w), family = "poisson") + splines(df = 10),
#    data = d,
#    group = "id"
#    time = "year",
#    chains = 2,
#    cores = 2
#  )

## -----------------------------------------------------------------------------
library(dynamite)
head(multichannel_example)

## -----------------------------------------------------------------------------
set.seed(1)
f <- obs(g ~ lag(g) + lag(logp), family = "gaussian") +
  obs(p ~ lag(g) + lag(logp) + lag(b), family = "poisson") +
  obs(b ~ lag(b) * lag(logp) + lag(b) * lag(g), family = "bernoulli") +
  aux(numeric(logp) ~ log(p + 1))

## ---- eval = FALSE------------------------------------------------------------
#  multichannel_example_fit <- dynamite(
#    f, multichannel_example, "id", "time",
#    chains = 1, cores = 1, iter = 2000, warmup = 1000, init = 0, refresh = 0,
#    thin = 5, save_warmup = FALSE)

## ---- eval = FALSE------------------------------------------------------------
#  multichannel_example_fit <- update(multichannel_example_fit,
#      iter = 2000,
#      warmup = 1000,
#      chains = 4,
#      cores = 4,
#      refresh = 0,
#      save_warmup = FALSE
#  )

## ---- eval = TRUE, echo = FALSE-----------------------------------------------
fit <- dynamite(f, multichannel_example, "id", "time", 
                debug = list(no_compile = TRUE))

## -----------------------------------------------------------------------------
plot_betas(multichannel_example_fit)

## -----------------------------------------------------------------------------
d <- multichannel_example
d <- d |> 
  dplyr::mutate(
    g = ifelse(time > 5, NA, g),
    p = ifelse(time > 5, NA, p),
    b = ifelse(time > 5, NA, b))

## -----------------------------------------------------------------------------
newdata0 <- d |> 
  dplyr::mutate(
    b = ifelse(time == 5, 0, b))
pred0 <- predict(multichannel_example_fit, newdata = newdata0, type = "mean")
newdata1 <- d |> 
  dplyr::mutate(
    b = ifelse(time == 5, 1, b))
pred1 <- predict(multichannel_example_fit, newdata = newdata1, type = "mean")

## -----------------------------------------------------------------------------
head(pred0, n = 10) # first rows are NA as those were treated as fixed

## -----------------------------------------------------------------------------
sumr <- dplyr::bind_rows(
  list(b0 = pred0, b1 = pred1), .id = "case") |> 
  dplyr::group_by(case, .draw, time) |> 
  dplyr::summarise(mean_t = mean(g_mean)) |> 
  dplyr::group_by(case, time) |> 
  dplyr::summarise(
    mean = mean(mean_t),
    q5 = quantile(mean_t, 0.05, na.rm = TRUE), 
    q95 = quantile(mean_t, 0.95, na.rm = TRUE))

## ---- eval = FALSE------------------------------------------------------------
#  pred0b <- predict(multichannel_example_fit, newdata = newdata0, type = "mean",
#    funs = list(g_mean = list(mean_t = mean)))$simulated
#  pred1b <- predict(multichannel_example_fit, newdata = newdata1, type = "mean",
#    funs = list(g_mean = list(mean_t = mean)))$simulated
#  sumrb <- dplyr::bind_rows(
#    list(b0 = pred0b, b1 = pred1b), .id = "case") |>
#    dplyr::group_by(case, time) |>
#    dplyr::summarise(
#      mean = mean(mean_t_g_mean),
#      q5 = quantile(mean_t_g_mean, 0.05, na.rm = TRUE),
#      q95 = quantile(mean_t_g_mean, 0.95, na.rm = TRUE))

## -----------------------------------------------------------------------------
library(ggplot2)
ggplot(sumr, aes(time, mean)) + 
  geom_ribbon(aes(ymin = q5, ymax = q95), alpha = 0.5) +
  geom_line() + 
  scale_x_continuous(n.breaks = 10) + 
  facet_wrap(~ case) +
  theme_bw()

## -----------------------------------------------------------------------------
sumr_diff <- dplyr::bind_rows(
  list(b0 = pred0, b1 = pred1), .id = "case") |> 
  dplyr::group_by(.draw, time) |>
  dplyr::summarise(
    mean_t = mean(g_mean[case == "b1"] - g_mean[case == "b0"])) |>
  dplyr::group_by(time) |>
  dplyr::summarise(
    mean = mean(mean_t),
    q5 = quantile(mean_t, 0.05, na.rm = TRUE),
    q95 = quantile(mean_t, 0.95, na.rm = TRUE))

ggplot(sumr_diff, aes(time, mean)) +
  geom_ribbon(aes(ymin = q5, ymax = q95), alpha = 0.5) +
  geom_line() +
  scale_x_continuous(n.breaks = 10) +
  theme_bw()

