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

## -----------------------------------------------------------------------------
f <- obs(y ~ -1 + z + varying(~ x + lag(y)) + random(~1), "gaussian") + 
  splines(df = 20)
dynamite_code <- get_code(f, data = gaussian_example, time = "time",  group = "id")

## -----------------------------------------------------------------------------
fit <- dynamite(
  dformula = f, data = gaussian_example, time = "time",  group = "id", 
  custom_stan_model = "custom_code.stan", chains = 1, refresh = 0
)

## -----------------------------------------------------------------------------
as.array(fit$stanfit, pars = "df") |> 
  posterior::as_draws_df() |> 
  posterior::summarise_draws()

## -----------------------------------------------------------------------------
plot(fit, type = "nu", n_params = 20)

## -----------------------------------------------------------------------------
loo(fit)

## -----------------------------------------------------------------------------
newdata <- data.frame(
  time = 1:30, 
  id = 51, 
  y = rep(c(3, NA), times = c(10, 20)), 
  x = 0, 
  z = 1
)

pp <- predict(fit, newdata = newdata, new_levels = "original", n_draws = 50)

ggplot2::ggplot(pp, ggplot2::aes(time, y_new, group = .draw)) +
  ggplot2::geom_line(alpha = 0.1) +
  ggplot2::theme_bw()

## -----------------------------------------------------------------------------
d <- get_data(fit)

## -----------------------------------------------------------------------------
model <- rstan::stan_model("custom_code.stan")
fit_vb <- rstan::vb(model, data = d, iter = 1e5, refresh = 0)

## -----------------------------------------------------------------------------
fit_vb_dynamite <- fit
fit_vb_dynamite$stanfit <- fit_vb

## -----------------------------------------------------------------------------
summary(fit_vb_dynamite, types = "beta")

pp2 <- predict(
  fit_vb_dynamite, newdata = newdata, new_levels = "original", n_draws = 50
)

ggplot2::ggplot(pp2, ggplot2::aes(time, y_new, group = .draw)) +
  ggplot2::geom_line(alpha = 0.1) +
  ggplot2::theme_bw()

