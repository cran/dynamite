## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library("dynamite")
suppressPackageStartupMessages(library("ggplot2"))

## ----categinit, eval=FALSE, echo=TRUE-----------------------------------------
#  library(dynamite)
#  set.seed(1)
#  n_id <- 100L
#  n_time <- 20L
#  d <- data.frame(
#    y = sample(factor(c("a", "b", "c")), size = n_id, replace = TRUE),
#    x = sample(factor(c("A", "B", "C")), size = n_id, replace = TRUE),
#    time = 1,
#    id = seq_len(n_id)
#  )

## ----categexpand, eval=FALSE, echo=TRUE---------------------------------------
#  d <- dplyr::right_join(
#    d,
#    data.frame(
#      time = rep(seq_len(n_time), each = n_id),
#      id = seq_len(n_id)
#    ),
#    by = c("time", "id")
#  )
#  d$z <- rnorm(nrow(d))

## ----categform, eval=FALSE, echo=TRUE-----------------------------------------
#  f <- obs(x ~ z + lag(x) + lag(y), family = "categorical") +
#    obs(y ~ z + lag(x) + lag(y), family = "categorical")

## ----categdimsecho, eval=FALSE, echo=TRUE-------------------------------------
#  get_parameter_dims(x = f, data = d, time = "time", group = "id")

## ----categdimseval, eval=TRUE, echo=FALSE-------------------------------------
# This is actually computed
get_parameter_dims(categorical_example_fit)

## ----categparamvals, eval=FALSE, echo=TRUE------------------------------------
#  init <- list(
#    beta_x_B = c(2, 0.8, 0.2, 0, 0),
#    a_x_B = -0.1,
#    beta_x_C = c(1, 0.5, 2, 0.2, 0.1),
#    a_x_C = 0.2,
#    beta_y_b = c(0, 1, 0.8, 0.3, 0.5),
#    a_y_b = 0.1,
#    beta_y_c = c(1, 0.2, 0, 0.3, -0.5),
#    a_y_c = -0.5
#  )

## ----categfit, eval=FALSE, echo=TRUE------------------------------------------
#  fit <- dynamite(
#    dformula = f,
#    data = d,
#    time = "time",
#    group = "id",
#    chains = 1,
#    iter = 1,
#    algorithm = "Fixed_param",
#    init = list(init),
#  )

## ----categpred, eval=FALSE, echo=TRUE-----------------------------------------
#  categorical_example <- predict(fit, type = "response") |>
#    dplyr::mutate(x = x_new, y = y_new) |>
#    dplyr::select(id, time, x, y, z)

## ----gaussiainit--------------------------------------------------------------
library("dynamite")
set.seed(1)
n_id <- 8L
n_time <- 20L
d <- data.frame(
  y = rnorm(n_id, 1, 0.5),
  time = 1,
  id = seq_len(n_id)
)

## ----gaussianexpand-----------------------------------------------------------
d <- dplyr::right_join(
  d,
  data.frame(
    time = rep(seq_len(n_time), each = n_id),
    id = seq_len(n_id)
  ),
  by = c("time", "id")
)
d$x <- rnorm(nrow(d))

## ----gaussianform-------------------------------------------------------------
f <- obs(y ~ 1 + varying(~ -1 + x + lag(y)), family = "gaussian") +
  splines(df = 10)

## ----gaussiandimsecho, eval=FALSE, echo=TRUE----------------------------------
#  get_parameter_dims(x = f, data = d, time = "time", group = "id")

## ----gaussiandimseval, eval=TRUE, echo=FALSE----------------------------------
list(
  omega_y = c(2L, 10L),
  tau_y = 2L,
  a_y = 1L,
  sigma_y = 1L
)

## ----gaussianparamvals--------------------------------------------------------
init <- list(
  omega_y = rbind(
    c(0.0,  0.2, -0.8, 1.0, 0.5, 0.0, 0.1, -0.2, -0.5,  0.1),
    c(0.3, -0.4,  0.7, 0.5, 0.0, 0.0, 0.0,  0.6,  0.9, -0.8)
  ),
  tau_y = c(1.0, 0.75),
  a_y = -1,
  sigma_y = 0.5
)

## ----gaussianfit, eval=FALSE, echo=TRUE---------------------------------------
#  gaussian_simulation_fit <- dynamite(
#    dformula = f,
#    data = d,
#    time = "time",
#    group = "id",
#    chains = 1,
#    iter = 1,
#    algorithm = "Fixed_param",
#    init = list(init),
#  )

## ----gaussianpred-------------------------------------------------------------
sim <- predict(gaussian_simulation_fit, type = "response") |>
  dplyr::mutate(y = y_new) |>
  dplyr::select(id, time, x, y)

## ----gaussianplot, fig.width = 7, fig.height=3.5------------------------------
library("ggplot2")
sim |>
  dplyr::filter(id < 9) |>
  ggplot(aes(time, y, color = factor(id))) +
  geom_line() +
  theme_bw()

