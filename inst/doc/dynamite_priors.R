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
f <- obs(y ~ -1 + z + varying(~ x + lag(y)) + random(~1 + z), "gaussian") +
  random_spec(correlated = TRUE) + splines(df = 20)
p <- get_priors(f, data = gaussian_example, time = "time", group = "id")
p

## -----------------------------------------------------------------------------
p$prior[p$type == "sigma_nu"] <- "normal(0, 1)" # change prior for sigma_nu
p$prior[p$parameter == "sigma_y"] <- "student_t(3, 0, 2)" # prior for sigma_y
p

## ----eval = FALSE-------------------------------------------------------------
#  fit <- dynamite(f, data = gaussian_example, time = "time",  group = "id", priors = p)

