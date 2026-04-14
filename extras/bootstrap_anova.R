library(emmeans)
library(infer)
library(tidyverse)

# Manualmente crear un objeto emmobj a partir de tus datos de grupo

dat <- list(
  A = rnorm(30, mean = 5, sd = 1),
  B = rnorm(30, mean = 6, sd = 1),
  C = rnorm(30, mean = 7, sd = 1)
)

dat = mtcars |> nest_by(cyl) |> mutate(var = list(pull(data,mpg))) |> pull(var)
lvls = c("4", "6", "8")
names(dat) = lvls

n_boot = 2000 # número de replicados bootstrap

## 1. Create bootstrap replicates of your group means (or medians)
boot_samples <- replicate(n_boot, {
  sapply(dat, function(x) mean(sample(x, replace = TRUE)))
})

## 2. Reformat: ensure rows are replicates and columns are groups
boot_samples <- t(boot_samples)

## 3. Create an emmobj using the means and covariance of your replicates
means_est <- apply(boot_samples, 2, mean)
cov_est <- cov(boot_samples)
EMM <- emmobj(means_est, cov_est, levels = names(dat), post.beta = boot_samples)

## 4. Run pairwise post-hoc tests
summary(EMM)
summary(pairs(EMM))

# Con funciones de paquetes

cars = mtcars |> 
  mutate(
    id = 1:nrow(mtcars),
    cyl = factor(cyl, labels = c("4", "6", "8"))
  )

## AOboot

AOboot::AObootBetween(
  var.between = "cyl",
  var.dv = "mpg",
  var.id = "id",
  levels.b1 = c("4", "6", "8"),
  eff.si = "ges",
  data = cars,
  silence = TRUE,
  n.sim = n_boot,
  alpha = .05,
  seed = 1234,
  n.round = 2)

## permuco

permuco::aovperm(
  formula = mpg ~ cyl,
  data = mtcars,
  nperm = n_boot)

## WRS2

WRS2::t1way(
  formula = mpg ~ cyl, 
  data = mtcars, 
  alpha = .05,
  nboot = n_boot)

WRS2::lincon(
  formula = mpg ~ cyl, 
  data = mtcars, 
  alpha = .05)

## infer

F_hat <- cars |> 
  specify(mpg ~ cyl) |>
  calculate(stat = "F")

null_dist <- cars |>
   specify(mpg ~ cyl) |>
   hypothesize(null = "independence") |>
   generate(reps = n_boot, type = "permute") |>
   calculate(stat = "F")

visualize(null_dist) +
  shade_p_value(obs_stat = F_hat, direction = "greater")

null_dist |>
  get_p_value(obs_stat = F_hat, direction = "greater")
