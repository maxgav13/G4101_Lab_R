
library(rstanarm)
library(brms)
library(gtsummary)
library(sjPlot)
library(ggeffects)
library(emmeans)
library(easystats)
library(marginaleffects)
library(tidyverse)

options(mc.cores = parallel::detectCores(),
        ggeffects_margin = "empirical")
set_theme(theme_bw())
  

airq = airquality %>% 
  tibble() %>% 
  mutate(Month = as.factor(Month))

d = mtcars %>% 
  mutate(across(c(cyl,am,gear,carb),factor)) %>% 
  tibble()

# single categorical predictor

m = lm(mpg ~ cyl, d) 

m %>% predictions(by = 'cyl')
m %>% avg_comparisons(variables = list(cyl='pairwise'))
m %>% plot_predictions(by = 'cyl')

emmeans(m, pairwise ~ cyl)$emmeans %>% plot
emmeans(m, pairwise ~ cyl)$contrasts %>% plot

estimate_means(m, by = 'cyl')
estimate_contrasts(m, by = 'cyl', p_adjust = 'tukey')

# single numerical predictor

m = lm(mpg ~ hp, d)

emmeans(m, ~hp)
m %>% avg_predictions(variables = list(hp=mean))

emmeans(m, ~hp, cov.reduce=range)
m %>% avg_predictions(variables = list(hp=range))
estimate_means(m, at = 'hp=[meansd]')

m1 = lm(mpg ~ poly(hp,2), d)

emmeans(m1, ~hp, at=list(hp=c(100,200,300))) %>% 
  plot(comparisons=T)
m1 %>% avg_predictions(variables = list(hp=c(100,200,300)), hypothesis = 'pairwise')
estimate_contrasts(m1, contrast = 'hp=c(100,200,300)')

m1 %>% plot_predictions(condition = 'hp', points = 1)

# one categorical + one numerical predictors

set.seed(10)
salary = ISLR::Wage %>% 
  group_by(jobclass) %>% 
  slice_sample(n = 50)

m = lm(wage ~ jobclass + age, salary)

emmeans(m, ~jobclass)
avg_predictions(m, variables = 'jobclass')
estimate_means(m, at = 'jobclass')

emmeans(m, pairwise ~ age | jobclass,
        at = list(age=c(25,45,65)))$emmeans
avg_predictions(m, variables = list(age=c(25,45,65),jobclass=unique))
estimate_means(m, at = list(age=c(25,45,65)), 
               fixed = 'jobclass', factors = 'all')
estimate_contrasts(m, contrast = list(age=c(25,45,65)), 
                   at = 'jobclass', factors = 'all')

emmeans(m, pairwise ~ age | jobclass,
        at = list(age=c(25,45,65)))$emmeans %>% plot(comparisons=T)
emmeans(m, pairwise ~ age | jobclass,
        at = list(age=c(25,45,65)))$contrasts %>% plot

plot_model(m, type = 'pred', terms = c('jobclass','age[meansd]'))
plot_predictions(m,condition = list('jobclass','age'='threenum'))

plot_model(m, type = 'pred', terms = c('age','jobclass'))
plot_predictions(m,condition = list('age','jobclass'))

# two categorical predictors

m = lm(mpg ~ am + cyl, d)

emmeans(m, pairwise~cyl)
avg_comparisons(m, variables = list(cyl='revpairwise'))
avg_predictions(m, by = 'cyl', newdata = datagrid(grid_type = "balanced"))
estimate_means(m, at = 'cyl')
estimate_contrasts(m, contrast = 'cyl')

# two numerical predictors

m = lm(mpg ~ hp + wt, d)

emmeans(m, ~wt | hp, cov.reduce=range)
avg_predictions(m, variables = list(wt=range,hp=range))
estimate_means(m, at = c("hp=[minmax]","wt=[minmax]"))

plot_model(m, type = 'pred', terms = c('wt','hp[52,335]'))
plot_predictions(m,condition = list('wt','hp'=range))

# multivariate model

m = lm(Temp ~ Ozone + Solar.R + Month, airq)

## model assumptions
check_model(m)

## visualize predictions
ggeffect(m)

ggeffect(m) %>% 
  plot(show.title = FALSE) %>% 
  patchwork::wrap_plots() +
  patchwork::plot_annotation(tag_level = 'A')
  # sjPlot::plot_grid()

get_predictors(m) %>% 
  names() %>% 
  map(~plot_model(m, type = 'pred', terms = .x)) %>% 
  map(~.x + labs(title = NULL)) %>% 
  patchwork::wrap_plots() +
  patchwork::plot_annotation(tag_level = 'A')

## multiple comparison
tbl_regression(
  m,
  add_pairwise_contrasts = T,
  pvalue_fun = ~style_pvalue(.x,digits = 3),
  estimate_fun = ~style_number(.x, digits = 2)
) %>% 
  bold_p() %>% 
  italicize_levels()

estimate_contrasts(m, method = 'revpairwise', p_adjust = 'tukey')

m %>%
  emmeans(~ 'Month') %>%
  contrast(method = 'revpairwise',infer=T)

avg_comparisons(m, variables = list(Month='pairwise'))

avg_predictions(m, variables = 'Month', hypothesis = 'revpairwise')

## variable importance
omega_squared(m) %>% 
  mutate(Interpret = interpret_omega_squared(Omega2_partial))

omega_squared(m) %>% 
  mutate(Interpret = interpret_omega_squared(Omega2_partial)) %>% 
  ggplot(aes(x = reorder(Parameter, Omega2_partial),
             y = Omega2_partial)) + 
  geom_bar(stat = 'identity') +
  geom_text(aes(label=Interpret),
            size = 4, hjust = -.1, fontface = 'bold') + 
  coord_flip() + 
  xlab(NULL) + 
  ylim(0,.7)

## goodness of fit
performance(m)

interpret_r2(.69)

# interactions

set.seed(1)
salary = ISLR::Wage %>% 
  mutate(age_cat = case_when(
    age < 40 ~ '1. young',
    .default = '2. old'
  )) %>% 
  group_by(education) %>% 
  slice_sample(n = 50)

## two categorical predictors

m = lm(wage ~ age_cat * jobclass, salary)
m1 = lm(wage ~ age_cat + jobclass, salary)

plot_model(m, type = 'int')
plot_predictions(m, condition = c('age_cat','jobclass'))

tbl_regression(m1)

emmeans(m, pairwise ~ age_cat)
avg_predictions(m, variables = 'jobclass', by = 'age_cat')
estimate_means(m, at = 'age_cat')

emmeans(m, pairwise ~ age_cat | jobclass, infer=T)$contrasts
avg_comparisons(m, variables = list(age_cat='revpairwise'), by = 'jobclass')
avg_predictions(m, variables = list('age_cat'=unique,'jobclass'=unique))
estimate_contrasts(m, contrast = 'age_cat', 
                   at = 'jobclass', factors = 'all')
estimate_means(m, at = 'age_cat', 
               fixed = 'jobclass', factors='all')


## categorical + numerical predictors

set.seed(1)
salary = ISLR::Wage %>% 
  group_by(education) %>% 
  slice_sample(n = 100)

m = lm(wage ~ health * age, salary)

plot_model(m, type = 'pred', terms = c('age','health'))
plot_model(m, type = 'int')
plot_model(m, type = 'int', mdrt.values = 'quart')
plot_predictions(m, condition = c('age','health'))
plot_predictions(m, condition = c('health','age'))

emtrends(m, pairwise ~ health, var = 'age', infer = T)
avg_slopes(m, variables = 'age', by = 'health')
estimate_slopes(m, trend = 'age', at = 'health')

emmeans(m, pairwise ~ health | age, cov.reduce=range)
avg_comparisons(m, variables = list(health='pairwise'), by = 'age',
                newdata = datagrid(FUN_integer = range))
estimate_contrasts(m, contrast = 'health', 
                   fixed = 'age', numerics='range')

avg_predictions(m, variables = list('age'=range,'health'=unique))
estimate_means(m, at=c("age=[minmax]", "health"), factors='all')
# meansd, sd, mad, quartiles, quartiles2, zeromax, 
# minmax, terciles, terciles2, fivenum

emmeans(m, pairwise ~ health | age, at = list(age=c(25,45,65)))
emmeans(m, pairwise ~ age | health, at = list(age=c(25,45,65)))
avg_comparisons(m, variables = list(age=c(25,65)), by = 'health')
estimate_contrasts(m, contrast=list(age=c(25,45,65)), 
                   fixed = "health", factors='all')
estimate_contrasts(m, contrast='health', 
                   fixed = "age", numerics = c(25,45,65))

predict_response(m, c('health','age[25,45,65]'))
predict_response(m, c('age[25,45,65]','health'))
test_predictions(m, c('health','age[25,45,65]'))

## two numerical predictors

m = lm(mpg ~ hp * qsec, mtcars)

emmeans(m, pairwise ~ qsec | hp, 
        at = list(hp = c(50,150,250),
                  qsec = c(15,18,21)))

avg_predictions(m, variables = list('qsec'=c(15,18,21),'hp'=c(50,150,250)))
avg_comparisons(m, variables = list(qsec=c(15,21)),by = 'hp',
                newdata = datagrid(hp=c(50,150,250)))
estimate_means(m, at=c("hp=c(50,150,250)", "qsec=c(15,18,21)"))
estimate_contrasts(m, contrast=list(qsec=c(15,18,21)), 
                   fixed = 'hp', numerics = c(50,150,250))

plot_model(m, type = 'pred', terms = c('hp','qsec[15,18,21]'))
plot_predictions(m, condition = list('hp','qsec'=c(15,18,21)))

predict_response(m, c('hp','qsec')) %>% johnson_neyman() %>% plot()
plot_model(m, type = 'pred', terms = c('hp','qsec[15,18,21]'),grid = T)

predict_response(m, c('qsec','hp')) %>% johnson_neyman() %>% plot()
plot_model(m, type = 'pred', terms = c('qsec','hp[100,140,250]'),grid = T)


