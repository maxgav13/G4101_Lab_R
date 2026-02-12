
# remotes::install_github("onofriAndreaPG/aomisc")

library(sjPlot)
library(ggeffects)
# library(aomisc)
library(nlraa)
library(nlme)
library(lme4)
library(lmerTest)
library(broom.mixed)
library(easystats)
library(tidyverse)

options(mc.cores = parallel::detectCores(),
        ggeffects_margin = "empirical")
set_theme(theme_bw())

# linear

## nlme

cylGrp = groupedData(mpg ~ wt | cyl, data = d)

fit0.nlme = lme(mpg ~ wt, 
                data = cylGrp, 
                random = ~ 1, 
                control = lmeControl(opt='optim'),
                method = 'ML')

fit1.nlme = lme(mpg ~ wt, 
                data = cylGrp, 
                random = ~ wt, 
                control = lmeControl(opt='optim'),
                method = 'ML')

plot(augPred(fit0.nlme, level = 0:1, length.out = 100))
plot(augPred(fit1.nlme, level = 0:1, length.out = 100))

anova(fit0.nlme,fit1.nlme)

## lme4

fit0.lme4 = lmer(mpg ~ wt + (1|cyl), 
                data = d, 
                control = lmerControl(optimizer='bobyqa'),
                REML = F)

fit1.lme4 = lmer(mpg ~ wt + (wt|cyl), 
                 data = d, 
                 control = lmerControl(optimizer='bobyqa'),
                 REML = F)

plot_model(fit0.lme4,type = 'pred',terms = c('wt','cyl'),
           pred.type = 're',show.data = T,grid = T)
plot_model(fit1.lme4,type = 'pred',terms = c('wt','cyl'),
           pred.type = 're',show.data = T,grid = T)

anova(fit0.lme4,fit1.lme4)

# non-linear

data("lfmc")

sb = lfmc %>% filter(leaf.type == 'S. bracteolactus')

ggplot(sb, aes(time,lfmc)) + 
  geom_point() +
  geom_smooth(se = F) +
  facet_wrap(~plot)

## nlme

### grouped data
sbGrp = groupedData(lfmc ~ time | plot, data = sb)
plot(sbGrp)


### individual fits
sb.plot = nlsList(lfmc ~ SSdlf(time, upper, lower, mid, scale), data = sbGrp)
sb.plot
summary(sb.plot)

### fit nlmm
sb.nlme = nlme(sb.plot)
sb.nlme

sb.nlme2 = nlme(sb.plot, random = pdDiag(upper + lower + mid + scale ~ 1))
sb.nlme2

fixef(sb.nlme2)
ranef(sb.nlme2)
coef(sb.nlme2)

plot(augPred(sb.nlme2, level = 0:1, length.out = 100))

sb.nlme3 = update(sb.nlme2, random = pdDiag(upper ~ 1))
sb.nlme3

broom.mixed::augment(sb.nlme3,data = sb) %>% 
  ggplot(aes(time,lfmc)) + 
  geom_point() +
  geom_line(aes(y=.fitted),col='orange') +
  geom_line(aes(y=.fixed),col='blue') +
  facet_wrap(~plot)

pred.grid = expand_grid(time=1:80,plot=factor(1:3))

pred.dat = predict(sb.nlme3,
                   pred.grid,
                   level = 0:1) %>% 
  as_tibble() %>% 
  bind_cols(pred.grid %>% select(-plot))

ggplot() + 
  geom_point(aes(time,lfmc),sb) +
  geom_line(aes(time,predict.plot),pred.dat,col='orange') +
  geom_line(aes(time,predict.fixed),pred.dat,col='blue') +
  facet_wrap(~plot)

## lme4

sb.nlmer = nlmer(lfmc ~ SSdlf(time, upper, lower, mid, scale) ~ upper | plot,
                 start = c(upper=260, lower=50, mid=30, scale=-16),
                 data = sb, 
                 control = nlmerControl(optCtrl = list(maxfun=2e4)))
sb.nlmer

broom.mixed::augment(sb.nlme3,data = sb) %>% 
  ggplot(aes(time,lfmc)) + 
  geom_point() +
  geom_line(aes(y=.fitted),col='orange') +
  facet_wrap(~plot)
