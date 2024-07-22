
library(CircStats)
library(circular)
library(tidyverse)

datos = fisherB6$set2
a = .05

datos.res = Directional::circ.summary(datos)

# circular dispersion & confidence cone

tm1 = trig.moment(rad(datos),p = 1)
tm2 = trig.moment(rad(datos),p = 2)
(delta_c = (1-tm2[[2]])/(2*tm1[[2]]^2))
(sigma_c = sqrt(delta_c/length(datos)))
(cone_c = deg(asin(sigma_c*qnorm(1-a/2))))

if (Rbar < 2/3) {
  acos(sqrt(2 * N * (2 * R^2 - N * 
                          qchisq(1-a, 
                                 1))/(R^2 * (4 * N - qchisq(1-a, 1)))))*180/pi
} else {
  acos(sqrt(N^2 - (N^2 - R^2) * 
                 exp(qchisq(1-a,1)/N))/R)*180/pi
}

# test for equal direction

theta = fisherB13$set10
theta2 = fisherB13$set1

c.1 = cos(rad(172.5))/(1/sqrt(50*.95*9.8))^2
c.2 = cos(rad(180.6))/(1/sqrt(100*.72*2.1))^2

s.1 = sin(rad(172.5))/(1/sqrt(50*.95*9.8))^2
s.2 = sin(rad(180.6))/(1/sqrt(100*.72*2.1))^2

(RM = sqrt((c.1+c.2)^2 + (s.1+s.2)^2))

(Y.r = 2*((50*.95*9.8 + 100*.72*2.1) - RM))

# test for equal kappa

theta = fisherB13$set14
theta2 = fisherB13$set5

d.1 = abs(sin(rad(theta-179.5)))
d.2 = abs(sin(rad(theta2-173)))

(d.1.hat = sum(d.1)/length(theta))
(d.2.hat = sum(d.2)/length(theta2))

(d.hat = c(d.1.hat*length(theta) + d.2.hat*length(theta2))/
  c(length(theta)+length(theta2)))

(f.r = (c(length(theta)+length(theta2))-2)*
    sum(c(length(theta)*(d.1.hat-d.hat)^2, 
          length(theta2)*(d.2.hat-d.hat)^2))) /
  ((2-1)*(sum(c(sum((d.1-d.1.hat)^2), sum((d.2-d.2.hat)^2)))))

#

dir_stats_circ = function (x, data = NULL, dir = 1, conf.level = 0.95) {
  
  if (is.null(data)) {
    theta = x
  } else {
    theta = data %>% dplyr::pull({{x}})
  }
  
  N = length(theta)
  
  circ_disp = function(theta) {
    thetarad = theta*pi/180
    tm1 = CircStats::trig.moment(thetarad,p = 1)
    tm2 = CircStats::trig.moment(thetarad,p = 2)
    delta = (1-tm2[[2]])/(2*tm1[[2]]^2)
    delta
  }
  
  if (dir == 0) {
    theta = theta * 2
  }
  thetarad = theta * pi/180
  x = sum(sin(thetarad))
  y = sum(cos(thetarad))
  meanrad = atan(x/y)
  meandeg = meanrad * 180/pi
  meantrue = dplyr::case_when(
    x > 0 & y > 0 ~ meandeg,
    x < 0 & y > 0 ~ meandeg + 360,
    .default = meandeg + 180
  )
  meantrue = ifelse(dir == 0, meantrue/2, meantrue)
  
  R = sqrt(x^2+y^2)
  Rbar = R/N
  V = 1 - Rbar
  v = sqrt(-2*log(Rbar))
  delta = circ_disp(theta)
  k = dplyr::case_when(Rbar < .53 ~ 2*Rbar + Rbar^3 + 5/6*Rbar^5,
                       Rbar < .85 ~ -.4 + 1.39*Rbar + .43/(1-Rbar),
                       .default = (Rbar^3 - 4*Rbar^2 + 3*Rbar)^(-1))
  
  a = 1 - conf.level
  se = (1/sqrt(N * Rbar * k))
  cono = suppressWarnings(asin(se*stats::qnorm(1-a/2))*180/pi)
  if (is.na(cono)) {
    se = (1/sqrt(N * Rbar * k))*180/pi
    cono = (se*stats::qnorm(1-a/2))
  }
  
  sigma_c = sqrt(delta/N)
  cone_c = asin(sigma_c*stats::qnorm(1-a/2))*180/pi
  
  cono_sup = meantrue + cono
  cono_inf = meantrue - cono
  cono_sup = ifelse(cono_sup > 360, cono_sup - 360, cono_sup)
  cono_inf = ifelse(cono_inf < 0, 360 + cono_inf, cono_inf)
  
  dir.stats = data.frame(theta.hat = round(meantrue, 1), 
                         N = N,
                         R = signif(R, 4), 
                         Rbar = signif(Rbar, 4), 
                         circ.var = signif(V, 4), 
                         circ.sd = signif(v, 4),
                         circ.disp = signif(delta, 4),
                         kappa = signif(k, 4), 
                         # lower = round(cono_inf, 1), 
                         # upper = round(cono_sup, 1), 
                         cone = round(cono, 2)
                         )
  return(dir.stats)
}

rose_diag_circ = function (x, data = NULL, 
                           width = 20, dir = 1, 
                           conf.level = 0.95,
                           alpha = .7,
                           fill.col = 'blue',
                           mean.col = 'red') {
  
  if (is.null(data)) {
    x = x
  } else {
    x = data %>% dplyr::pull({{x}})
  }
  
  x = ifelse(x > 360, x - 360, x)
  
  if (dir == 0) {
    y = x + 180
    # y = ifelse(y > 360, y - 360, y)
    z = c(x, y)
  } else {
    z = x
  }
  
  z = ifelse(z > 360, z - 360, z)
  
  if (dir == 0) {
    max.dir = DescTools::Freq(z,seq(0,180,width)) %>% 
      tibble::tibble() %>% 
      dplyr::slice_max(perc) %>% 
      dplyr::slice_head(n = 1) %>% 
      dplyr::pull(perc, freq) %>%
      round(3)*100
  } else {
    max.dir = DescTools::Freq(x,seq(0,360,width)) %>% 
      tibble::tibble() %>% 
      dplyr::slice_max(perc) %>% 
      dplyr::slice_head(n = 1) %>% 
      dplyr::pull(perc, freq) %>%
      round(3)*100
  }
  
  N = length(x)
  r = suppressWarnings(dir_stats_circ(x, dir = dir, conf.level = conf.level))
  
  labelmeandir = bquote("Mean Direction =" ~ .(format(round(r$theta.hat, 1))) * 
                          # degree * "" %+-% "" * .(format(round(r$cone, 1))) * 
                          degree ~ ", N =" ~ .(r$N))
  
  labelmeandir2 = paste0("Mean Direction =", format(r$theta.hat,1), 
                         " (", format(round(r$cone, 1)), ")", 
                         ", N =", r$N)
  
  theta.180 = ifelse(r$theta.hat > 180, r$theta.hat - 180, r$theta.hat + 180)
  
  theme_rose = theme_bw() + 
    theme(title = element_text(size = 14), 
          axis.title.x = element_blank(), 
          panel.border = element_blank(), 
          axis.ticks.y = element_blank(), 
          axis.text.x = element_text(size = 12, face = "plain", 
                                     hjust = 0.9, vjust = 1.3), 
          panel.grid.major = element_line(linewidth = 0.3, 
                                          colour = "grey60"), 
          panel.grid.minor = element_line(linewidth = 0.1, 
                                          colour = "grey60"))
  
  if (dir == 1) {
    plt.dirrose <- ggplot(data.frame(z), aes(z)) + 
      stat_bin(aes(y = sqrt(after_stat(.data$count)/max(after_stat(.data$count)) * 
                              100^2)), 
               breaks = seq(0,360,width), 
               colour = "black",  alpha = alpha,
               fill = fill.col, closed = "left") + 
      scale_x_continuous(breaks = seq(0,360,30), 
                         minor_breaks = seq(0,360,10),
                         limits = c(0, 360)) + 
      geom_vline(xintercept = r$theta.hat, 
                 col = mean.col, 
                 linewidth = .5) + 
      scale_y_continuous(NULL, 
                         breaks = seq(0,100,20), 
                         labels = NULL) + 
      labs(title = labelmeandir, 
           subtitle = paste0('Max. = ',max.dir,'%')) + 
      coord_radial(expand = F) +
      theme_rose
    
  } else {
    plt.dirrose <- ggplot(data.frame(z), aes(z)) + 
      stat_bin(aes(y = sqrt(after_stat(.data$count)/max(after_stat(.data$count)) * 
                              100^2)), 
               breaks = seq(0,360,width), 
               colour = "black", alpha = alpha,
               fill = fill.col, closed = "left") + 
      scale_x_continuous(breaks = seq(0,360,30), 
                         minor_breaks = seq(0,360,10),
                         limits = c(0, 360)) + 
      geom_vline(xintercept = c(r$theta.hat, theta.180), 
                 col = mean.col, 
                 linewidth = .5) + 
      scale_y_continuous(NULL, 
                         breaks = seq(0,100,20), 
                         labels = NULL) + 
      labs(title = labelmeandir, 
           subtitle = paste0('Max. = ',max.dir,'%')) + 
      coord_radial(expand = F) +
      theme_rose
  }
  return(plt.dirrose)
}

# pasa a datos circulares
windc = circular(wind, type='angles',
                 units='radians')
datosc = circular(datos, type='angles',
                  units='degrees')

# grafico de puntos
plot(windc, cex=1.5, bin=720, stack=TRUE, sep=0.035, shrink=1.3)
axis.circular(at=circular(seq(0,7*pi/4,pi/4)), 
              labels = as.character(seq(0,315,45)),
              zero=pi/2, rotation='clock',
              cex=1.1)
ticks.circular(circular(seq(0,2*pi,pi/8)), 
               zero=pi/2, rotation='clock', tcl=0.075)

# agrega diagrama de rosas
rose.diag(windc, bins=16, col='dodgerblue', cex=1.5, prop=1.3)
# agrega vector medio
arrows.circular(mean(windc), y=rho.circular(windc), lwd=3, col='red')

# estadistica descriptiva
mean(windc)
median.circular(windc)
rho.circular(windc)
sd.circular(windc)
range.circular(windc)

# inferencias sobre una muestra

## prueba de uniformidad
rayleigh.test(windc)
kuiper.test(windc)
watson.test(windc)
rao.spacing.test(windc)

## uniformidad con respecto a una direccion espcifica
mu = circular(315, units='degress', template='geographics')
rayleigh.test(windc, mu)


## simetria reflectiva

### para muestras grandes > 50
RSTestStat = function(circdat) {
  n = length(circdat) ; Rbar = rho.circular(circdat)
  t2bar = trigonometric.moment(circdat, p=2, center=TRUE)
  t3bar = trigonometric.moment(circdat, p=3, center=TRUE)
  t4bar = trigonometric.moment(circdat, p=4, center=TRUE)
  bbar2 = t2bar$sin ; abar2 = t2bar$cos
  abar3 = t3bar$cos ; abar4 = t4bar$cos
  var = ((1-abar4)/2-(2*abar2)+(2*abar2/Rbar)*(abar3+(abar2*(1-abar2)/Rbar)))/n
  absz = abs(bbar2/sqrt(var)) ; return(absz)
}

cdat = circular(wind) ; absz = RSTestStat(cdat)
pval = 2*pnorm(absz, mean=0, sd=1, lower=FALSE) ; pval

### bootstrap
RSTestBoot = function(origdat, B) {
  n = length(origdat) ; absz = RSTestStat(origdat)
  tbar = mean(origdat) ; refcdat = 2*tbar-origdat
  symmcdat = c(origdat, refcdat) ; nxtrm = 1
  for (b in 2:(B+1)) {
    bootsymmdat = sample(symmcdat, size=n, replace=TRUE)
    absz[b] = RSTestStat(bootsymmdat)
    if (absz[b] >= absz[1]) {nxtrm = nxtrm+1}
  }
  pval = nxtrm/(B+1) ; return(pval)
}

cdat = circular(wind) ; B = 9999
pval = RSTestBoot(cdat, B) ; pval

## estimadores puntuales y de intervalo insesgados

### muestras grandes
ConfIntLS = function(circdat, indsym, conflevel) {
  n = length(circdat) ; tbar = mean(circdat) ; Rbar = rho.circular(circdat)
  t2bar = trigonometric.moment(circdat, p=2, center=TRUE)
  t3bar = trigonometric.moment(circdat, p=3, center=TRUE)
  t4bar = trigonometric.moment(circdat, p=4, center=TRUE)
  abar2 = t2bar$cos ; abar3 = t3bar$cos ; abar4 = t4bar$cos
  bbar2 = t2bar$sin ; bbar3 = t3bar$sin
  Rbar2 = Rbar*Rbar ; Rbar4 = Rbar2*Rbar2
  alpha = (100-conflevel)/100 ; qval = qnorm(1-alpha/2)
  rhobc = Rbar - ((1-abar2)/(4*n*Rbar))
  rbarstderr = sqrt((1-2*Rbar2+abar2)/(2*n))
  rhoup = rhobc+qval*rbarstderr ; rholo = rhobc-qval*rbarstderr
  rhores = c(rhobc, rholo, rhoup)
  if (indsym == 1) { bbar2 = 0 ; bbar3 = 0 } else
    if (indsym == 0) {
      betab2bc = bbar2 + ((bbar3/Rbar)+(bbar2/Rbar2)-(2*abar2*bbar2/Rbar4))/n
      b2bstderr = sqrt((((1-abar4)/2)-(2*abar2)-(bbar2*bbar2)+(2*abar2/Rbar)*
                           (abar3+(abar2*(1-abar2)/Rbar)))/n)
      betab2up = betab2bc+qval*b2bstderr
      betab2lo = betab2bc-qval*b2bstderr
      betab2res = c(betab2bc, betab2lo, betab2up)
    }
  div = 2*n*Rbar2
  mubc = tbar+(bbar2/div) ; tbarstderr = sqrt((1-abar2)/div)
  muup = mubc+qval*tbarstderr ; mulo = mubc-qval*tbarstderr
  mures = c(mubc, mulo, muup)
  alphab2bc = abar2-(1-(abar3/Rbar)-((abar2*(1-abar2)+bbar2*bbar2)/Rbar2))/n
  a2bstderr = sqrt((((1+abar4)/2)-(abar2*abar2)+(2*bbar2/Rbar)*(bbar3+(bbar2*(1-abar2)/Rbar)))/n)
  alphab2up = alphab2bc+qval*a2bstderr
  alphab2lo = alphab2bc-qval*a2bstderr
  alphab2res = c(alphab2bc, alphab2lo, alphab2up)
  if (indsym == 0) { return(list(mures, rhores, betab2res, alphab2res)) } else
    if (indsym == 1) { return(list(mures, rhores, alphab2res)) }
}

cdat = circular(wind) ; sym = 0 ; clev = 95
LSCIOut = ConfIntLS(cdat, sym, clev) ; LSCIOut

### bootstrap
BiasCEsts = function(circdat, indsym, n) {
  t10bar = trigonometric.moment(circdat, p=1, center=FALSE)
  tbar = atan2(t10bar$sin, t10bar$cos) ; if (tbar < 0) {tbar = tbar+2*pi}
  Rbar = rho.circular(circdat)
  t2bar = trigonometric.moment(circdat, p=2, center=TRUE)
  t3bar = trigonometric.moment(circdat, p=3, center=TRUE)
  abar2 = t2bar$cos ; abar3 = t3bar$cos
  bbar2 = t2bar$sin ; bbar3 = t3bar$sin
  Rbar2 = Rbar*Rbar ; Rbar4 = Rbar2*Rbar2
  rhobc = Rbar - ((1-abar2)/(4*n*Rbar))
  if (indsym == 1) {bbar2 = 0 ; bbar3 = 0 ; betab2bc = 0} else
    if (indsym == 0) {
      betab2bc = bbar2 + ((bbar3/Rbar)+(bbar2/Rbar2)-(2*abar2*bbar2/Rbar4))/n
    }
  div = 2*n*Rbar2 ; mubc = tbar+(bbar2/div)
  if (mubc > 2*pi) {mubc = mubc-2*pi} else
    if (mubc < 0) {mubc = mubc+2*pi}
  alphab2bc = abar2- (1-(abar3/Rbar)-((abar2*(1-abar2)+bbar2*bbar2)/Rbar2))/n
  return(list(mubc, rhobc, betab2bc, alphab2bc))
}

ConfIntBoot = function(origdat, indsym, conflevel, B) {
  alpha = (100-conflevel)/100 ; n = length(origdat)
  ests = BiasCEsts(origdat, indsym, n)
  muest = ests[[1]] ; rhoest = ests[[2]]
  betab2est = ests[[3]] ; alphab2est = ests[[4]]
  if (indsym == 1) {
    refdat = 2*muest-origdat ; sampledat = c(origdat, refdat) } else
      if (indsym == 0) { sampledat = origdat }
  for (b in 2:(B+1)) {
    bootdat = sample(sampledat, size=n, replace=TRUE)
    ests = BiasCEsts(bootdat, indsym, n)
    muest[b] = ests[[1]] ; rhoest[b] = ests[[2]]
    betab2est[b] = ests[[3]] ; alphab2est[b] = ests[[4]]
  }
  dist = 0
  if (indsym == 1) {
    dist = pi-abs(pi-abs(muest-muest[1])) ; sdist = sort(dist)
    mulo = muest[1]-sdist[(B+1)*(1-alpha)]
    muup = muest[1]+sdist[(B+1)*(1-alpha)]
  } else
    if (indsym == 0) {
      if (muest[1] < pi) {
        ref = muest[1]+pi
        for (b in 1:(B+1)) {
          dist[b] = -(pi-abs(pi-abs(muest[b]-muest[1])))
          if (muest[b] > muest[1]) {
            if (muest[b] < ref) { dist[b] = -dist[b] }
          }}
      } else
        if (muest[1] >= pi) {
          ref = muest[1]-pi
          for (b in 1:(B+1)) {
            dist[b] = pi-abs(pi-abs(muest[b]-muest[1]))
            if (muest[b] > ref) {
              if (muest[b] < muest[1]) { dist[b] = -dist[j] }
            }}}
      sdist = sort(dist) ; mulo = muest[1]+sdist[(B+1)*(alpha/2)]
      muup = muest[1]+sdist[(B+1)*(1-alpha/2)]
      sbetab2est = sort(betab2est)
      betab2lo = sbetab2est[(B+1)*(alpha/2)]
      betab2up = sbetab2est[(B+1)*(1-alpha/2)]
      betab2res = c(betab2est[1], betab2lo, betab2up)
    }
  mures = c(muest[1], mulo, muup) ; srhoest = sort(rhoest)
  rholo = srhoest[(B+1)*(alpha/2)] ; rhoup = srhoest[(B+1)*(1-alpha/2)]
  salphab2est = sort(alphab2est)
  alphab2lo = salphab2est[(B+1)*(alpha/2)]
  alphab2up = salphab2est[(B+1)*(1-alpha/2)]
  rhores = c(rhoest[1], rholo, rhoup)
  alphab2res = c(alphab2est[1], alphab2lo, alphab2up)
  if (indsym == 0) { return(list(mures, rhores, betab2res, alphab2res)) } else
    if (indsym == 1) { return(list(mures, rhores, alphab2res)) }
}

cdat = circular(wind)
sym = 1 ; clev = 95 ; B = 9999
BCIOut = ConfIntBoot(cdat, sym, clev, B) ; BCIOut

## prueba con respecto a una direccion especifica

### muestras grandes
SpecMeanTestRes = function(circdat, indsym, mu0) {
  n = length(circdat)
  t10bar = trigonometric.moment(circdat, p=1, center=FALSE)
  tbar = atan2(t10bar$sin, t10bar$cos)
  if (tbar < 0) {tbar = tbar+2*pi}
  Rbar = rho.circular(circdat) ; Rbar2 = Rbar*Rbar
  t2bar = trigonometric.moment(circdat, p=2, center=TRUE)
  abar2 = t2bar$cos ; bbar2 = t2bar$sin
  if (indsym == 1) {bbar2 = 0}
  div = 2*n*Rbar2
  mubc = tbar+(bbar2/div) ; tbarstderr = sqrt((1-abar2)/div)
  if (mubc > 2*pi) {mubc = mubc-2*pi} else
    if (mubc < 0) {mubc = mubc+2*pi}
  dist = pi-abs(pi-abs(mubc-mu0)) ; z = dist/tbarstderr
  return(list(z, mubc))
}

cdat = circular(wind) ; sym = 0 ; mu0 = 0
testres = SpecMeanTestRes(cdat, sym, mu0) ; z = testres[[1]]
pval = 2*pnorm(z, mean=0, sd=1, lower=FALSE) ; pval

### bootstrap
SpecMeanTestBoot = function(origdat, mu0, indsym, B) {
  n = length(origdat)
  testres = SpecMeanTestRes(origdat, indsym, mu0)
  z = testres[[1]] ; mubc = testres[[2]]
  shiftdat = origdat-mubc+mu0
  if (indsym == 1) {
    refdat = 2*mu0-shiftdat ; sampledat = c(shiftdat, refdat)
  } else
    if (indsym == 0) { sampledat = shiftdat }
  nxtrm = 1
  for (b in 2:(B+1)) {
    bootdat = sample(sampledat, size=n, replace=TRUE)
    testres = SpecMeanTestRes(bootdat, indsym, mu0)
    z[b] = testres[[1]]
    if (z[b] >= z[1]) { nxtrm = nxtrm+1 }
  }
  pval = nxtrm/(B+1) ; return(pval)
}

cdat = circular(wind) ; sym = 0 ; mu0 = 0 ; B = 9999
pval = SpecMeanTestBoot(cdat, mu0, sym, B) ; pval


