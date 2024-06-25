
library(CircStats)
library(circular)
library(tidyverse)

datos = fisherB6$set2
a = .05

datos.res = Directional::circ.summary(datos)

# circular dispersion

# rho_2 = sum(cos(rad(2*(datos-datos.res$mesos))))/length(datos)
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

theta = fisherB13$set14
theta2 = fisherB13$set5

d.1 = abs(sin(rad(theta-179.5)))
d.2 = abs(sin(rad(theta2-173)))

(d.1.hat = sum(d.1)/length(theta))
(d.2.hat = sum(d.2)/length(theta2))

(d.hat = c(d.1.hat*length(theta) + d.2.hat*length(theta2))/
  c(length(theta)+length(theta2)))

((c(length(theta)+length(theta2))-2)*sum(c(length(theta)*(d.1.hat-d.hat)^2, 
                                           length(theta2)*(d.2.hat-d.hat)^2))) /
  ((2-1)*(sum(c(sum((d.1-d.1.hat)^2), sum((d.2-d.2.hat)^2)))))

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


