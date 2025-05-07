ZeroTwoPi <- function(a) {
  b <- a
  twopi <- 2.0 * pi
  if (b < 0.0) {
    b <- b + twopi
  } else if (b >= twopi) {
    b <- b - twopi
  }
  return(b)
}

SphToCart <- function(trd, plg, k = 0) {
  
  # k is an integer to tell whether the trend and plunge of a line 
  # (k = 0) or strike and dip of a plane in right hand rule 
  # (k = 1) are being sent in the trd and plg slots
  
  if (k == 0) {
    cd <- sin(plg)
    ce <- cos(plg) * sin(trd)
    cn <- cos(plg) * cos(trd)
  } else if (k == 1) {
    cd <- cos(plg)
    ce <- -sin(plg) * cos(trd)
    cn <- sin(plg) * sin(trd)
  }
  return(c(cn=cn, ce=ce, cd=cd))
}

SphToCartD <- function(trd, plg, k = 0) {
  
  trd0 = trd
  plg0 = plg
  trd = (trd0)*pi/180
  plg = (plg0)*pi/180
  
  if (k == 0) {
    cd <- sin(plg)
    ce <- cos(plg) * sin(trd)
    cn <- cos(plg) * cos(trd)
  } else if (k == 1) {
    cd <- cos(plg)
    ce <- -sin(plg) * cos(trd)
    cn <- sin(plg) * sin(trd)
  }
  return(list(cn=cn, ce=ce, cd=cd))
}

CartToSph <- function(cn, ce, cd) {
  # Plunge (see Table 2.1)
  plg <- asin(cd)
  
  # Trend
  # If north direction cosine is zero, trend is east or west
  # Choose which one by the sign of the east direction cosine
  if (cn == 0.0) {
    if (ce < 0.0) {
      trd <- 3.0/2.0*pi  # trend is west
    } else {
      trd <- pi/2.0  # trend is east
    }
  } else {
    trd <- atan(ce/cn)
    if (cn < 0.0) {
      # Add pi
      trd <- trd + pi
    }
    # Make sure trd is between 0 and 2*pi
    trd <- ZeroTwoPi(trd)
  }
  
  return(c(trd = trd, plg = plg))
}

CartToSphD <- function(cn, ce, cd) {
  # Plunge (see Table 2.1)
  plg <- asin(cd)
  
  # Trend
  # If north direction cosine is zero, trend is east or west
  # Choose which one by the sign of the east direction cosine
  if (cn == 0.0) {
    if (ce < 0.0) {
      trd <- 3.0/2.0*pi  # trend is west
    } else {
      trd <- pi/2.0  # trend is east
    }
  } else {
    trd <- atan(ce/cn)
    if (cn < 0.0) {
      # Add pi
      trd <- trd + pi
    }
    # Make sure trd is between 0 and 2*pi
    trd <- ZeroTwoPi(trd)
  }
  
  return(c(trd = (trd)*180/pi, plg = (plg)*180/pi))
}

GreatCircle = function (strike, dip, wulff = F) {
  
  tpa <- Pole(strike, dip, k = 1)
  trd <- strike
  plg <- 0
  rot <- 0:180*pi/180
  rot = ifelse(rot == pi, rot*.9999,rot)
  out <- matrix(0, nrow = 181, ncol = 2)
  for (i in 1:181) {
    rtp <- Rotate(tpa[1], tpa[2], rot[i], trd, plg, "a")
    out[i, ] <- StCoordLine(rtp[1], rtp[2], wulff = wulff)
  }
  out
}

GreatCircleD = function (strike, dip, wulff = F) {

  strike = (strike)*pi/180
  dip = (dip)*pi/180
  
  tpa <- Pole(strike, dip, k = 1)
  trd <- strike
  plg <- 0
  rot <- 0:180*pi/180
  rot = ifelse(rot == pi, rot*.9999,rot)
  out <- matrix(0, nrow = 181, ncol = 2)
  for (i in 1:181) {
    rtp <- Rotate(tpa[1], tpa[2], rot[i], trd, plg, "a")
    out[i, ] <- StCoordLine(rtp[1], rtp[2], wulff = wulff)
  }
  tibble::as_tibble(out) %>% purrr::set_names(c('xp','yp'))
}

SmallCircle = function (trda, plga, coneAngle, wulff = F, closed = F) {
  
  if (plga < coneAngle) {
    plga = ifelse(plga == pi/2.0, plga * 0.9999, plga)
    angle <- acos(cos(coneAngle)/cos(plga))
    trd <- ZeroTwoPi(trda + angle)
    plg <- 0
  } else {
    trd <- trda
    plg <- plga - coneAngle
  }
  rot <- (0:360) * pi/180
  path1 <- NULL
  path2 <- NULL
  np1 <- 0
  np2 <- 0
  for (i in 1:360) {
    rtp = Rotate(trda, plga, rot[i], trd, plg, "v")
    if (rtp[2] >= 0) {
      path1 <- rbind(path1, StCoordLine(rtp[1], rtp[2], 
                                        wulff = wulff))
    } else {
      path2 <- rbind(path2, StCoordLine(rtp[1], rtp[2], 
                                        wulff = wulff))
    }
  }
  
  if (closed) {
    if (plga < coneAngle) {
      path2 = rbind(path2, -path1[1,])
    } else {
      path1 = rbind(path1, path1[1,])
    }
  }
  
  list(path1 = path1, path2 = path2)
  
}

SmallCircleD = function (trda, plga, coneAngle, wulff = F, closed = F) {
  trda = (trda)*pi/180
  plga = (plga)*pi/180
  coneAngle = (coneAngle)*pi/180
  
  if (plga < coneAngle) {
    plga = ifelse(plga == pi/2.0, plga * 0.9999, plga)
    angle <- acos(cos(coneAngle)/cos(plga))
    trd <- ZeroTwoPi(trda + angle)
    plg <- 0
  } else {
    trd <- trda
    plg <- plga - coneAngle
  }
  rot <- (0:360) * pi/180
  path1 <- NULL
  path2 <- NULL
  np1 <- 0
  np2 <- 0
  for (i in 1:360) {
    rtp = Rotate(trda, plga, rot[i], trd, plg, "v")
    if (rtp[2] >= 0) {
      path1 <- rbind(path1, StCoordLine(rtp[1], rtp[2], 
                                        wulff = wulff))
    } else {
      path2 <- rbind(path2, StCoordLine(rtp[1], rtp[2], 
                                        wulff = wulff))
    }
  }
  
  path1 = tibble::as_tibble(path1)
  path2 = tibble::as_tibble(path2)
  
  if (closed) {
    if (plga < coneAngle) {
      path2 = dplyr::bind_rows(path2, -path1[1,])
    } else {
      path1 = dplyr::bind_rows(path1, path1[1,])
    }
  }
  
  list(path1 = path1, path2 = path2)
  
}

Rotate = function (raz, rdip, rot, trd, plg, ans0) {
  # Rotate rotates a line by performing a coordinate transformation on
  # vectors. The algorithm was originally written by Randall A. Marrett
  # 
  #    USE: [rtrd,rplg] = Rotate(raz,rdip,rot,trd,plg,ans0)
  # 
  #    raz = trend of rotation axis
  #    rdip = plunge of rotation axis
  #    rot = magnitude of rotation
  #    trd = trend of the vector to be rotated
  #    plg = plunge of the vector to be rotated
  #    ans0 = A character indicating whether the line to be rotated is an axis
  #    (ans0 = 'a') or a vector (ans0 = 'v')
  # 
  #    NOTE: All angles are in radians
  
  a <- matrix(0, 3, 3)
  pole <- rep(0, 3)
  plotr <- rep(0, 3)
  temp <- rep(0, 3)
  p <- SphToCart(raz, rdip, 0)
  x <- 1 - cos(rot)
  sinRot <- sin(rot)
  cosRot <- cos(rot)
  a[1, 1] <- cosRot + p[1] * p[1] * x
  a[1, 2] <- -p[3] * sinRot + p[1] * p[2] * x
  a[1, 3] <- p[2] * sinRot + p[1] * p[3] * x
  a[2, 1] <- p[3] * sinRot + p[2] * p[1] * x
  a[2, 2] <- cosRot + p[2] * p[2] * x
  a[2, 3] <- -p[1] * sinRot + p[2] * p[3] * x
  a[3, 1] <- -p[2] * sinRot + p[3] * p[1] * x
  a[3, 2] <- p[1] * sinRot + p[3] * p[2] * x
  a[3, 3] <- cosRot + p[3] * p[3] * x
  temp <- SphToCart(trd, plg, 0)
  for (i in 1:3) {
    plotr[i] = 0
    for (j in 1:3) {
      plotr[i] = a[i, j] * temp[j] + plotr[i]
    }
  }
  if (plotr[3] < 0 && ans0 == "a") {
    plotr[1] <- -plotr[1]
    plotr[2] <- -plotr[2]
    plotr[3] <- -plotr[3]
  }
  CartToSph(plotr[1], plotr[2], plotr[3])
}

StCoordLine = function (trd, plg, wulff = F) {
  
  if (plg < 0) {
    trd <- ZeroTwoPi(trd + pi)
    plg <- -plg
  }
  piS4 <- pi/4
  s2 <- sqrt(2)
  plgS2 <- plg/2
  if (wulff) {
    xp <- tan(piS4 - plgS2) * sin(trd)
    yp <- tan(piS4 - plgS2) * cos(trd)
  } else {
    xp <- s2 * sin(piS4 - plgS2) * sin(trd)
    yp <- s2 * sin(piS4 - plgS2) * cos(trd)
  }
  cbind(xp, yp)
}

StCoordLineD = function (trd, plg, wulff = F) {
  trd = (trd)*pi/180
  plg = (plg)*pi/180
  
  piS4 <- pi/4
  s2 <- sqrt(2)
  plgS2 <- plg/2
  
  xp = rep(0,length(trd))
  yp = rep(0,length(trd))
  
  for (i in 1:length(trd)) {
    if (plg[i] < 0) {
      trd[i] <- ZeroTwoPi(trd[i] + pi)
      plg[i] <- -plg[i]
    }
    
    if (wulff) {
      xp[i] <- tan(piS4 - plgS2[i]) * sin(trd[i])
      yp[i] <- tan(piS4 - plgS2[i]) * cos(trd[i])
    } else {
      xp[i] <- s2 * sin(piS4 - plgS2[i]) * sin(trd[i])
      yp[i] <- s2 * sin(piS4 - plgS2[i]) * cos(trd[i])
    }
  }
  
  tibble::tibble(xp, yp)
}

StPointD = function (az, iang) {
  
  fmod = function (k, m) 
  {
    j = floor(k/m)
    a = k - m * j
    return(a)
  }
  
  FixDip = function (A) 
  {
    az = A$az
    dip = A$dip
    DEG2RAD = pi/180
    RAD2DEG = 180/pi
    tdip = DEG2RAD * fmod(dip, 360)
    co = cos(tdip)
    si = sin(tdip)
    ang = RAD2DEG * atan2(si, co)
    quad = rep(1, length(dip))
    quad[co >= 0 & si >= 0] = 1
    quad[co < 0 & si >= 0] = 2
    quad[co < 0 & si < 0] = 3
    quad[co >= 0 & si < 0] = 4
    dip[quad == 1] = ang[quad == 1]
    dip[quad == 2] = 180 - ang[quad == 2]
    dip[quad == 3] = 180 + ang[quad == 3]
    dip[quad == 4] = -ang[quad == 4]
    az[quad == 1] = az[quad == 1]
    az[quad == 2] = 180 + az[quad == 2]
    az[quad == 3] = az[quad == 3]
    az[quad == 4] = 180 + az[quad == 4]
    A$az = fmod(az, 360)
    A$dip = dip
    return(A)
  }
  
  DEG2RAD = pi/180
  sph = cos(DEG2RAD * iang)
  sph[sph >= 0] = 0
  sph[sph < 0] = 1
  
  A = list(az = az, dip = iang)
  A$dip = iang
  A$az = az
  B = FixDip(A)
  trot = DEG2RAD * B$az
  tdip = B$dip
  xi = DEG2RAD * tdip
  tq = sqrt(2) * sin(xi/2)
  pltx = tq * sin(trot)
  plty = tq * cos(trot)
  
  return(tibble::tibble(x = pltx, y = plty))
}

dir_stats_spher = function(trd, plg, data = NULL, conf.level = .95,
                           type = c('line','dir','strike'), 
                           plot = c('all','eig','mean')) {
  
  a = 1 - conf.level
  
  if (is.null(data)) {
    trd = trd
    plg = plg
  } else {
    trd = data %>% dplyr::pull({{trd}})
    plg = data %>% dplyr::pull({{plg}})
  }
  
  watson_k1 = rio::import('data/spherical_watson_kappas.xlsx',
                          sheet = 'bipolar',
                          setclass = 'tbl')
  watson_k3 = rio::import('data/spherical_watson_kappas.xlsx',
                          sheet = 'girdle',
                          setclass = 'tbl')
  bingham_kappas = rio::import('data/spherical_bingham_kappas.xlsx',
                               setclass = 'tbl')
  
  if (any(type == 'line')) {
    d = trd
    i = plg
  } else if (type == 'dir') {
    d = trd + 180
    i = 90 - plg
  } else if (type == 'strike') {
    d = trd - 90
    i = 90 - plg
  }
  
  d.plot = ifelse(i < 0, ifelse(d+180 > 360, d-180, d+180), d)
  i.plot = ifelse(i < 0, -i, i)
  
  N = length(d)
  drad = d * pi/180
  irad = i * pi/180
  x = cos(irad) * cos(drad)
  y = cos(irad) * sin(drad)
  z = sin(irad)
  cosines = data.frame(x,y,z) %>% as.matrix()
  Sx = sum(x)
  Sy = sum(y)
  Sz = sum(z)
  R0 = sqrt(Sx^2+Sy^2+Sz^2)
  # R0 = sqrt(sum(colSums(cosines)^2))
  Rbar = R0/N
  
  Rbar0 = sqrt(sum(cos(i.plot * pi/180) * cos(d.plot * pi/180))^2 + 
                 sum(cos(i.plot * pi/180) * sin(d.plot * pi/180))^2 + 
                 sum(sin(i.plot * pi/180))^2)/N
  
  if (all(i >= 0) & Sz < 0) {
    Sx <- -Sx
    Sy <- -Sy
    Sz <- -Sz
  }
  
  # h = colSums(cosines)/R0
  xh = Sx/R0
  yh = Sy/R0
  zh = Sz/R0
  res <- CartToSphD(xh,yh,zh)
  
  d.sig = 1-(cosines %*% c(xh, yh, zh) %>% .^2 %>% sum())/N
  sig_s = sqrt(d.sig/(N*Rbar^2))
  cone_s = suppressWarnings(asin(sqrt(-log(a))*sig_s)*180/pi)
  cone_s = ifelse(is.na(cone_s),
                  sqrt(-log(a))*sig_s*180/pi,
                  cone_s)
  
  if (N < 16){
    afact <- 1.0-(1.0/N)
    conc <- (N/(N-R0))*afact^2
  } else {
    conc <- (N-1.0)/(N-R0)
    # conc = Rbar * (3 - Rbar^2)/(1 - Rbar^2)
  }
  
  mean_dir = ifelse(res[1] + 180 > 360, 
                    res[1] - 180, 
                    res[1] + 180)
  mean_strike = d2s(mean_dir)
  mean_dip = 90 - res[2]
  
  # condifence cone for mean vector
  cono = (acos(1 - ((N - R0)/R0) * ((1/a)^(1/(N - 1)) - 1))) * 180/pi
  
  # eigen
  a11 = sum(x^2)
  a12 = sum(x * y)
  a13 = sum(x * z)
  a22 = sum(y^2)
  a33 = sum(z^2)
  a23 = sum(y * z)
  O = matrix(c(a11, a12, a13, a12, a22, a23, a13, a23, a33), 
             nrow = 3)
  stress = eigen(O)
  eigval = as.data.frame(stress[1])
  names(eigval) = "lambda"
  eigval$S = sapply(1:3, function(x) eigval[x, 1]/sum(eigval))
  s = eigval$S
  row.names(eigval) = c("V1", "V2", "V3")
  eigvec = as.data.frame(stress[2])
  correctedvec = ifelse(eigvec[3, ] < 0, -eigvec[1:3, ], eigvec[1:3, ])
  eigvec = as.data.frame(correctedvec)
  names(eigvec) = c("V1", "V2", "V3")
  row.names(eigvec) = c("x", "y", "z")
  
  # confidence cone and ellipse for principal axis of symmetric bipolar distr
  sig_c1 = (cosines %*% eigvec[,1] %>% .^4 %>% sum())/nrow(cosines)
  
  cone.1 = suppressWarnings((asin((sqrt((s[1]-sig_c1)/nrow(cosines))/(s[1]-s[2]))*
                                    sqrt(-log(a))))*180/pi)
  cone.1 = ifelse(is.na(cone.1),0,cone.1)
  
  e11.1 = (nrow(cosines)*(s[3]-s[1])^2)^(-1) * 
    sum(((cosines %*% eigvec[,3])^2)*((cosines %*% eigvec[,1])^2))
  e22.1 = (nrow(cosines)*(s[2]-s[1])^2)^(-1) * 
    sum(((cosines %*% eigvec[,2])^2)*((cosines %*% eigvec[,1])^2))
  e12.1 = (nrow(cosines)*(s[3]-s[1])*(s[2]-s[1]))^(-1) * 
    sum((cosines %*% eigvec[,3])*(cosines %*% eigvec[,2])*((cosines %*% eigvec[,1])^2))
  
  E.1 = matrix(c(e11.1,e12.1,e12.1,e22.1),nrow = 2)
  F.1 = solve(E.1)
  A.1 = F.1[1,1]
  B.1 = F.1[1,2]
  C.1 = F.1[2,2]
  D.1 = -2*log(a)/nrow(cosines)
 
  Z.1 = matrix(c(A.1,B.1,B.1,C.1),nrow = 2)

  g.1 = sort(sqrt(D.1/eigen(Z.1)$values),T)
  
  betas.v1 = suppressWarnings(asin(g.1)*180/pi)
  
  if (anyNA(betas.v1)) {betas.v1 = c(0,0)}
  
  # confidence cone and ellipse for polar axis of symmetric girdle distr
  sig_c3 = (cosines %*% eigvec[,3] %>% .^4 %>% sum())/nrow(cosines)
  
  cone.3 = suppressWarnings((asin((sqrt((s[3]-sig_c3)/nrow(cosines))/(s[2]-s[3]))*
                                    sqrt(-log(a))))*180/pi)
  cone.3 = ifelse(is.na(cone.3),0,cone.3)
  
  e11.3 = (nrow(cosines)*(s[3]-s[1])^2)^(-1) * 
    sum(((cosines %*% eigvec[,3])^2)*((cosines %*% eigvec[,1])^2))
  e22.3 = (nrow(cosines)*(s[3]-s[2])^2)^(-1) * 
    sum(((cosines %*% eigvec[,3])^2)*((cosines %*% eigvec[,2])^2))
  e12.3 = (nrow(cosines)*(s[3]-s[2])*(s[3]-s[1]))^(-1) * 
    sum((cosines %*% eigvec[,3])^2*(cosines %*% eigvec[,2])*((cosines %*% eigvec[,1])))
  
  E.3 = matrix(c(e11.3,e12.3,e12.3,e22.3),nrow = 2)
  F.3 = solve(E.3)
  A.3 = F.3[1,1]
  B.3 = F.3[1,2]
  C.3 = F.3[2,2]
  D.3 = -2*log(a)/nrow(cosines)
 
  Z.3 = matrix(c(A.3,B.3,B.3,C.3),nrow = 2)

  g.3 = sort(sqrt(D.3/eigen(Z.3)$values),T)
  
  betas.v3 = suppressWarnings(asin(g.3)*180/pi)
  
  if (anyNA(betas.v3)) {betas.v3 = c(0,0)}
  
  # confidence cone and ellipse for V2
  sig_c2 = (cosines %*% eigvec[,2] %>% .^4 %>% sum())/nrow(cosines)
  
  cone.2 = suppressWarnings(asin((sqrt((s[2]-sig_c2)/nrow(cosines))/(s[1]-s[2]))*
                                   sqrt(-log(a)))*180/pi)
  cone.2 = ifelse(is.na(cone.2),0,cone.2)
  
  e11.2 = (nrow(cosines)*(s[2]-s[1])^2)^(-1) * 
    sum(((cosines %*% eigvec[,2])^2)*((cosines %*% eigvec[,1])^2))
  e22.2 = (nrow(cosines)*(s[2]-s[3])^2)^(-1) * 
    sum(((cosines %*% eigvec[,2])^2)*((cosines %*% eigvec[,3])^2))
  e12.2 = (nrow(cosines)*(s[1]-s[2])*(s[2]-s[3]))^(-1) * 
    sum((cosines %*% eigvec[,3])*(cosines %*% eigvec[,1])*((cosines %*% eigvec[,2])^2))
  
  E.2 = matrix(c(e11.2,e12.2,e12.2,e22.2),nrow = 2)
  F.2 = solve(E.2)
  A.2 = F.2[1,1]
  B.2 = F.2[1,2]
  C.2 = F.2[2,2]
  D.2 = -2*log(a)/nrow(cosines)
  
  Z.2 = matrix(c(A.2,B.2,B.2,C.2),nrow = 2)
  
  g.2 = sort(sqrt(D.2/eigen(Z.2)$values),T)
  
  betas.v2 = suppressWarnings(asin(g.2)*180/pi)
  
  if (anyNA(betas.v2)) {betas.v2 = c(0,0)}
  
  # kappa Watson bipolar distr
  if (1/3 <= s[1] & s[1] <= .34) {
    conc.wb = 3.75*(3*s[1]-1)
  } else if (.34 < s[1] & s[1] <= .64) {
    conc.wb = -5.95 + 14.9*s[1] + 1.48*(1-s[1])^(-1) - 11.05*s[1]^2
  } else if (s[1] > .64) {
    conc.wb = -7.96 + 21.5*s[1] + (1-s[1])^(-1) - 13.25*s[1]^2
  }
  
  # kappa Watson girdle distr
  if (0 <= s[3] & s[3] <= .06) {
    conc.wg = -((2*s[3])^(-1))
  } else if (.06 < s[3] & s[3] <= .32) {
    conc.wg = -(.961 - 7.08*s[3] + .466/s[3])
  } else if (.32 < s[3] & s[3] <= 1/3) {
    conc.wg = -(3.75*(1-3*s[3]))
  }
  
  # bingham kappas
  k1 = 0
  if (s[2]<.02 & s[3]<.02) {
    k2 = -(2*s[2])^-1
    k3 = -(2*s[3])^-1
  } else if (s[3]<.02) {
    k2 = with(bingham_kappas,
              pracma::interp2(seq(.02,.32,.02),
                              seq(.02,.48,.02),
                              matrix(k2,nrow = 24,ncol = 16),
                              .02,s[2]))
    k3 = -(2*s[3])^-1
  } else {
    k2 = with(bingham_kappas,
              pracma::interp2(seq(.02,.32,.02),
                              seq(.02,.48,.02),
                              matrix(k2,nrow = 24,ncol = 16),
                              s[3],s[2]))
    k3 = with(bingham_kappas,
              pracma::interp2(seq(.02,.32,.02),
                              seq(.02,.48,.02),
                              matrix(k3,nrow = 24,ncol = 16),
                              s[3],s[2]))
  }
  
  #
  TP = matrix(0, nrow = 2, ncol = 3)
  colnames(TP) = c("V1", "V2", "V3")
  row.names(TP) = c("trd", "plg")
  
  DD = matrix(0, nrow = 2, ncol = 3)
  colnames(DD) = c("V1", "V2", "V3")
  row.names(DD) = c("dir", "dip")
  
  TP[,1] = CartToSphD(eigvec[1,1],eigvec[2,1],eigvec[3,1])
  TP[,2] = CartToSphD(eigvec[1,2],eigvec[2,2],eigvec[3,2])
  TP[,3] = CartToSphD(eigvec[1,3],eigvec[2,3],eigvec[3,3])
  
  TP = round(TP, 2)
  
  for (r in 1:3) {
    DD[1,r] = TP[1,r]+180
    if (DD[1, r] > 360) {
      DD[1, r] = DD[1, r]-360
    }
    DD[2,r] = 90-TP[2,r]
  }
  
  STATS = tibble::tibble(trd = res[1], 
                         plg = res[2], 
                         strike = mean_strike,
                         dir = mean_dir,
                         dip = mean_dip,
                         R = R0,
                         Rbar = Rbar, 
                         kappa = conc, 
                         cone = cono,
                         N = N)
  
  trd.plot = ifelse(STATS$plg < 0, 
                    ifelse(STATS$trd>180, 
                           STATS$trd-180, 
                           STATS$trd+180),
                    STATS$trd)
  plg.plot = ifelse(STATS$plg < 0, -STATS$plg, STATS$plg)
  
  PGR = tibble::tibble(P = s[1]-s[2], G = 2*(s[2]-s[3]), R = 3*s[3], B = 1-R,
                       s1.s2 = s[1]/s[2], s1.s3 = s[1]/s[3], s2.s3 = s[2]/s[3],
                       r1 = log(s2.s3), r2 = log(s1.s2),
                       K = r2/r1, C = log(s1.s3),
                       I = sum((s-1/3)^2)*15/2, D = sqrt(sum((s-1/3)^2)*3/2)) %>% 
    dplyr::relocate(I,D,K,C,.after = B)
  
  STATS2 = tibble::tibble(V1_cone = cone.1, 
                          V2_cone = cone.2,
                          V3_cone = cone.3, 
                          kappa_bipolar = conc.wb, 
                          kappa_girdle = conc.wg)
  
  STATS3 = data.frame(V1 = c(k1,betas.v1),
                      V2 = c(k2,betas.v2),
                      V3 = c(k3,betas.v3)) %>% 
    round(3) %>% 
    `row.names<-`(c('bingham kappa','watson max ellipse','watson min ellipse'))
  
  eigenplot = ggplot2::ggplot(tibble::tibble(r1=PGR$r1,r2=PGR$r2),
                              ggplot2::aes(r1,r2)) + 
    ggplot2::geom_abline(slope = c(.2,.5,.8,2,5),intercept = 0,
                         col='purple',linewidth=.2,linetype=2) + 
    ggplot2::geom_abline(slope = -1,intercept = c(2,4,6),
                         col='darkgreen',linewidth=.2,linetype=2) + 
    ggplot2::geom_abline(slope = 1,intercept = 0,col='red') + 
    ggplot2::geom_point(col='dodgerblue',size=2) + 
    ggplot2::labs(x = expression(log(s[2]/s[3])), y = expression(log(s[1]/s[2]))) +
    ggplot2::coord_fixed(xlim = c(0,ceiling(max(PGR[,c('r1','r2')])*1.1)),
                         ylim = c(0,ceiling(max(PGR[,c('r1','r2')])*1.1)),
                         expand = F) +
    ggplot2::theme_bw()
  
  pgrplot = ggtern::ggtern(PGR,ggtern::aes(R,P,G)) + 
    ggplot2::geom_point(col='dodgerblue',size=2) +
    ggplot2::theme_bw() + 
    ggtern::theme_tropical() +
    ggtern::theme_arrowdefault() + 
    ggtern::theme_showgrid() + 
    ggtern::theme_clockwise() +
    ggtern::theme_rotate() +
    ggtern::theme_hidemask()
  
  mean.shape = 'triangle' # 8
  eigen.shape = 'square'
  
  if (any(plot == 'all')) {
    stereoplot = ggstereo() + 
      # geom_point(ggplot2::aes(x,y),StPointD(d,90-i),size=2,alpha=.5) + 
      ggplot2::geom_point(ggplot2::aes(xp,yp),StCoordLineD(d.plot,i.plot),
                          size=2,alpha=.5) +
      ggplot2::geom_path(ggplot2::aes(xp,yp,group=path),
                         SmallCircleD(trd.plot,plg.plot,cono,closed = T) %>% 
                           dplyr::bind_rows(.id = 'path'),col='gold') +
      ggplot2::geom_point(ggplot2::aes(x,y),StPointD(trd.plot,90-plg.plot),
                          col='gold',size=3,alpha=.7,shape=mean.shape,stroke=.75) + 
      ggplot2::geom_path(ggplot2::aes(xp,yp,group=path),
                         SmallCircleD(TP[1,1],TP[2,1],cone.1,closed = T) %>% 
                           dplyr::bind_rows(.id = 'path'),col='red') +
      ggplot2::geom_point(ggplot2::aes(x,y),StPointD(TP[1,1],90-TP[2,1]),
                          col='red',size=3,alpha=.7,shape=eigen.shape,stroke=.75) + 
      ggplot2::geom_path(ggplot2::aes(xp,yp,group=path),
                         SmallCircleD(TP[1,2],TP[2,2],cone.2,closed = T) %>% 
                           dplyr::bind_rows(.id = 'path'),col='green3') +
      ggplot2::geom_point(ggplot2::aes(x,y),StPointD(TP[1,2],90-TP[2,2]),
                          col='green3',size=3,alpha=.7,shape=eigen.shape,stroke=.75) + 
      ggplot2::geom_path(ggplot2::aes(xp,yp,group=path),
                         SmallCircleD(TP[1,3],TP[2,3],cone.3,closed = T) %>% 
                           dplyr::bind_rows(.id = 'path'),col='blue') +
      ggplot2::geom_point(ggplot2::aes(x,y),StPointD(TP[1,3],90-TP[2,3]),
                          col='blue',size=3,alpha=.7,shape=eigen.shape,stroke=.75)
  } else if (plot == 'eig') {
    stereoplot = ggstereo() + 
      ggplot2::geom_point(ggplot2::aes(xp,yp),StCoordLineD(d.plot,i.plot),
                          size=2,alpha=.5) +
      ggplot2::geom_path(ggplot2::aes(xp,yp,group=path),
                         SmallCircleD(TP[1,1],TP[2,1],cone.1,closed = T) %>% 
                           dplyr::bind_rows(.id = 'path'),col='red') +
      ggplot2::geom_point(ggplot2::aes(x,y),StPointD(TP[1,1],90-TP[2,1]),
                          col='red',size=3,alpha=.7,shape=eigen.shape,stroke=.75) + 
      ggplot2::geom_path(ggplot2::aes(xp,yp,group=path),
                         SmallCircleD(TP[1,2],TP[2,2],cone.2,closed = T) %>% 
                           dplyr::bind_rows(.id = 'path'),col='green3') +
      ggplot2::geom_point(ggplot2::aes(x,y),StPointD(TP[1,2],90-TP[2,2]),
                          col='green3',size=3,alpha=.7,shape=eigen.shape,stroke=.75) + 
      ggplot2::geom_path(ggplot2::aes(xp,yp,group=path),
                         SmallCircleD(TP[1,3],TP[2,3],cone.3,closed = T) %>% 
                           dplyr::bind_rows(.id = 'path'),col='blue') +
      ggplot2::geom_point(ggplot2::aes(x,y),StPointD(TP[1,3],90-TP[2,3]),
                          col='blue',size=3,alpha=.7,shape=eigen.shape,stroke=.75)
  } else if (plot == 'mean') {
    stereoplot = ggstereo() + 
      ggplot2::geom_point(ggplot2::aes(xp,yp),StCoordLineD(d.plot,i.plot),
                          size=2,alpha=.5) +
      ggplot2::geom_path(ggplot2::aes(xp,yp,group=path),
                         SmallCircleD(trd.plot,plg.plot,cono,closed = T) %>% 
                           dplyr::bind_rows(.id = 'path'),col='gold') +
      ggplot2::geom_point(ggplot2::aes(x,y),StPointD(trd.plot,90-plg.plot),
                          col='gold',size=3,alpha=.7,shape=mean.shape,stroke=.75)
  }
  
    res.lst = list(eigen_values = t(round(eigval,4)),
                   eigen_vectors = eigvec,
                   orientation_matrix = O,
                   trend_plunge = TP, 
                   dir_dip = DD, 
                   vector_stats = dplyr::bind_cols(STATS,x=xh,y=yh,z=zh), 
                   axis_stats_watson = STATS2,
                   axis_stats_aniso = STATS3,
                   PointGirdleRandom = PGR,
                   eigenplot = eigenplot, 
                   pgrplot = pgrplot,
                   stereoplot = stereoplot)
  
  return(res.lst)
}


dir_aov_sph_ax = function(trd, plg, grp, data = NULL, conf.level = .95,
                          type = c('line','dir','strike')) {
  
  a = 1 - conf.level
  
  if (is.null(data)) {
    trd = trd
    plg = plg
    grp = grp
  } else {
    trd = data %>% dplyr::pull({{trd}})
    plg = data %>% dplyr::pull({{plg}})
    grp = data %>% dplyr::pull({{grp}})
  }
  
  if (any(type == 'line')) {
    d = trd
    i = plg
  } else if (type == 'dir') {
    d = trd + 180
    i = 90 - plg
  } else if (type == 'strike') {
    d = trd - 90
    i = 90 - plg
  }
  
  d.plot = ifelse(i < 0, ifelse(d+180 > 360, d-180, d+180), d)
  i.plot = ifelse(i < 0, -i, i)
  dat.plot = StCoordLineD(d.plot,i.plot) %>% 
    dplyr::mutate(grp=grp)
  
  N = length(d)
  drad = d * pi/180
  irad = i * pi/180
  x = cos(irad) * cos(drad)
  y = cos(irad) * sin(drad)
  z = sin(irad)
  cosines = data.frame(x,y,z) %>% as.matrix()
  dat.df = data.frame(d,i,grp=grp) 
  
  axial_pooled = dir_stats_spher(d,i,dat.df,conf.level=conf.level)
  
  dat.df.g = dat.df %>% 
    dplyr::nest_by(grp) %>% 
    dplyr::mutate(N = nrow(data),
                  tau = list(dir_stats_spher(d,i,data,conf.level=conf.level)$eigen_values[1,]),
                  tauh = list(dir_stats_spher(d,i,data,conf.level=conf.level)$eigen_values[2,]),
                  tp = list(dir_stats_spher(d,i,data,conf.level=conf.level)$trend_plunge),
                  axc = list(dir_stats_spher(d,i,data,conf.level=conf.level)$axis_stats_watson))
  
  a11 = sum(x^2)
  a12 = sum(x * y)
  a13 = sum(x * z)
  a22 = sum(y^2)
  a33 = sum(z^2)
  a23 = sum(y * z)
  D = matrix(c(a11, a12, a13, a12, a22, a23, a13, a23, a33), 
             nrow = 3)
  stress = eigen(D)
  eigval = as.data.frame(stress[1])
  names(eigval) = "lambda"
  eigval$S = sapply(1:3, function(x) eigval[x, 1]/sum(eigval))
  s = eigval$S
  row.names(eigval) = c("V1", "V2", "V3")
  eigvec = as.data.frame(stress[2])
  correctedvec = ifelse(eigvec[3, ] < 0, -eigvec[1:3, ], eigvec[1:3, ])
  eigvec = as.data.frame(correctedvec)
  names(eigvec) = c("V1", "V2", "V3")
  row.names(eigvec) = c("x", "y", "z")
  
  PGR = tibble::tibble(P = s[1]-s[2], G = 2*(s[2]-s[3]), 
                       R = 3*s[3], B = 1-R)
  
  if (PGR$P > PGR$G) {
    sig_c = (cosines %*% eigvec[,1] %>% .^4 %>% sum())/nrow(cosines)
    g = 2*(s[1]-sig_c)/(3*s[1]-1)
    Nr = (((dat.df.g$tau %>% dplyr::bind_rows() %>% dplyr::pull(V1) %>% sum()) - 
             eigval$lambda[1])/g) %>% abs()
    tp.g = dat.df.g$tp %>% purrr::map(~.x[,1]) %>% dplyr::bind_rows() %>% 
      dplyr::mutate(cone = dat.df.g$axc %>% purrr::map(~.x[,1]) %>% 
                      dplyr::bind_rows() %>% dplyr::pull(),
                    kappa = dat.df.g$axc %>% purrr::map(~.x[,4]) %>% 
                      dplyr::bind_rows() %>% dplyr::pull(),
                    lambda = dat.df.g$tau %>% dplyr::bind_rows() %>% dplyr::pull(V1),
                    S = dat.df.g$tauh %>% dplyr::bind_rows() %>% dplyr::pull(V1),
                    grp = dat.df.g$grp,
                    n = dat.df.g$N) %>% 
      dplyr::relocate(grp,n)
    axist = 'Principal'
  } else {
    sig_c = (cosines %*% eigvec[,3] %>% .^4 %>% sum())/nrow(cosines)
    g = 2*(s[3]-sig_c)/(1-3*s[3])
    Nr = (((dat.df.g$tau %>% dplyr::bind_rows() %>% dplyr::pull(V3) %>% sum()) - 
             eigval$lambda[3])/g) %>% abs()
    tp.g = dat.df.g$tp %>% purrr::map(~.x[,3]) %>% dplyr::bind_rows() %>% 
      dplyr::mutate(cone = dat.df.g$axc %>% purrr::map(~.x[,3]) %>% 
                      dplyr::bind_rows() %>% dplyr::pull(),
                    kappa = dat.df.g$axc %>% purrr::map(~.x[,5]) %>% 
                      dplyr::bind_rows() %>% dplyr::pull(),
                    lambda = dat.df.g$tau %>% dplyr::bind_rows() %>% dplyr::pull(V3),
                    S = dat.df.g$tauh %>% dplyr::bind_rows() %>% dplyr::pull(V3),
                    grp = dat.df.g$grp,
                    n = dat.df.g$N) %>% 
      dplyr::relocate(grp,n)
    axist = 'Polar'
  }
  
  v = 2*nrow(dat.df.g)-2
  chi.crit = qchisq(1-a,v)
  
  if (Nr < chi.crit) {
    q = asin(sqrt(chi.crit)*sqrt(g/(2*N)))*180/pi
    statement = paste('Samples come from a distribution with common',
                      stringr::str_to_lower(axist),'axis')
  } else {
    statement = paste(axist,'axis differs between samples')
  }
  
  if (PGR$P > PGR$G) {
    axial_axis = axial_pooled$trend_plunge[,1]
    axial_col = 'red'
    tau = axial_pooled$eigen_values[1,1]
    tauh = axial_pooled$eigen_values[2,1]
    k = axial_pooled$axis_stats_watson[[4]]
  } else {
    axial_axis = axial_pooled$trend_plunge[,3]
    axial_col = 'blue'
    tau = axial_pooled$eigen_values[1,3]
    tauh = axial_pooled$eigen_values[2,3]
    k = axial_pooled$axis_stats_watson[[5]]
  }
  
  eig.plot = tp.g %>% 
    dplyr::mutate(trd = ifelse(plg < 0, ifelse(trd+180 > 360,
                                               trd-180,trd+180),trd),
                  plg = ifelse(plg < 0, -plg,plg)) %>% 
    with(StCoordLineD(trd,plg)) %>% 
    dplyr::mutate(grp=tp.g$grp)
  
  eig.cone.plot = tp.g %>% 
    dplyr::mutate(trd = ifelse(plg < 0, ifelse(trd+180 > 360,
                                               trd-180,trd+180),trd),
                  plg = ifelse(plg < 0, -plg,plg)) %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(sc = list(SmallCircleD(trd,plg,cone) %>%
                              dplyr::bind_rows(.id = 'path'))) %>% 
    tidyr::unnest(sc) %>% 
    dplyr::mutate(path = paste0(path,grp))
  
  pgrplot = ggtern::ggtern(PGR,ggtern::aes(R,P,G)) + 
    ggplot2::geom_point(col='dodgerblue',size=2) +
    ggplot2::theme_bw() + 
    ggtern::theme_tropical() +
    ggtern::theme_arrowdefault() + 
    ggtern::theme_showgrid() + 
    ggtern::theme_clockwise() +
    ggtern::theme_rotate() +
    ggtern::theme_hidemask()
  
  eigen.shape = 'square' # 8
  
  if (all(PGR$R > c(PGR$P, PGR$G))) {
    stereoplot = ggstereo() + 
      ggplot2::geom_point(ggplot2::aes(xp,yp,col=grp,shape=grp),dat.plot,size=2,alpha=.5) +
      ggplot2::scale_color_brewer(palette = 'Dark2')
  } else if (Nr < chi.crit) {
    stereoplot = ggstereo() + 
      ggplot2::geom_point(ggplot2::aes(xp,yp,col=grp,shape=grp),dat.plot,size=2,alpha=.5) +
      ggplot2::geom_path(ggplot2::aes(xp,yp,col=grp,group=path),eig.cone.plot) +
      ggplot2::geom_point(ggplot2::aes(xp,yp,col=grp,shape=grp),eig.plot,
                          size=4,alpha=.7) +
      ggplot2::geom_path(ggplot2::aes(xp,yp,group=path),
                         SmallCircleD(axial_axis[[1]],axial_axis[[2]],q,closed = T) %>%
                           dplyr::bind_rows(.id = 'path'),col=axial_col) +
      ggplot2::geom_point(ggplot2::aes(xp,yp),StCoordLineD(axial_axis[[1]],axial_axis[[2]]),
                          size=4,alpha=.7,col=axial_col,shape=eigen.shape,stroke=.75) +
      ggplot2::scale_color_brewer(palette = 'Dark2')
  } else {
    stereoplot = ggstereo() + 
      ggplot2::geom_point(ggplot2::aes(xp,yp,col=grp,shape=grp),dat.plot,size=2,alpha=.5) +
      ggplot2::geom_path(ggplot2::aes(xp,yp,col=grp,group=path),eig.cone.plot) +
      ggplot2::geom_point(ggplot2::aes(xp,yp,col=grp,shape=grp),eig.plot,
                          size=4,alpha=.7) +
      ggplot2::scale_color_brewer(palette = 'Dark2')
  }
  
  
  if (all(PGR$R > c(PGR$P, PGR$G))) {
    res.lst = list(
      PGR = PGR,
      statement = 'Pooled data follows a random distribution',
      pgrplot = pgrplot,
      stereoplot = stereoplot
      )
  } else if (Nr < chi.crit) {
    res.lst = list(
      PGR = PGR,
      statement = statement,
      axial_g = tp.g,
      axial_res = tibble::tibble(sigma = sig_c, g = g, 
                                 Nr = Nr, p.value = pchisq(Nr,v,lower.tail = F),
                                 trd = axial_axis[[1]], 
                                 plg = axial_axis[[2]],
                                 cone = q,
                                 kappa = k,
                                 N = N,
                                 lambda = tau,
                                 S = tauh),
      pgrplot = pgrplot,
      stereoplot = stereoplot
    )
    
  } else {
    
    res.lst = list(
      PGR = PGR,
      statement = statement,
      axial_g = tp.g,
      axial_res = tibble::tibble(sigma = sig_c, g = g, Nr = Nr,
                                 p.value = pchisq(Nr,v,lower.tail = F),
                                 # k = k,
                                 lambda = tau,
                                 S = tauh),
      # pgrplot = pgrplot,
      stereoplot = stereoplot
    )
    
  }
  
  return(res.lst)
}

dir_aov_sph_vc = function(trd, plg, grp, data = NULL, conf.level = .95,
                          type = c('line','dir','strike')) {
  
  a = 1 - conf.level
  
  if (is.null(data)) {
    trd = trd
    plg = plg
    grp = grp
  } else {
    trd = data %>% dplyr::pull({{trd}})
    plg = data %>% dplyr::pull({{plg}})
    grp = data %>% dplyr::pull({{grp}})
  }
  
  if (any(type == 'line')) {
    d = trd
    i = plg
  } else if (type == 'dir') {
    d = trd + 180
    i = 90 - plg
  } else if (type == 'strike') {
    d = trd - 90
    i = 90 - plg
  }
  
  d.plot = ifelse(i < 0, ifelse(d+180 > 360, d-180, d+180), d)
  i.plot = ifelse(i < 0, -i, i)
  
  N = length(d)
  
  dat.plot = StCoordLineD(d.plot,i.plot) %>% 
    dplyr::mutate(grp=grp)
  
  dat.df = data.frame(d,i,grp=grp) 
  
  vector_pooled = dir_stats_spher(d,i,dat.df,conf.level=conf.level)
  
  dat.vec.aov = dat.df %>% 
    dplyr::nest_by(grp) %>% 
    dplyr::mutate(cosines = list(with(data,SphToCartD(d,i,0) %>% 
                                        tibble::as_tibble())),
                  N = nrow(data),
                  R = with(cosines,sqrt(sum(cn)^2+sum(ce)^2+sum(cd)^2)),
                  Rbar = R/N,
                  kappa = ifelse(N<16,(N/(N-R))*(1.0-(1.0/N))^2,(N-1.0)/(N-R)),
                  xh = with(cosines,sum(cn)/R),
                  yh = with(cosines,sum(ce)/R),
                  zh = with(cosines,sum(cd)/R),
                  sigc = sqrt((1-(as.matrix(cosines) %*% c(xh, yh, zh) %>% 
                                    .^2 %>% sum())/N)/(N*(R/N)^2)),
                  mean = list(CartToSphD(xh,yh,zh)),
                  cone = asin(sqrt(-log(a))*sigc)*180/pi) %>% 
    tidyr::unnest_wider(mean) %>% 
    dplyr::select(-c(data,cosines)) %>% 
    dplyr::ungroup()
  
  vec_res = dat.vec.aov %>% 
    dplyr::summarise(x = sum(xh/sigc^2),
                     y = sum(yh/sigc^2),
                     z = sum(zh/sigc^2),
                     s = sum(1/sigc^2),
                     df = 2*nrow(.)-2) %>% 
    dplyr::summarise(Nr = abs(4*(s-sqrt(x^2+y^2+z^2))),
                     df = df,
                     p.value = pchisq(Nr,df,lower.tail = F))
  
  vec.plot = dat.vec.aov %>% 
    dplyr::mutate(trd = ifelse(plg < 0, ifelse(trd+180 > 360,
                                               trd-180,trd+180),trd),
                  plg = ifelse(plg < 0, -plg,plg)) %>% 
    with(StCoordLineD(trd,plg)) %>% 
    dplyr::mutate(grp=dat.vec.aov$grp)
  
  vec.cone.plot = dat.vec.aov %>% 
    dplyr::mutate(trd = ifelse(plg < 0, ifelse(trd+180 > 360,
                                               trd-180,trd+180),trd),
                  plg = ifelse(plg < 0, -plg,plg)) %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(sc = list(SmallCircleD(trd,plg,cone) %>%
                              dplyr::bind_rows(.id = 'path'))) %>% 
    tidyr::unnest(sc) %>% 
    dplyr::mutate(path = paste0(path,grp))
  
  v = 2*nrow(dat.vec.aov)-2
  chi.crit = qchisq(1-a,v)
  
  if (vec_res$Nr < chi.crit) {
    q = vector_pooled$vector_stats
    q2 = q %>% 
      dplyr::mutate(trd = ifelse(plg < 0, ifelse(trd+180 > 360,
                                                 trd-180,trd+180),trd),
                    plg = ifelse(plg < 0, -plg, plg))
    statement = 'Samples come from a distribution with common mean direction'
  } else {
    statement = 'Mean direction differs between samples'
  }
  
  mean.shape = 'triangle' # 8
  
  if (vec_res$Nr < chi.crit) {
    stereoplot = ggstereo() + 
      ggplot2::geom_point(ggplot2::aes(xp,yp,col=grp,shape=grp),dat.plot,size=2,alpha=.5) +
      ggplot2::geom_path(ggplot2::aes(xp,yp,col=grp,group=path),vec.cone.plot) +
      ggplot2::geom_point(ggplot2::aes(xp,yp,col=grp,shape=grp),vec.plot,
                          size=4,alpha=.7) +
      ggplot2::geom_path(ggplot2::aes(xp,yp,group=path),
                         SmallCircleD(q2$trd,q2$plg,q2$cone,closed = T) %>%
                           dplyr::bind_rows(.id = 'path'),col='gold') +
      ggplot2::geom_point(ggplot2::aes(xp,yp),StCoordLineD(q2$trd,q2$plg),
                          size=4,alpha=.7,col='gold',shape=mean.shape,stroke=.75) +
      ggplot2::scale_color_brewer(palette = 'Dark2')
  } else {
    stereoplot = ggstereo() + 
      ggplot2::geom_point(ggplot2::aes(xp,yp,col=grp,shape=grp),dat.plot,size=2,alpha=.5) +
      ggplot2::geom_path(ggplot2::aes(xp,yp,col=grp,group=path),vec.cone.plot) +
      ggplot2::geom_point(ggplot2::aes(xp,yp,col=grp,shape=grp),vec.plot,
                          size=4,alpha=.7) +
      ggplot2::scale_color_brewer(palette = 'Dark2')
  }
  
  if (vec_res$Nr < chi.crit) {
    res.lst = list(
      statement = statement,
      vector_g = dat.vec.aov %>% dplyr::select(-ends_with('h')),
      vector_res = dplyr::bind_cols(vec_res,q),
      stereoplot = stereoplot
    )
  } else {
    res.lst = list(
      statement = statement,
      vector_g = dat.vec.aov %>% dplyr::select(-ends_with('h')),
      vector_res = vec_res,
      stereoplot = stereoplot
    )
  }
  
  
  return(res.lst)
  
}


rose_diag_spher = function (trd, plg, data = NULL, conf.level = .95,
                            width1 = 20, width2 = 5, alpha = .7,
                            fill.col = 'blue', mean.col = 'red',
                            type = c('line','dir','strike'),
                            show = 'all',
                            show.mean = T) {
  
  if (is.null(data)) {
    trd = trd
    plg = plg
  } else {
    trd = data %>% dplyr::pull({{trd}})
    plg = data %>% dplyr::pull({{plg}})
  }
  
  if (any(type == 'line')) {
    d = trd
    i = plg
  } else if (type == 'dir') {
    d = trd + 180
    i = 90 - plg
  } else if (type == 'strike') {
    d = trd - 90
    i = 90 - plg
  }
  
  d = ifelse(i < 0, ifelse(d+180 > 360, d-180, d+180), d)
  i = ifelse(i < 0, -i, i)
  d = ifelse(d > 360, d-360, d)
  
  dat.rose = purrr::map2(d,i,~(Pole(.x*pi/180,.y*pi/180,0)*180/pi) %>% 
         tibble::as_tibble_row()) %>% 
    purrr::list_rbind() %>% 
    dplyr::mutate(dir = s2d(strike),
                  trd = d,
                  plg = i)
  
  strike2 = dat.rose$strike + 180
  strike2 = ifelse(strike2 > 360, strike2 - 360, strike2)
  strike = c(dat.rose$strike,strike2)
  
  max.strike = DescTools::Freq(strike,seq(0,180,width1)) %>% 
    tibble::tibble() %>% 
    dplyr::slice_max(perc) %>% 
    dplyr::slice_head(n = 1) %>% 
    dplyr::pull(perc, freq) %>% 
    round(3)*100
  
  max.dir = DescTools::Freq(dat.rose$dir,seq(0,360,width1)) %>% 
    tibble::tibble() %>% 
    dplyr::slice_max(perc) %>% 
    dplyr::slice_head(n = 1) %>% 
    dplyr::pull(perc, freq) %>% 
    round(3)*100
  
  max.dip = DescTools::Freq(dat.rose$dip,seq(0,90,width2)) %>% 
    tibble::tibble() %>% 
    dplyr::slice_max(perc) %>% 
    dplyr::slice_head(n = 1) %>% 
    dplyr::pull(perc, freq) %>% 
    round(3)*100
  
  max.trd = DescTools::Freq(dat.rose$trd,seq(0,360,width1)) %>% 
    tibble::tibble() %>% 
    dplyr::slice_max(perc) %>% 
    dplyr::slice_head(n = 1) %>% 
    dplyr::pull(perc, freq) %>% 
    round(3)*100
  
  max.plg = DescTools::Freq(dat.rose$plg,seq(0,90,width2)) %>% 
    tibble::tibble() %>% 
    dplyr::slice_max(perc) %>% 
    dplyr::slice_head(n = 1) %>% 
    dplyr::pull(perc, freq) %>% 
    round(3)*100
  
  N = length(d)
  
  r = dir_stats_spher(d,i,conf.level = conf.level)
  
  PGR = r$PointGirdleRandom
  
  if (all(PGR$R > c(PGR$P, PGR$G))) {
    mean.strike = NA
    mean.dip = NA
    mean.dir = NA
    mean.trd = NA
    mean.plg = NA
  } else if (PGR$P > PGR$G) {
    mean.dip = r$dir_dip[2,1]
    mean.dir = r$dir_dip[1,1]
    mean.strike = d2s(mean.dir)
    mean.trd = r$trend_plunge[1,1]
    mean.plg = r$trend_plunge[2,1]
  } else if (PGR$G > PGR$P) {
    mean.strike = r$dir_dip[1,3]
    mean.dip = r$dir_dip[2,3]
    mean.dir = s2d(mean.strike)
    mean.trd = r$vector_stats$trd
    mean.plg = r$vector_stats$plg
  }
  
  labelmeandir = bquote("Mean dip direction =" ~ .(format(round(mean.dir, 1))) * 
                          # degree * "" %+-% "" * .(format(round(r$cone, 1))) * 
                          degree 
                        # ~ ", N =" ~ .(N)
                        )
  
  labelmeanstrike = bquote("Mean strike =" ~ .(format(round(mean.strike, 1))) * 
                             degree 
                           # ~ ", N =" ~ .(N)
                           )
  
  labelmeandip = bquote("Mean dip angle =" ~ .(format(round(mean.dip, 1))) * 
                          degree 
                        # ~ ", N =" ~ .(N)
                        )
  
  labelmeantrd = bquote("Mean trend =" ~ .(format(round(mean.trd, 1))) * 
                          degree 
                        # ~ ", N =" ~ .(N)
                        )
  
  labelmeanplg = bquote("Mean plunge =" ~ .(format(round(mean.plg, 1))) * 
                          degree 
                        # ~ ", N =" ~ .(N)
                        )
  
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
  
  strike.180 = ifelse(mean.strike > 180, mean.strike - 180, mean.strike + 180)
  
  plt.dirrose <- ggplot(dat.rose, aes(dir)) + 
    stat_bin(aes(y = sqrt(after_stat(.data$count)/max(after_stat(.data$count)) * 
                            100^2)), 
             breaks = seq(0,360,width1), 
             colour = "black",  alpha = alpha,
             fill = fill.col, closed = "left") + 
    scale_x_continuous(breaks = seq(0,360,30), 
                       minor_breaks = seq(0,360,10),
                       limits = c(0, 360)) + 
    scale_y_continuous(NULL, 
                       breaks = seq(0,100,20), 
                       labels = NULL) + 
    labs(subtitle = 'Dip direction',
         caption = paste0('Max. = ',max.dir,'%',', N = ', N)) +
    coord_radial(expand = F) +
    theme_rose
  
  plt.strikerose <- ggplot(data.frame(strike), aes(strike)) + 
    stat_bin(aes(y = sqrt(after_stat(.data$count)/max(after_stat(.data$count)) * 
                            100^2)), 
             breaks = seq(0,360,width1), 
             colour = "black",  alpha = alpha, 
             fill = fill.col, closed = "left") + 
    scale_x_continuous(breaks = seq(0,360,30), 
                       minor_breaks = seq(0,360,10),
                       limits = c(0, 360)) +
    scale_y_continuous(NULL, 
                       breaks = seq(0,100,20), 
                       labels = NULL) + 
    labs(subtitle = 'Strike',
         caption = paste0('Max. = ',max.strike,'%',', N = ', N)) +
    coord_radial(expand = F) + 
    theme_rose
  
  plt.diprose = ggplot(dat.rose, aes(dip)) + 
    stat_bin(aes(y = sqrt(after_stat(.data$count)/max(after_stat(.data$count)) * 
                            100^2)), 
             breaks = seq(0,90,width2),
             colour = "black",  alpha = alpha, 
             fill = fill.col, closed = "left") + 
    scale_x_continuous(breaks = seq(0,90,10), limits = c(0, 90)) + 
    scale_y_continuous(NULL, 
                       breaks = seq(0,100,20), 
                       labels = NULL) + 
    labs(subtitle = 'Dip angle',
         caption = paste0('Max. = ',max.dip,'%',', N = ', N)) +
    coord_radial(start = pi/2, end = pi, expand = F) + 
    theme_rose
  
  plt.trdrose <- ggplot(dat.rose, aes(trd)) + 
    stat_bin(aes(y = sqrt(after_stat(.data$count)/max(after_stat(.data$count)) * 
                            100^2)), 
             breaks = seq(0,360,width1), 
             colour = "black",  alpha = alpha,
             fill = fill.col, closed = "left") + 
    scale_x_continuous(breaks = seq(0,360,30), 
                       minor_breaks = seq(0,360,10),
                       limits = c(0, 360)) + 
    scale_y_continuous(NULL, 
                       breaks = seq(0,100,20), 
                       labels = NULL) + 
    labs(subtitle = 'Trend',
         caption = paste0('Max. = ',max.trd,'%',', N = ', N)) +
    coord_radial(expand = F) +
    theme_rose
  
  plt.plgrose = ggplot(dat.rose, aes(plg)) + 
    stat_bin(aes(y = sqrt(after_stat(.data$count)/max(after_stat(.data$count)) * 
                            100^2)), 
             breaks = seq(0,90,width2),
             colour = "black",  alpha = alpha, 
             fill = fill.col, closed = "left") + 
    scale_x_continuous(breaks = seq(0,90,10), limits = c(0, 90)) +
    scale_y_continuous(NULL, 
                       breaks = seq(0,100,20), 
                       labels = NULL) +
    labs(subtitle = 'Plunge',
         caption = paste0('Max. = ',max.plg,'%',', N = ', N)) +
    coord_radial(start = pi/2, end = pi, expand = F) + 
    theme_rose
  
  if (show.mean) {
    plt.dirrose = plt.dirrose + 
      geom_vline(xintercept = mean.dir, 
                 col = mean.col, 
                 linewidth = .5) +
      labs(subtitle = labelmeandir)
    
    plt.strikerose = plt.strikerose + 
      geom_vline(xintercept = c(mean.strike,strike.180),
                 col = mean.col,
                 linewidth = .5) +
      labs(subtitle = labelmeanstrike)
    
    plt.diprose = plt.diprose +
      geom_vline(xintercept = mean.dip,
                 col = mean.col,
                 linewidth = .5) +
      labs(subtitle = labelmeandip)
    
    plt.trdrose = plt.trdrose +
      geom_vline(xintercept = mean.trd,
                 col = mean.col,
                 linewidth = .5) +
      labs(subtitle = labelmeantrd)
    
    plt.plgrose = plt.plgrose  + 
      geom_vline(xintercept = mean.plg,
                 col = mean.col,
                 linewidth = .5) + 
      labs(subtitle = labelmeanplg)
    
  }
  
  l.all = list(strike = plt.strikerose, 
               dir = plt.dirrose, 
               dip = plt.diprose,
               trend = plt.trdrose,
               plunge = plt.plgrose)
  
  if (any(show == 'all')) {
    l.res = l.all
  } else {
    l.res = purrr::map(show,~l.all[[.x]]) %>% 
      purrr::set_names(show) 
  }
  
  return(l.res)
  
}

  
  
