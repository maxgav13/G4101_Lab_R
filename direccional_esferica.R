# esferica

library(pracma)
library(ggtern)
library(tidyverse)

files = fs::dir_ls('StructuralGeologyAlgorithms-R/')

source('StructuralGeologyAlgorithms.R',echo = F)
map(files,source,echo=F)

dat.lst = rio::import_list('data/spherical_data.xlsx',
                           setclass = 'tbl')

# PGR

PGR = tibble(file = as_factor(c('B2','B4','B5','B6',
                                'B11','B12','B13','B14')),
             type = as_factor(c(rep('vector',4),
                                rep('axial',4))),
             s1=c(.9825,.6631,.3933,.6062,
                  .9165,.5454,.6343,.5173),
             s2=c(.0104,.3347,.3499,.2731,
                  .0481,.4354,.3439,.4196),
             s3=c(.0071,.0022,.2569,.1206,
                  .0353,.0192,.0218,.0631)) %>% 
  mutate(P=s1-s2, G=2*(s2-s3), R=3*s3, B=1-R,
         s1.s2 = s1/s2, s1.s3 = s1/s3, s2.s3 = s2/s3,
         r1 = log(s2.s3), r2 = log(s1.s2),
         K = r2/r1, C = log(s1.s3),
         sample = as.factor(1:nrow(.)))
PGR

gg_pgr = ggtern(PGR,aes(R,P,G)) + 
  # geom_point(col='dodgerblue',size=3) +
  geom_point(aes(col=file,shape=type),size=3) +
  theme_bw() + 
  theme_tropical() +
  theme_arrowdefault() + 
  theme_showgrid() + 
  theme_clockwise() +
  theme_rotate()
gg_pgr

gg_eigen = ggplot(PGR,aes(r1,r2)) + 
  geom_abline(slope = c(.2,.5,.8,2,5),intercept = 0,
              col='purple',linewidth=.2,linetype=2) + 
  geom_abline(slope = -1,intercept = c(2,4,6),
              col='darkgreen',linewidth=.2,linetype=2) + 
  geom_abline(slope = 1,intercept = 0,col='red') + 
  # geom_point(col='dodgerblue',size=3) + 
  geom_point(aes(col=file,shape=type),size=3) +
  labs(x = expression(log(s[2]/s[3])), 
       y = expression(log(s[1]/s[2]))) +
  coord_fixed(xlim = c(0,ceiling(max(PGR[,c('r1','r2')])*1.1)),
              ylim = c(0,ceiling(max(PGR[,c('r1','r2')])*1.1)),
              expand = F) +
  theme_bw()
gg_eigen

img = png::readPNG('imgs/eigen_crop.png')
g = grid::rasterGrob(img, width = unit(.1, "npc"), 
                     height = unit(.1, "npc"), 
                     interpolate = FALSE)

ggplot(PGR,aes(r1,r2)) + 
  annotation_custom(g,
                    -31.5,38.5,
                    -31.5,38.5) +
  # geom_point(col='dodgerblue',size=3) +
  geom_point(aes(shape=sample),col='dodgerblue',size=3) +
  scale_x_continuous(breaks = 0:7,labels = as.character(0:7)) +
  scale_y_continuous(breaks = 0:7,labels = as.character(0:7)) +
  labs(x = expression(log(s[2]/s[3])), y = expression(log(s[1]/s[2]))) +
  coord_fixed(xlim = c(0,7),ylim = c(0,7),expand = F)

# ggsave('figs/pgr.svg', plot = gg_pgr, width = 8,height = 8,dpi = 300)


# directed data

dat = rio::import('data/spherical_data.xlsx',
                  setclass = 'tbl',
                  sheet='fisher_etal_B2')
dat

if (any(names(dat) %in% 'Trend')) {
  dat = dat %>% 
    mutate(Trend = ifelse(Plunge < 0, ifelse(Trend+180 > 360,
                                             Trend-180,Trend+180),Trend),
           Plunge = ifelse(Plunge < 0, -Plunge,Plunge))
}
dat

dat.spher = 
  tibble(trd = c(12,18,22,15,10,20),
         plg = c(42,40,48,30,42,30)
         # ,x = cos(circular::rad(plg))*cos(circular::rad(trd)),
         # y = cos(circular::rad(plg))*sin(circular::rad(trd)),
         # z = sin(circular::rad(plg))
         )

# spher.m = dat.spher %>% 
#   select(x:z) %>% 
#   as.matrix()

spher.tb = with(dat.spher,SphToCartD(trd,plg,0)) %>% 
  as_tibble() %>% 
  set_names(c('x','y','z'))

spher.m = spher.tb %>% 
  as.matrix()

Directional::fishkent(spher.m) # H0: vmf, H1: kent

spher.mle.vmf = spher.m %>% 
  Directional::vmf.mle()
spher.mle.vmf

spher.mle.kent = spher.m %>% 
  Directional::kent.mle()
spher.mle.kent

mean.cos = spher.mle.vmf$mu

spher.res = CartToSphD(mean.cos[1], mean.cos[2], mean.cos[3])
spher.res

## anova

dat2 = rio::import('data/spherical_data.xlsx',
                   setclass = 'tbl',
                   sheet='fisher_etal_B21')
dat2

dat.vec.aov = dat2 %>% 
  nest_by(Type) %>% 
  mutate(cosines = list(with(data,SphToCartD(Trend,Plunge,0) %>% 
                               as_tibble())),
         N = nrow(data),
         R = with(cosines,sqrt(sum(cn)^2+sum(ce)^2+sum(cd)^2)),
         Rbar = R/N,
         k = ifelse(N<16,(N/(N-R))*(1.0-(1.0/N))^2,(N-1.0)/(N-R)),
         xh = with(cosines,sum(cn)/R),
         yh = with(cosines,sum(ce)/R),
         zh = with(cosines,sum(cd)/R),
         sigc = sqrt((1-(as.matrix(cosines) %*% c(xh, yh, zh) %>% 
                           .^2 %>% sum())/N)/(N*(R/N)^2)),
         mean = list(CartToSphD(xh,yh,zh)),
         cone = asin(sqrt(-log(.05))*sigc)*180/pi) %>% 
  unnest_wider(mean) %>% 
  select(-c(data,cosines)) %>% 
  ungroup()

dat.vec.aov %>% 
  summarise(x = sum(xh/sigc^2),
            y = sum(yh/sigc^2),
            z = sum(zh/sigc^2),
            s = sum(1/sigc^2),
            df = 2*nrow(.)-2) %>% 
  summarise(Nr = 4*(s-sqrt(x^2+y^2+z^2)),
            df = df,
            p.value = pchisq(Nr,df,lower.tail = F))

with(dat2,SphToCartD(Trend,Plunge,0) %>% as_tibble() %>% as.matrix()) %>% 
  het.aov(dat2$Type %>% as.factor())

# undirected data - bipolar

dat = rio::import('data/spherical_data.xlsx',
                  setclass = 'tbl',
                  sheet='fisher_etal_B11'
                  # sheet='fisher_etal_B17'
                  # sheet='fisher_etal_B18'
                  )
dat

if (any(names(dat) %in% 'Trend')) {
  dat = dat %>% 
    mutate(Trend = ifelse(Plunge < 0, ifelse(Trend+180 > 360,
                                             Trend-180,Trend+180),Trend),
           Plunge = ifelse(Plunge < 0, -Plunge,Plunge))
}
dat

if (any(names(dat) %in% 'Dip')) {
  cosines = SphToCartD(dat$`Dip Direction`+180,90-dat$Dip,0) %>% 
    as_tibble() %>% 
    as.matrix()
  
  V = dir_3D_eigen(dat$`Dip Direction`,dat$Dip,type = 'dir',results = 'full')$eigen_vectors
  S = dir_3D_eigen(dat$`Dip Direction`,dat$Dip,type = 'dir',results = 'full')$eigen_values[2,]
} else if (any(names(dat) %in% 'Trend')) {
  cosines = SphToCartD(dat$Trend,dat$Plunge,0) %>% 
    as_tibble() %>% 
    as.matrix()
  
  V = dir_3D_eigen(dat$Trend,dat$Plunge,results = 'full')$eigen_vectors
  S = dir_3D_eigen(dat$Trend,dat$Plunge,results = 'full')$eigen_values[2,]
}


## 6.3.1(ii) Confidence ellipse for principal axis of bipolar distr

a = .05

e11.1 = (nrow(cosines)*(S[3]-S[1])^2)^(-1) * 
  sum(((cosines %*% V[,3])^2)*((cosines %*% V[,1])^2))
e22.1 = (nrow(cosines)*(S[2]-S[1])^2)^(-1) * 
  sum(((cosines %*% V[,2])^2)*((cosines %*% V[,1])^2))
e12.1 = (nrow(cosines)*(S[3]-S[1])*(S[2]-S[1]))^(-1) * 
  sum((cosines %*% V[,3])*(cosines %*% V[,2])*((cosines %*% V[,1])^2))

E.1 = matrix(c(e11.1,e12.1,e12.1,e22.1),nrow = 2)
F.1 = solve(E.1)
A.1 = F.1[1,1]
B.1 = F.1[1,2]
C.1 = F.1[2,2]
D.1 = -2*log(a)/nrow(cosines)

# a.1 = (A.1-C.1)/(2*B.1) + sqrt((A.1-C.1)^2/(4*B.1^2)+1)
# 
# aa.1 = a.1/sqrt(1+a.1^2)
# bb.1 = 1/sqrt(1+a.1^2)

Z.1 = matrix(c(A.1,B.1,B.1,C.1),nrow = 2)
# Y.1 = matrix(c(aa.1,bb.1,-bb.1,aa.1),nrow = 2)
# X.1 = t(Y.1) %*% Z.1 %*% Y.1
# 
# g1.1 = sqrt(D.1/X.1[1,1])
g.1 = sort(ifelse(sqrt(D.1/eigen(Z.1)$values) >= 1, 
                  .99, 
                  sqrt(D.1/eigen(Z.1)$values)),T)

betas.v1 = asin(g.1)*180/pi
betas.v1

## 6.3.1(v) Confidence cone for principal axis of symmetric bipolar distr

a = .05

sig_c1 = (cosines %*% V[,1] %>% .^4 %>% sum())/nrow(cosines)

cone.1 = asin((sqrt((S[1]-sig_c1)/nrow(cosines))/(S[1]-S[2]))*
                 sqrt(-log(a)))*180/pi
(cone.1 = ifelse(is.na(cone.1),0,cone.1))

## 6.3.2(iv) Parameter estimation Watson bipolar distr

watson_k1 = rio::import('data/spherical_watson_kappas.xlsx',
                        sheet = 'bipolar',
                        setclass = 'tbl')

wk1 = with(watson_k1,
           pracma::interp1(s1,k,S[1]))
wk1

if (1/3 <= S[1] & S[1] <= .34) {
  conc.wb = 3.75*(3*S[1]-1)
} else if (.34 < S[1] & S[1] <= .64) {
  conc.wb = -5.95 + 14.9*S[1] + 1.48*(1-S[1])^(-1) - 11.05*S[1]^2
} else if (S[1] > .64) {
  conc.wb = -7.96 + 21.5*S[1] + (1-S[1])^(-1) - 13.25*S[1]^2
}
conc.wb

## Bingham kappas (mardia & jupp, 2000)
d.b = S[2]-S[3]
s.b = S[2]+S[3]
k0.b = -conc.wb
delta.b = (2*d.b*k0.b)/(s.b*(k0.b-1.5)+1)

conc.bb.k1 = 0
conc.bb.k2 = k0.b + delta.b
conc.bb.k3 = k0.b - delta.b
conc.bb.k1;conc.bb.k2;conc.bb.k3

# undirected data - girdle

dat = rio::import('data/spherical_data.xlsx',
                  setclass = 'tbl',
                  # sheet='fisher_etal_B12'
                  sheet='fisher_etal_B19'
                  )
dat

if (any(names(dat) %in% 'Trend')) {
  dat = dat %>% 
    mutate(Trend = ifelse(Plunge < 0, ifelse(Trend+180 > 360,
                                             Trend-180,Trend+180),Trend),
           Plunge = ifelse(Plunge < 0, -Plunge,Plunge))
}
dat

if (any(names(dat) %in% 'Dip')) {
  cosines = SphToCartD(dat$`Dip Direction`+180,90-dat$Dip,0) %>% 
    as_tibble() %>% 
    as.matrix()
  
  V = dir_3D_eigen(dat$`Dip Direction`,dat$Dip,type = 'dir',results = 'full')$eigen_vectors
  S = dir_3D_eigen(dat$`Dip Direction`,dat$Dip,type = 'dir',results = 'full')$eigen_values[2,]
} else if (any(names(dat) %in% 'Trend')) {
  cosines = SphToCartD(dat$Trend,dat$Plunge,0) %>% 
    as_tibble() %>% 
    as.matrix()
  
  V = dir_3D_eigen(dat$Trend,dat$Plunge,results = 'full')$eigen_vectors
  S = dir_3D_eigen(dat$Trend,dat$Plunge,results = 'full')$eigen_values[2,]
}

## 6.4.2(ii) Confidence ellipse for polar axis of girdle distr

a = .05

e11.3 = (nrow(cosines)*(S[3]-S[1])^2)^(-1) * 
  sum(((cosines %*% V[,3])^2)*((cosines %*% V[,1])^2))
e22.3 = (nrow(cosines)*(S[3]-S[2])^2)^(-1) * 
  sum(((cosines %*% V[,3])^2)*((cosines %*% V[,2])^2))
e12.3 = (nrow(cosines)*(S[3]-S[2])*(S[3]-S[1]))^(-1) * 
  sum((cosines %*% V[,3])^2*(cosines %*% V[,2])*((cosines %*% V[,1])))

E.3 = matrix(c(e11.3,e12.3,e12.3,e22.3),nrow = 2)
F.3 = solve(E.3)
A.3 = F.3[1,1]
B.3 = F.3[1,2]
C.3 = F.3[2,2]
D.3 = -2*log(a)/nrow(cosines)

# a.3 = (A.3-C.3)/(2*B.3) + sqrt((A.3-C.3)^2/(4*B.3^2)+1)

# aa.3 = a.3/sqrt(1+a.3^2)
# bb.3 = 1/sqrt(1+a.3^2)

Z.3 = matrix(c(A.3,B.3,B.3,C.3),nrow = 2)
# Y.3 = matrix(c(aa.3,bb.3,-bb.3,aa.3),nrow = 2)
# X.3 = t(Y.3) %*% Z.3 %*% Y.3
# 
g.3 = sort(ifelse(sqrt(D.3/eigen(Z.3)$values) >= 1, 
                  .99, 
                  sqrt(D.3/eigen(Z.3)$values)),T)


betas.v3 = asin(g.3)*180/pi
betas.v3

v1.3 = g.3[1]*cos((0:360)*pi/180)
v2.3 = g.3[2]*sin((0:360)*pi/180)

x.3 = aa.3*v1.3 - bb.3*v2.3
y.3 = bb.3*v1.3 + aa.3*v2.3
z.3 = sqrt(1-x.3^2-y.3^2)

ggstereo + geom_path(aes(x,y),tibble(x=.113+x.3,y=-.00531+y.3))

## 6.4.2(v) Confidence cone for polar axis of symmetric girdle distr

a = .05

sig_c3 = (cosines %*% V[,3] %>% .^4 %>% sum())/nrow(cosines)

cone.3 = asin((sqrt((S[3]-sig_c3)/nrow(cosines))/(S[2]-S[3]))*
                 sqrt(-log(a)))*180/pi
(cone.3 = ifelse(is.na(cone.3),0,cone.3))

## 6.4.3(iv) Parameter estimation Watson girdle distr

watson_k3 = rio::import('data/spherical_watson_kappas.xlsx',
                        sheet = 'girdle',
                        setclass = 'tbl')

wk3 = with(watson_k3,
           pracma::interp1(s3,k,S[3]))
wk3

if (0 <= S[3] & S[3] <= .06) {
  conc.wg = -((2*S[3])^(-1))
} else if (.06 < S[3] & S[3] <= .32) {
  conc.wg = -(.961 - 7.08*S[3] + .466/S[3])
} else if (.32 < S[3] & S[3] <= 1/3) {
  conc.wg = -(3.75*(1-3*S[3]))
}
conc.wg

## Bingham kappas (mardia & jupp, 2000)
d.g = S[1]-S[2]
s.g = S[1]+S[2]
k0.g = conc.wg
delta.g = (2*d.g*k0.g)/(s.g*(k0.g-1.5)+1)

conc.bg.k1 = 0
conc.bg.k2 = -2*delta.g
conc.bg.k3 = k0.g - delta.g
conc.bg.k1;conc.bg.k2;conc.bg.k3

## 6.6 Confidence ellipse for V2

a = .05

e11.2 = (nrow(cosines)*(S[2]-S[1])^2)^(-1) * 
  sum(((cosines %*% V[,2])^2)*((cosines %*% V[,1])^2))
e22.2 = (nrow(cosines)*(S[2]-S[3])^2)^(-1) * 
  sum(((cosines %*% V[,2])^2)*((cosines %*% V[,3])^2))
e12.2 = (nrow(cosines)*(S[1]-S[2])*(S[2]-S[3]))^(-1) * 
  sum((cosines %*% V[,3])*(cosines %*% V[,1])*((cosines %*% V[,2])^2))

E.2 = matrix(c(e11.2,e12.2,e12.2,e22.2),nrow = 2)
F.2 = solve(E.2)
A.2 = F.2[1,1]
B.2 = F.2[1,2]
C.2 = F.2[2,2]
D.2 = -2*log(a)/nrow(cosines)

# a.2 = (A.2-C.2)/(2*B.2) + sqrt((A.2-C.2)^2/(4*B.2^2)+1)

# aa.2 = a.2/sqrt(1+a.2^2)
# bb.2 = 1/sqrt(1+a.2^2)

Z.2 = matrix(c(A.2,B.2,B.2,C.2),nrow = 2)

g.2 = sort(ifelse(sqrt(D.2/eigen(Z.2)$values) >= 1, 
                  .99, 
                  sqrt(D.2/eigen(Z.2)$values)),T)

betas.v2 = asin(g.2)*180/pi
betas.v2

v1.2 = g.2[1]*cos((0:360)*pi/180)
v2.2 = g.2[2]*sin((0:360)*pi/180)

x.2 = aa.2*v1.2 - bb.2*v2.2
y.2 = bb.2*v1.2 + aa.2*v2.2
z.2 = sqrt(1-x.2^2-y.2^2)
  
ggstereo + geom_path(aes(x,y),tibble(x=-.9+x.2,y=.177+y.2))

## Confidence cone?

sig_c2 = (cosines %*% V[,2] %>% .^4 %>% sum())/nrow(cosines)

cone.2 = asin((sqrt((S[2]-sig_c2)/nrow(cosines))/(S[1]-S[2]))*
                sqrt(-log(a)))*180/pi
(cone.2 = ifelse(is.na(cone.2),0,cone.2))

## Bingham kappas
bingham_kappas = rio::import('data/spherical_bingham_kappas.xlsx',
                             setclass = 'tbl')

k1 = 0
if (S[2]<.02 & S[3]<.02) {
  k2 = -(2*S[2])^-1
  k3 = -(2*S[3])^-1
} else if (S[3]<.02) {
  k2 = with(bingham_kappas,
            pracma::interp2(seq(.02,.32,.02),
                            seq(.02,.48,.02),
                            matrix(k2,nrow = 24,ncol = 16),
                            .02,S[2]))
  k3 = -(2*S[3])^-1
} else {
  k2 = with(bingham_kappas,
            pracma::interp2(seq(.02,.32,.02),
                            seq(.02,.48,.02),
                            matrix(k2,nrow = 24,ncol = 16),
                            S[3],S[2]))
  k3 = with(bingham_kappas,
            pracma::interp2(seq(.02,.32,.02),
                            seq(.02,.48,.02),
                            matrix(k3,nrow = 24,ncol = 16),
                            S[3],S[2]))
}

k1;k2;k3

## 7.3.1(i) anova bipolar - same principal axis

2*(68.68/77-.8159)/(3*68.68/77-1)

(sum(c(32.68,37.86))-68.68)/.0908

# ((sum(c(35*32.68/35,42*37.86/42))-(77*68.68/77))/((2-1)*(3-1)))/(sum(c(35*(1-32.68/35),42*(1-37.86/42)))/((77-2)*(3-1)))

## 7.3.1(ii) bipolar common axis

asin(sqrt(qchisq(.95,2))*sqrt(.0908/(2*77)))*180/pi

## 7.4.1(i) anova girdle - same polar axis

2*(.0189-.001)/(1-3*.0189)

(sum(c(1.1879,1.3813))-2.5694)/.0379

# ((136*.0189)-(sum(c(64*.0186,72*.0192)))/((2-1)*(3-1)))/(sum(c(64*.0186,72*.0192))/(136-2*(3-1)))

## 7.4.1(ii) girdle common axis

asin(sqrt(qchisq(.95,2))*sqrt(.0379/(2*136)))*180/pi


