
library(tidyverse)

theme_set(theme_bw(base_size = 12))

mytheme = theme(strip.text.y.right = element_text(size = 12,face = 'bold',angle = 0),
                strip.background = element_rect(fill = NA),
                # legend.position = 'top',
                legend.text = element_text(size = 12),
                legend.title = element_text(size = 12,face = 'bold',hjust = .5))

# models

## 2 param models

inv_poly_1 = function(x,a,b) {x/(a+b*x)} # a>0,b>0 - b=1/meseta

tibble(a=c(2,6,10),b=.5) %>% 
  mutate(id=1:nrow(.)) %>% 
  rowwise() %>% 
  mutate(x = list(seq(0,20,length.out=100)),
         y = list(inv_poly_1(x,a,b))) %>% 
  unnest(c(x,y)) %>% 
  mutate(across(a:b,as.factor)) %>% 
  ggplot(aes(x,y,group=id,col=a)) + 
  geom_line() + 
  # geom_point(size=.5) + 
  labs(x=NULL,y=NULL) +
  facet_grid(rows = vars(b),
             labeller = label_both) + 
  mytheme

tibble(a=4,b=c(.5,.2,.1)) %>% 
  mutate(id=1:nrow(.)) %>% 
  rowwise() %>% 
  mutate(x = list(seq(0,100,length.out=100)),
         y = list(inv_poly_1(x,a,b))) %>% 
  unnest(c(x,y)) %>% 
  mutate(across(a:b,as.factor)) %>% 
  ggplot(aes(x,y,group=id,col=b)) + 
  geom_line() + 
  # geom_point(size=.5) + 
  labs(x=NULL,y=NULL) +
  facet_grid(rows = vars(a),
             labeller = label_both) + 
  mytheme

exp3 = function(x,a,b) {a*exp(b/x)} # a>0,b<0 - a=meseta

tibble(a=c(20,50,75),b=-2) %>% 
  mutate(id=1:nrow(.)) %>% 
  rowwise() %>% 
  mutate(x = list(seq(0,20,length.out=100)),
         y = list(exp3(x,a,b))) %>% 
  unnest(c(x,y)) %>% 
  mutate(across(a:b,as.factor)) %>% 
  ggplot(aes(x,y,group=id,col=a)) + 
  geom_line() + 
  # geom_point(size=.5) + 
  labs(x=NULL,y=NULL) +
  facet_grid(rows = vars(b),
             labeller = label_both) + 
  mytheme

tibble(a=10,b=c(-5,-2,-1)) %>% 
  mutate(id=1:nrow(.)) %>% 
  rowwise() %>% 
  mutate(x = list(seq(0,20,length.out=100)),
         y = list(exp3(x,a,b))) %>% 
  unnest(c(x,y)) %>% 
  mutate(across(a:b,as.factor)) %>% 
  ggplot(aes(x,y,group=id,col=b)) + 
  geom_line() + 
  # geom_point(size=.5) + 
  labs(x=NULL,y=NULL) +
  facet_grid(rows = vars(a),
             labeller = label_both) + 
  mytheme

potencia = function(x,a,b) {a*x^b} # a>0,0<b<1

tibble(a=c(2,4,10),b=.5) %>% 
  mutate(id=1:nrow(.)) %>% 
  rowwise() %>% 
  mutate(x = list(seq(0,10,length.out=100)),
         y = list(potencia(x,a,b))) %>% 
  unnest(c(x,y)) %>% 
  mutate(across(a:b,as.factor)) %>% 
  ggplot(aes(x,y,group=id,col=a)) + 
  geom_line() + 
  # geom_point(size=.5) + 
  labs(x=NULL,y=NULL) +
  facet_grid(rows = vars(b),
             labeller = label_both) + 
  mytheme

tibble(a=5,b=c(.3,.5,.8)) %>% 
  mutate(id=1:nrow(.)) %>% 
  rowwise() %>% 
  mutate(x = list(seq(0,10,length.out=100)),
         y = list(potencia(x,a,b))) %>% 
  unnest(c(x,y)) %>% 
  mutate(across(a:b,as.factor)) %>% 
  ggplot(aes(x,y,group=id,col=b)) + 
  geom_line() + 
  # geom_point(size=.5) + 
  labs(x=NULL,y=NULL) +
  facet_grid(rows = vars(a),
             labeller = label_both) + 
  mytheme

weibull = function(x,a,b) {1-exp(-a*x^b)} # a>0,b>0

tibble(a=c(.1,.5,2),b=2) %>% 
  mutate(id=1:nrow(.)) %>% 
  rowwise() %>% 
  mutate(x = list(seq(0,10,length.out=100)),
         y = list(weibull(x,a,b))) %>% 
  unnest(c(x,y)) %>% 
  mutate(across(a:b,as.factor)) %>% 
  ggplot(aes(x,y,group=id,col=a)) + 
  geom_line() + 
  # geom_point(size=.5) + 
  labs(x=NULL,y=NULL) +
  facet_grid(rows = vars(b),
             labeller = label_both) + 
  mytheme

tibble(a=.5,b=c(.5,1,2)) %>% 
  mutate(id=1:nrow(.)) %>% 
  rowwise() %>% 
  mutate(x = list(seq(0,10,length.out=100)),
         y = list(weibull(x,a,b))) %>% 
  unnest(c(x,y)) %>% 
  mutate(across(a:b,as.factor)) %>% 
  ggplot(aes(x,y,group=id,col=b)) + 
  geom_line() + 
  # geom_point(size=.5) + 
  labs(x=NULL,y=NULL) +
  facet_grid(rows = vars(a),
             labeller = label_both) + 
  mytheme

## 3 param models

inv_poly_2 = function(x,a,b,c) {x/(a+b*x+c*x^2)} # a>0,b>0,c>0 - b=1/meseta

tibble(a=c(.5,2,10)
       ,b=2
       ,c=.5) %>% 
  mutate(id=1:nrow(.)) %>% 
  rowwise() %>% 
  mutate(x = list(seq(0,10,length.out=100)),
         y = list(inv_poly_2(x,a,b,c))) %>% 
  unnest(c(x,y)) %>% 
  mutate(across(a:c,as.factor)) %>% 
  ggplot(aes(x,y,group=id,col=a)) + 
  geom_line() + 
  # geom_point(size=.5) + 
  labs(x=NULL,y=NULL) +
  facet_grid(rows = vars(b,c),
             labeller = label_both) + 
  mytheme

tibble(a=2
       ,b=c(.5,2,10)
       ,c=.5) %>% 
  mutate(id=1:nrow(.)) %>% 
  rowwise() %>% 
  mutate(x = list(seq(0,10,length.out=100)),
         y = list(inv_poly_2(x,a,b,c))) %>% 
  unnest(c(x,y)) %>% 
  mutate(across(a:c,as.factor)) %>% 
  ggplot(aes(x,y,group=id,col=b)) + 
  geom_line() + 
  # geom_point(size=.5) + 
  labs(x=NULL,y=NULL) +
  facet_grid(rows = vars(a,c),
             labeller = label_both) + 
  mytheme

tibble(a=2
       ,b=.5
       ,c=c(.5,2,10)) %>% 
  mutate(id=1:nrow(.)) %>% 
  rowwise() %>% 
  mutate(x = list(seq(0,10,length.out=100)),
         y = list(inv_poly_2(x,a,b,c))) %>% 
  unnest(c(x,y)) %>% 
  mutate(across(a:c,as.factor)) %>% 
  ggplot(aes(x,y,group=id,col=c)) + 
  geom_line() + 
  # geom_point(size=.5) + 
  labs(x=NULL,y=NULL) +
  facet_grid(rows = vars(b,a),
             labeller = label_both) + 
  mytheme

chapman_richard = function(x,a,b,c) {a*(1-exp(-b*x))^c} # a>0,b>0,c>0 - a=meseta

tibble(a=c(2,5,10)
       ,b=2
       ,c=1) %>% 
  mutate(id=1:nrow(.)) %>% 
  rowwise() %>% 
  mutate(x = list(seq(0,10,length.out=100)),
         y = list(chapman_richard(x,a,b,c))) %>% 
  unnest(c(x,y)) %>% 
  mutate(across(a:c,as.factor)) %>% 
  ggplot(aes(x,y,group=id,col=a)) + 
  geom_line() + 
  # geom_point(size=.5) + 
  labs(x=NULL,y=NULL) +
  facet_grid(rows = vars(b,c),
             labeller = label_both) + 
  mytheme

tibble(a=8
       ,b=c(.1,.5,2)
       ,c=1) %>% 
  mutate(id=1:nrow(.)) %>% 
  rowwise() %>% 
  mutate(x = list(seq(0,10,length.out=100)),
         y = list(chapman_richard(x,a,b,c))) %>% 
  unnest(c(x,y)) %>% 
  mutate(across(a:c,as.factor)) %>% 
  ggplot(aes(x,y,group=id,col=b)) + 
  geom_line() + 
  # geom_point(size=.5) + 
  labs(x=NULL,y=NULL) +
  facet_grid(rows = vars(c,a),
             labeller = label_both) + 
  mytheme

tibble(a=8
       ,b=.5
       ,c=c(.5,1,5)) %>% 
  mutate(id=1:nrow(.)) %>% 
  rowwise() %>% 
  mutate(x = list(seq(0,10,length.out=100)),
         y = list(chapman_richard(x,a,b,c))) %>% 
  unnest(c(x,y)) %>% 
  mutate(across(a:c,as.factor)) %>% 
  ggplot(aes(x,y,group=id,col=c)) + 
  geom_line() + 
  # geom_point(size=.5) + 
  labs(x=NULL,y=NULL) +
  facet_grid(rows = vars(b,a),
             labeller = label_both) + 
  mytheme

gompertz = function(x,a,b,c) {a*exp(-exp(b-c*x))} # a>0,b>0,c>0 - a=meseta

tibble(a=c(2,6,10)
       ,b=2
       ,c=1
) %>% 
  mutate(id=1:nrow(.)) %>% 
  rowwise() %>% 
  mutate(x = list(seq(0,10,length.out=100)),
         y = list(gompertz(x,a,b,c))) %>% 
  unnest(c(x,y)) %>% 
  mutate(across(a:c,as.factor)) %>% 
  ggplot(aes(x,y,group=id,col=a)) + 
  geom_line() + 
  # geom_point(size=.5) + 
  labs(x=NULL,y=NULL) +
  facet_grid(rows = vars(b,c),
             labeller = label_both) + 
  mytheme

tibble(a=2,
       b=c(.5,2,5)
       ,c=1) %>% 
  mutate(id=1:nrow(.)) %>% 
  rowwise() %>% 
  mutate(x = list(seq(0,10,length.out=100)),
         y = list(gompertz(x,a,b,c))) %>% 
  unnest(c(x,y)) %>% 
  mutate(across(a:c,as.factor)) %>% 
  ggplot(aes(x,y,group=id,col=b)) + 
  geom_line() + 
  # geom_point(size=.5) + 
  labs(x=NULL,y=NULL) +
  facet_grid(rows = vars(c,a),
             labeller = label_both) + 
  mytheme

tibble(a=2,
       b=2
       ,c=c(.5,1,2)) %>% 
  mutate(id=1:nrow(.)) %>% 
  rowwise() %>% 
  mutate(x = list(seq(0,10,length.out=100)),
         y = list(gompertz(x,a,b,c))) %>% 
  unnest(c(x,y)) %>% 
  mutate(across(a:c,as.factor)) %>% 
  ggplot(aes(x,y,group=id,col=c)) + 
  geom_line() + 
  # geom_point(size=.5) + 
  labs(x=NULL,y=NULL) +
  facet_grid(rows = vars(b,a),
             labeller = label_both) + 
  mytheme

expneg = function(x,a,b,c) {a+b*exp(c*x)} # a>0,b<0 - a=meseta, b=min(y)-a

expneg2 = function(x,a,b,c) {a*exp(c*x)+b*(1-exp(c*x))} # a>0,b>0 - a=int, b=meseta

tibble(a=c(0,2,5)
       ,b=10
       ,c=-.5) %>% 
  mutate(id=1:nrow(.)) %>% 
  rowwise() %>% 
  mutate(x = list(seq(0,10,length.out=100)),
         y = list(expneg2(x,a,b,c))) %>% 
  unnest(c(x,y)) %>% 
  mutate(across(a:c,as.factor)) %>% 
  ggplot(aes(x,y,group=id,col=a)) + 
  geom_line() + 
  # geom_point(size=.5) + 
  labs(x=NULL,y=NULL) +
  facet_grid(rows = vars(b,c),
             labeller = label_both) + 
  mytheme

tibble(a=0
       ,b=c(5,10,15)
       ,c=-.5) %>% 
  mutate(id=1:nrow(.)) %>% 
  rowwise() %>% 
  mutate(x = list(seq(0,10,length.out=100)),
         y = list(expneg2(x,a,b,c))) %>% 
  unnest(c(x,y)) %>% 
  mutate(across(a:c,as.factor)) %>% 
  ggplot(aes(x,y,group=id,col=b)) + 
  geom_line() + 
  # geom_point(size=.5) + 
  labs(x=NULL,y=NULL) +
  facet_grid(rows = vars(a,c),
             labeller = label_both) + 
  mytheme

tibble(a=0
       ,b=10
       ,c=c(-1,-.5,-.2)) %>% 
  mutate(id=1:nrow(.)) %>% 
  rowwise() %>% 
  mutate(x = list(seq(0,10,length.out=100)),
         y = list(expneg2(x,a,b,c))) %>% 
  unnest(c(x,y)) %>% 
  mutate(across(a:c,as.factor)) %>% 
  ggplot(aes(x,y,group=id,col=c)) + 
  geom_line() + 
  # geom_point(size=.5) + 
  labs(x=NULL,y=NULL) +
  facet_grid(rows = vars(a,b),
             labeller = label_both) + 
  mytheme

expvar = function(x,nug,psill,range) {nug+psill*(1-exp(-x/range))} # - nug=int, psill=max(y)-nug

tibble(nug=c(0,.5,1)
       ,psill=10
       ,range=10) %>% 
  mutate(id=1:nrow(.)) %>% 
  rowwise() %>% 
  mutate(x = list(seq(0,30,length.out=100)),
         y = list(expvar(x,nug,psill,range))) %>% 
  unnest(c(x,y)) %>% 
  mutate(across(nug:range,as.factor)) %>% 
  ggplot(aes(x,y,group=id,col=nug)) + 
  geom_line() + 
  # geom_point(size=.5) + 
  labs(x=NULL,y=NULL) +
  facet_grid(rows = vars(psill,range),
             labeller = label_both) + 
  mytheme

tibble(nug=0
       ,psill=c(5,10,15)
       ,range=10) %>% 
  mutate(id=1:nrow(.)) %>% 
  rowwise() %>% 
  mutate(x = list(seq(0,30,length.out=100)),
         y = list(expvar(x,nug,psill,range))) %>% 
  unnest(c(x,y)) %>% 
  mutate(across(nug:range,as.factor)) %>% 
  ggplot(aes(x,y,group=id,col=psill)) + 
  geom_line() + 
  # geom_point(size=.5) + 
  labs(x=NULL,y=NULL) +
  facet_grid(rows = vars(nug,range),
             labeller = label_both) + 
  mytheme

tibble(nug=0
       ,psill=10
       ,range=c(3,6,9)) %>% 
  mutate(id=1:nrow(.)) %>% 
  rowwise() %>% 
  mutate(x = list(seq(0,30,length.out=100)),
         y = list(expvar(x,nug,psill,range))) %>% 
  unnest(c(x,y)) %>% 
  mutate(across(nug:range,as.factor)) %>% 
  ggplot(aes(x,y,group=id,col=range)) + 
  geom_line() + 
  # geom_point(size=.5) + 
  labs(x=NULL,y=NULL) +
  facet_grid(rows = vars(nug,psill),
             labeller = label_both) + 
  mytheme

gaussvar = function(x,nug,psill,range) {nug+psill*(1-exp(-(x/range)^2))} # - nug=int, psill=max(y)-nug

tibble(nug=c(0,.5,1)
       ,psill=10
       ,range=10) %>% 
  mutate(id=1:nrow(.)) %>% 
  rowwise() %>% 
  mutate(x = list(seq(0,30,length.out=100)),
         y = list(gaussvar(x,nug,psill,range))) %>% 
  unnest(c(x,y)) %>% 
  mutate(across(nug:range,as.factor)) %>% 
  ggplot(aes(x,y,group=id,col=nug)) + 
  geom_line() + 
  # geom_point(size=.5) + 
  labs(x=NULL,y=NULL) +
  facet_grid(rows = vars(psill,range),
             labeller = label_both) + 
  mytheme

tibble(nug=0
       ,psill=c(5,10,15)
       ,range=10) %>% 
  mutate(id=1:nrow(.)) %>% 
  rowwise() %>% 
  mutate(x = list(seq(0,30,length.out=100)),
         y = list(gaussvar(x,nug,psill,range))) %>% 
  unnest(c(x,y)) %>% 
  mutate(across(nug:range,as.factor)) %>% 
  ggplot(aes(x,y,group=id,col=psill)) + 
  geom_line() + 
  # geom_point(size=.5) + 
  labs(x=NULL,y=NULL) +
  facet_grid(rows = vars(nug,range),
             labeller = label_both) + 
  mytheme

tibble(nug=0
       ,psill=10
       ,range=c(3,6,9)) %>% 
  mutate(id=1:nrow(.)) %>% 
  rowwise() %>% 
  mutate(x = list(seq(0,30,length.out=100)),
         y = list(gaussvar(x,nug,psill,range))) %>% 
  unnest(c(x,y)) %>% 
  mutate(across(nug:range,as.factor)) %>% 
  ggplot(aes(x,y,group=id,col=range)) + 
  geom_line() + 
  # geom_point(size=.5) + 
  labs(x=NULL,y=NULL) +
  facet_grid(rows = vars(nug,psill),
             labeller = label_both) + 
  mytheme

sphvar = function(x,nug,psill,range) {ifelse(x<=range,nug+psill*(3/2*(x/range)-1/2*(x/range)^3),
                                             nug+psill)} # - c0=int, psill=max(y)-nug

tibble(nug=c(0,.5,1)
       ,psill=10
       ,range=10) %>% 
  mutate(id=1:nrow(.)) %>% 
  rowwise() %>% 
  mutate(x = list(seq(0,30,length.out=100)),
         y = list(sphvar(x,nug,psill,range))) %>% 
  unnest(c(x,y)) %>% 
  mutate(across(nug:range,as.factor)) %>% 
  ggplot(aes(x,y,group=id,col=nug)) + 
  geom_line() + 
  # geom_point(size=.5) + 
  labs(x=NULL,y=NULL) +
  facet_grid(rows = vars(psill,range),
             labeller = label_both) + 
  mytheme

tibble(nug=0
       ,psill=c(5,10,15)
       ,range=10) %>% 
  mutate(id=1:nrow(.)) %>% 
  rowwise() %>% 
  mutate(x = list(seq(0,30,length.out=100)),
         y = list(sphvar(x,nug,psill,range))) %>% 
  unnest(c(x,y)) %>% 
  mutate(across(nug:range,as.factor)) %>% 
  ggplot(aes(x,y,group=id,col=psill)) + 
  geom_line() + 
  # geom_point(size=.5) + 
  labs(x=NULL,y=NULL) +
  facet_grid(rows = vars(nug,range),
             labeller = label_both) + 
  mytheme

tibble(nug=0
       ,psill=10
       ,range=c(10,15,20)) %>% 
  mutate(id=1:nrow(.)) %>% 
  rowwise() %>% 
  mutate(x = list(seq(0,30,length.out=100)),
         y = list(sphvar(x,nug,psill,range))) %>% 
  unnest(c(x,y)) %>% 
  mutate(across(nug:range,as.factor)) %>% 
  ggplot(aes(x,y,group=id,col=range)) + 
  geom_line() + 
  # geom_point(size=.5) + 
  labs(x=NULL,y=NULL) +
  facet_grid(rows = vars(nug,psill),
             labeller = label_both) + 
  mytheme

kostiakov_mod = function(x,alfa,beta,fc) {fc + alfa*x^(beta)} # alfa>0,beta<0 - fc=meseta

## 4 param models

inv_poly_3 = function(x,a,b,c,d) {x/(a+b*x+c*x^2+d*x^3)}

tibble(a=c(.5,2,5)
       ,b=1
       ,c=1
       ,d=1
) %>% 
  mutate(id=1:nrow(.)) %>% 
  rowwise() %>% 
  mutate(x = list(seq(0,10,length.out=100)),
         y = list(inv_poly_3(x,a,b,c,d))) %>% 
  unnest(c(x,y)) %>% 
  mutate(across(a:d,as.factor)) %>% 
  ggplot(aes(x,y,group=id,col=a)) + 
  geom_line() + 
  # geom_point(size=.5) + 
  labs(x=NULL,y=NULL) +
  facet_grid(rows = vars(d,c,b),
             labeller = label_both) + 
  mytheme

tibble(a=1
       ,b=c(.5,2,5)
       ,c=1
       ,d=1
) %>% 
  mutate(id=1:nrow(.)) %>% 
  rowwise() %>% 
  mutate(x = list(seq(0,10,length.out=100)),
         y = list(inv_poly_3(x,a,b,c,d))) %>% 
  unnest(c(x,y)) %>% 
  mutate(across(a:d,as.factor)) %>% 
  ggplot(aes(x,y,group=id,col=b)) + 
  geom_line() + 
  # geom_point(size=.5) + 
  labs(x=NULL,y=NULL) +
  facet_grid(rows = vars(d,c,a),
             labeller = label_both) + 
  mytheme

tibble(a=1
       ,b=1
       ,c=c(.5,2,5)
       ,d=1
) %>% 
  mutate(id=1:nrow(.)) %>% 
  rowwise() %>% 
  mutate(x = list(seq(0,10,length.out=100)),
         y = list(inv_poly_3(x,a,b,c,d))) %>% 
  unnest(c(x,y)) %>% 
  mutate(across(a:d,as.factor)) %>% 
  ggplot(aes(x,y,group=id,col=c)) + 
  geom_line() + 
  # geom_point(size=.5) + 
  labs(x=NULL,y=NULL) +
  facet_grid(rows = vars(d,b,a),
             labeller = label_both) + 
  mytheme

tibble(a=1
       ,b=1
       ,c=1
       ,d=c(.5,2,5)
) %>% 
  mutate(id=1:nrow(.)) %>% 
  rowwise() %>% 
  mutate(x = list(seq(0,10,length.out=100)),
         y = list(inv_poly_3(x,a,b,c,d))) %>% 
  unnest(c(x,y)) %>% 
  mutate(across(a:d,as.factor)) %>% 
  ggplot(aes(x,y,group=id,col=d)) + 
  geom_line() + 
  # geom_point(size=.5) + 
  labs(x=NULL,y=NULL) +
  facet_grid(rows = vars(c,b,a),
             labeller = label_both) + 
  mytheme

rational = function(x,a,b,c,d) {a/(1+b/x) + c/(1+d/x)} # a>0,b>0,c>0,d>0 - a+c=meseta

tibble(a=c(.5,1,2)
       ,b=1
       ,c=1
       ,d=1
) %>% 
  mutate(id=1:nrow(.)) %>% 
  rowwise() %>% 
  mutate(x = list(seq(0,10,length.out=100)),
         y = list(rational(x,a,b,c,d))) %>% 
  unnest(c(x,y)) %>% 
  mutate(across(a:d,as.factor)) %>% 
  ggplot(aes(x,y,group=id,col=a)) + 
  geom_line() + 
  # geom_point(size=.5) + 
  labs(x=NULL,y=NULL) +
  facet_grid(rows = vars(d,c,b),
             labeller = label_both) + 
  mytheme

tibble(a=1
       ,b=c(.1,1,10)
       ,c=1
       ,d=1
) %>% 
  mutate(id=1:nrow(.)) %>% 
  rowwise() %>% 
  mutate(x = list(seq(0,10,length.out=100)),
         y = list(rational(x,a,b,c,d))) %>% 
  unnest(c(x,y)) %>% 
  mutate(across(a:d,as.factor)) %>% 
  ggplot(aes(x,y,group=id,col=b)) + 
  geom_line() + 
  # geom_point(size=.5) + 
  labs(x=NULL,y=NULL) +
  facet_grid(rows = vars(d,c,a),
             labeller = label_both) + 
  mytheme


tibble(a=1
       ,b=1
       ,c=c(1,5,10)
       ,d=1
) %>% 
  mutate(id=1:nrow(.)) %>% 
  rowwise() %>% 
  mutate(x = list(seq(0,10,length.out=100)),
         y = list(rational(x,a,b,c,d))) %>% 
  unnest(c(x,y)) %>% 
  mutate(across(a:d,as.factor)) %>% 
  ggplot(aes(x,y,group=id,col=c)) + 
  geom_line() + 
  # geom_point(size=.5) + 
  labs(x=NULL,y=NULL) +
  facet_grid(rows = vars(d,b,a),
             labeller = label_both) + 
  mytheme

tibble(a=1
       ,b=1
       ,c=1
       ,d=c(1,5,10)
) %>% 
  mutate(id=1:nrow(.)) %>% 
  rowwise() %>% 
  mutate(x = list(seq(0,10,length.out=100)),
         y = list(rational(x,a,b,c,d))) %>% 
  unnest(c(x,y)) %>% 
  mutate(across(a:d,as.factor)) %>% 
  ggplot(aes(x,y,group=id,col=d)) + 
  geom_line() + 
  # geom_point(size=.5) + 
  labs(x=NULL,y=NULL) +
  facet_grid(rows = vars(c,b,a),
             labeller = label_both) + 
  mytheme
