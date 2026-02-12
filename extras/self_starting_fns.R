

SSngm <- selfStart(
  function(x,a,b) {a*(1-exp(-b*x))}, 
  function(mCall, LHS, data, ...) {
    xy <- sortedXyData(mCall[["x"]], LHS, data)
    x <-  xy[, "x"]; y <- xy[, "y"]
    
    a = max(y)*1.001
    
    pseudoy = log(1 - (y/a))
    
    ## Linear regression on pseudo-y and pseudo-x
    coefs <- (coef( lm(pseudoy ~ x-1) ))
    b = -coefs[1]
    start <- c(a, b)
    names(start) <- mCall[c("a", "b")]
    start
  }, 
  parameters=c("a", "b")
)


SSchaprich <- selfStart(
  function(x,a,b,c) {a*(1-exp(-b*x))^c}, 
  function(mCall, LHS, data, ...) {
    xy <- sortedXyData(mCall[["x"]], LHS, data)
    x <-  xy[, "x"]; y <- xy[, "y"]
    
    a = max(y)*1.001
    c = 1
    
    pseudoy = log(1 - (y/a)^(1/c))
    
    ## Linear regression on pseudo-y and pseudo-x
    coefs <- (coef( lm(pseudoy ~ x-1) ))
    b = -coefs[1]
    start <- c(a, b, c)
    names(start) <- mCall[c("a", "b", "c")]
    start
  }, 
  parameters=c("a", "b", "c")
)


SSpow <- selfStart(
  function(x,a,b) {a*x^b}, 
  function(mCall, LHS, data, ...) {
    xy <- sortedXyData(mCall[["x"]], LHS, data)
    x <-  xy[, "x"]; y <- xy[, "y"]
    
    pseudox = log(x)
    pseudoy = log(y)
    
    ## Linear regression on pseudo-y and pseudo-x
    coefs <- (coef( lm(pseudoy ~ pseudox) ))
    b = coefs[2]
    a = exp(coefs[1])
    start <- c(a, b)
    names(start) <- mCall[c("a", "b")]
    start
  }, 
  parameters=c("a", "b")
)


SSexp <- selfStart(
  function(x,a,b) {a*exp(b/x)}, 
  function(mCall, LHS, data, ...) {
    xy <- sortedXyData(mCall[["x"]], LHS, data)
    x <-  xy[, "x"]; y <- xy[, "y"]
    
    pseudox = 1/x
    pseudoy = log(y)
    
    ## Linear regression on pseudo-y and pseudo-x
    coefs <- (coef( lm(pseudoy ~ pseudox) ))
    b = coefs[2]
    a = exp(coefs[1])
    start <- c(a, b)
    names(start) <- mCall[c("a", "b")]
    start
  }, 
  parameters=c("a", "b")
)


SSexpneg <- selfStart(
  function(x,a,b,c) {a+b*exp(c*x)}, 
  function(mCall, LHS, data, ...) {
    xy <- sortedXyData(mCall[["x"]], LHS, data)
    x <-  xy[, "x"]; y <- xy[, "y"]
    
    a = max(y)
    b = min(y) - a
    c = -.5
    
    start <- c(a, b, c)
    names(start) <- mCall[c("a", "b", "c")]
    start
  }, 
  parameters=c("a", "b", "c")
)


SSexpneg2 <- selfStart(
  function(x,a,b,c) {a*exp(c*x)+b*(1-exp(c*x))}, 
  function(mCall, LHS, data, ...) {
    xy <- sortedXyData(mCall[["x"]], LHS, data)
    x <-  xy[, "x"]; y <- xy[, "y"]
    
    a = min(y)
    b = max(y)
    c = -.5
    
    start <- c(a, b, c)
    names(start) <- mCall[c("a", "b", "c")]
    start
  }, 
  parameters=c("a", "b", "c")
)


SSexpvar <- selfStart(
  function(x,nug,psill,range) {nug+psill*(1-exp(-x/range))}, 
  function(mCall, LHS, data, ...) {
    xy <- sortedXyData(mCall[["x"]], LHS, data)
    x <-  xy[, "x"]; y <- xy[, "y"]
    
    nug = min(y)
    psill = max(y)-nug
    range = x[which.max(y)]*1/3
    
    start <- c(nug, psill, range)
    names(start) <- mCall[c("nug", "psill", "range")]
    start
  }, 
  parameters=c("nug", "psill", "range")
)


SSgaussvar <- selfStart(
  function(x,nug,psill,range) {nug+psill*(1-exp(-(x/range)^2))}, 
  function(mCall, LHS, data, ...) {
    xy <- sortedXyData(mCall[["x"]], LHS, data)
    x <-  xy[, "x"]; y <- xy[, "y"]
    
    nug = min(y)
    psill = max(y)-nug
    range = x[which.max(y)]*1/sqrt(3)
    
    start <- c(nug, psill, range)
    names(start) <- mCall[c("nug", "psill", "range")]
    start
  }, 
  parameters=c("nug", "psill", "range")
)


SSsphvar <- selfStart(
  function(x,nug,psill,range) {ifelse(x<=range,nug+psill*(3/2*(x/range)-1/2*(x/range)^3),
                              nug+psill)}, 
  function(mCall, LHS, data, ...) {
    xy <- sortedXyData(mCall[["x"]], LHS, data)
    x <-  xy[, "x"]; y <- xy[, "y"]
    
    nug = min(y)
    psill = max(y)-nug
    range = x[which.max(y)]*2/3
    
    start <- c(nug, psill, range)
    names(start) <- mCall[c("nug", "psill", "range")]
    start
  }, 
  parameters=c("nug", "psill", "range")
)


SSinvpoly1 <- selfStart(
  function(x,a,b) {x/(a+b*x)}, 
  function(mCall, LHS, data, ...) {
    xy <- sortedXyData(mCall[["x"]], LHS, data)
    x <-  xy[, "x"]; y <- xy[, "y"]
    
    x = x[which(y!=0)]
    y = y[which(y!=0)]
    
    pseudox = 1/x
    pseudoy = 1/y
    
    ## Linear regression on pseudo-y and pseudo-x
    coefs <- abs(coef( lm(pseudoy ~ pseudox) ))
    b <- coefs[1]
    a = coefs[2]
    start <- c(a, b)
    names(start) <- mCall[c("a", "b")]
    start
  }, 
  parameters=c("a", "b")
)


SSinvpoly2 <- selfStart(
  function(x,a,b,c) {x/(a+b*x+c*x^2)}, 
  function(mCall, LHS, data, ...) {
    xy <- sortedXyData(mCall[["x"]], LHS, data)
    x <-  xy[, "x"]; y <- xy[, "y"]
    
    x = x[which(y!=0)]
    y = y[which(y!=0)]
    
    pseudox = 1/x
    pseudoy = 1/y
    
    ## Linear regression on pseudo-y and pseudo-x
    coefs <- abs(coef( lm(pseudoy ~ pseudox + x) ))
    b <- coefs[1]
    a = coefs[2]
    c = coefs[3]
    start <- c(a, b, c)
    names(start) <- mCall[c("a", "b", "c")]
    start
  }, 
  parameters=c("a", "b", "c")
)


SSinvpoly3 <- selfStart(
  function(x,a,b,c,d) {x/(a+b*x+c*x^2+d*x^3)}, 
  function(mCall, LHS, data, ...) {
    xy <- sortedXyData(mCall[["x"]], LHS, data)
    x <-  xy[, "x"]; y <- xy[, "y"]
    
    x = x[which(y!=0)]
    y = y[which(y!=0)]
    
    pseudox = 1/x
    pseudoy = 1/y
    
    ## Linear regression on pseudo-y and pseudo-x
    coefs <- abs(coef( lm(pseudoy ~ pseudox + x + I(x^2)) ))
    b <- coefs[1]
    a = coefs[2]
    c = coefs[3]
    d = coefs[4]
    start <- c(a, b, c, d)
    names(start) <- mCall[c("a", "b", "c", "d")]
    start
  }, 
  parameters=c("a", "b", "c", "d")
)

SSkostmod <- selfStart(
  function(x,alfa,beta,fc) {alfa*x^(beta) + fc}, 
  function(mCall, LHS, data, ...) {
    xy <- sortedXyData(mCall[["x"]], LHS, data)
    x <-  xy[, "x"]; y <- xy[, "y"]
    
    yi = head(y,1)
    yf = tail(y,1)
    
    beta <- -.5
    alfa = ifelse(yf < yi, .5, -.5)
    fc = yf
    
    start <- c(alfa, beta, fc)
    names(start) <- mCall[c("alfa", "beta", "fc")]
    start
  }, 
  parameters=c("alfa", "beta", "fc")
)

SSds <- selfStart(
  function(sign,tau0,phi) {tau0 + sign*tan(phi*pi/180)}, 
  function(mCall, LHS, data, ...) {
    xy <- sortedXyData(mCall[["sign"]], LHS, data)
    x <-  xy[, "x"]; y <- xy[, "y"]

    coefs = coef(lm(y ~ x))
    tau0 = ifelse(coefs[1]<0,0,coefs[1])
    phi = atan(coefs[2])*180/pi

    start <- c(tau0, phi)
    names(start) <- mCall[c("tau0", "phi")]
    start
  }, 
  parameters=c("tau0", "phi")
)

