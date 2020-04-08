library(LaplacesDemon)

approxn <- function(size, randf, p, step = 0.001, p1 = 0, p2 = 1) {
  x <- randf(size, p1, p2)
  n <- length(x)
  x <- sort(x)
  vals <- unique(x)
  aproxVal <- approxfun(vals, cumsum(tabulate(match(x, vals)))/n, method = "constant", yleft = 0, yright = 1, f = 0, ties = "ordered")
  interval <- seq(-4, 4, step)
  plot(interval, aproxVal(interval), type = "l", col = "red", xlab = "x", ylab = "F(x)")
  curve(p(x, p1, p2), add = TRUE)
}

approxn(20, rnorm, pnorm)
approxn(60, rnorm, pnorm)
approxn(100, rnorm, pnorm)

approxn(20, rcauchy, pcauchy)
approxn(60, rcauchy, pcauchy)
approxn(100, rcauchy, pcauchy)

approxn(20, rlaplace, plaplace, p1 = 0, p2 = 1/sqrt(2))
approxn(60, rlaplace, plaplace, p1 = 0, p2 = 1/sqrt(2))
approxn(100, rlaplace, plaplace, p1 = 0, p2 = 1/sqrt(2))

approxn(20, runif, punif, p1 = -sqrt(3), p2 = sqrt(3))
approxn(60, runif, punif, p1 = -sqrt(3), p2 = sqrt(3))
approxn(100, runif, punif, p1 = -sqrt(3), p2 = sqrt(3))

aproximate_pois<- function(size) {
  x <- rpois(size, 10)
  n <- length(x)
  x <- sort(x); 
  vals <- unique(x)
  aproxVal <- approxfun(vals, cumsum(tabulate(match(x, vals)))/n, method = "constant", yleft = 0, yright = 1, f = 0,ties = "ordered")
  interval <- seq(6, 14, 0.001)
  plot(interval, aproxVal(interval), type = "l", col = "red", xlab = "x", ylab = "F(x)")
  curve(ppois(x, 10), add = TRUE)
}
aproximate_pois(20)
aproximate_pois(60)
aproximate_pois(100)




kernel <- function(u) { 
  return (exp(-u * u / 2) / sqrt(2 * pi))
}

den <- function(u, x, hn) {
  s <- 0
  for (i in x) {
    s <- s + kernel((u - i) / hn)
  }
  return (s /(length(x)*hn))
}

es <- function(randf, dens, f1, f2, size, hd) {
  interval <- seq(-4, 4, 0.001)
  x <- randf(size, f1, f2)
  h <- 1.06*length(x)^(-1/5)*sqrt(var(x))
  hn <- h / hd
  y <- den(interval, x, hn)
  plot(c(interval, NaN), c(y, dens(0, f1, f2)), type = "l", col = "blue", xlab = "x", ylab = "f(x)")
  curve(dens(x, f1, f2), add = TRUE)
}

es_pois<-function(size,hd){
  interval <- seq(6, 14, 1)
  x <- rpois(size, 10)
  h <- 1.06*length(x)^(-1/5)*sqrt(var(x))
  hn <- h / hd
  y <- den(interval, x, hn)
  plot(c(interval, NaN), c(y, dpois(10, 10)), type = "l", col = "blue", xlab = "x", ylab = "f(x)")
  lin <- c()
  for (j in 1:length(interval)) {
    lin[j] <- dpois(interval[j], 10)
  }
  lines(interval, lin)
}

es(rnorm, dnorm,  0, 1, 20,hd=2)
es(rnorm, dnorm,  0, 1, 60,hd=2)
es(rnorm, dnorm,  0, 1, 100,hd=2)

es(rnorm, dnorm,  0, 1, 20,hd=1)
es(rnorm, dnorm,  0, 1, 60,hd=1)
es(rnorm, dnorm,  0, 1, 100,hd=1)

es(rnorm, dnorm, 0, 1, 20,hd=0.5)
es(rnorm, dnorm, 0, 1, 60,hd=0.5)
es(rnorm, dnorm, 0, 1, 100,hd=0.5)


es(rcauchy, dcauchy, 0, 1, 20,hd= 2)
es(rcauchy, dcauchy, 0, 1, 60, hd=2)
es(rcauchy, dcauchy, 0, 1, 100,hd= 2)

es(rcauchy, dcauchy, 0, 1, 20, hd=1)
es(rcauchy, dcauchy, 0, 1, 60,hd= 1)
es(rcauchy, dcauchy, 0, 1, 100,hd= 1)

es(rcauchy, dcauchy, 0, 1, 20,hd= 0.5)
es(rcauchy, dcauchy, 0, 1, 60,hd= 0.5)
es(rcauchy, dcauchy, 0, 1, 100,hd= 0.5)


es(rlaplace, dlaplace, 0, 1/sqrt(2), 20, hd=2)
es(rlaplace, dlaplace, 0, 1/sqrt(2), 60, hd=2)
es(rlaplace, dlaplace, 0, 1/sqrt(2), 100, hd=2)

es(rlaplace, dlaplace, 0, 1/sqrt(2), 20, hd=1)
es(rlaplace, dlaplace, 0, 1/sqrt(2), 60, hd=1)
es(rlaplace, dlaplace, 0, 1/sqrt(2), 100, hd=1)

es(rlaplace, dlaplace, 0, 1/sqrt(2), 20, hd=0.5)
es(rlaplace, dlaplace, 0, 1/sqrt(2), 60, hd=0.5)
es(rlaplace, dlaplace, 0, 1/sqrt(2), 100, hd=0.5)


es(runif, dunif, -sqrt(3), sqrt(3), 20, hd=2)
es(runif, dunif, -sqrt(3), sqrt(3), 60, hd=2)
es(runif, dunif, -sqrt(3), sqrt(3), 100, hd=2)

es(runif, dunif, -sqrt(3), sqrt(3), 20, hd=1)
es(runif, dunif, -sqrt(3), sqrt(3), 60, hd=1)
es(runif, dunif, -sqrt(3), sqrt(3), 100, hd=1)

es(runif, dunif, -sqrt(3), sqrt(3), 20, hd=0.5)
es(runif, dunif, -sqrt(3), sqrt(3), 60, hd=0.5)
es(runif, dunif, -sqrt(3), sqrt(3), 100, hd=0.5)


es_pois(20,2)
es_pois(100,2)
es_pois(60,2)

es_pois(20,1)
es_pois(60,1)
es_pois(100,1)

es_pois(20,0.5)
es_pois(60,0.5)
es_pois(100,0.5)