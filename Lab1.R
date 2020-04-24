library(LaplacesDemon)
size<-10
bord<-4
n <- rnorm(size,0,1)
hist(n,xlim=c(-bord, bord), freq = FALSE, col = "lightblue",
     xlab = "n=10",
     ylab = "Плотность вероятности",
     main = "Normal") 
curve(dnorm(x, 0,1), add=TRUE,-bord,bord)

bord<-10
c <- rcauchy(size,0,1)
hist(c,xlim=c(-bord, bord),ylim=c(0,bord/15), freq = FALSE, col = "lightblue",
     xlab = "n=10",
     ylab = "Плотность вероятности",
     main = "Cauchy",
     breaks=20) 
curve(dcauchy(x, 0,1), add=TRUE,-bord,bord)

l <- rlaplace(size,0,1/sqrt(2))
hist(l,xlim=c(-bord, bord),ylim=c(0,bord/10), freq = FALSE, col = "lightblue",
     xlab = "n=10",
     ylab = "Плотность вероятности",
     main = "Laplace") 
curve(dlaplace(x, 0,1/sqrt(2)), add=TRUE,-bord,bord)

p<-rpois(size,10)
x=0:(2*bord)
hist(p,xlim=c(0, 2*bord), freq = FALSE, col = "lightblue",
     xlab = "n=10",
     ylab = "Плотность вероятности",
     main = "Poisson") 
lines(x,dpois(x, 10))

bord<-4
r<-runif(size,-sqrt(3),sqrt(3))
hist(r,xlim=c(-bord, bord), freq = FALSE, col = "lightblue",
     xlab = "n=10",
     ylab = "Плотность вероятности",
     main = "Uniform") 
curve(dunif(x, -sqrt(3),sqrt(3)), add=TRUE,-bord,bord)








size<-50
bord<-4
n <- rnorm(size,0,1)
hist(n,xlim=c(-bord, bord), freq = FALSE, col = "lightblue",breaks=50,
     xlab = "n=50",
     ylab = "Плотность вероятности",
     main = "Normal") 
curve(dnorm(x, 0,1), add=TRUE,-bord,bord)

bord<-30
c <- rcauchy(size,0,1)
hist(c,xlim=c(-bord, bord),ylim=c(0,bord/100), freq = FALSE, col = "lightblue",
     xlab = "n=50",
     ylab = "Плотность вероятности",
     main = "Cauchy",
     breaks=50) 
curve(dcauchy(x, 0,1), add=TRUE,-bord,bord)

l <- rlaplace(size,0,1/sqrt(2))
hist(l,xlim=c(-bord, bord),ylim=c(0,bord/3), freq = FALSE, col = "lightblue",breaks=50,
     xlab = "n=50",
     ylab = "Плотность вероятности",
     main = "Laplace") 

curve(dlaplace(x, 0,1/sqrt(2)), add=TRUE,-bord,bord)

p<-rpois(size,10)
x=0:(20)
hist(p,xlim=c(0, 5*bord), freq = FALSE, col = "lightblue",breaks=50,
     xlab = "n=50",
     ylab = "Плотность вероятности",
     main = "Poisson") 
lines(x,dpois(x, 10))

bord<-2
r<-runif(size,-sqrt(3),sqrt(3))
hist(r,xlim=c(-bord, bord), freq = FALSE, col = "lightblue",breaks=50,
     xlab = "n=50",
     ylab = "Плотность вероятности",
     main = "Uniform") 
curve(dunif(x, -sqrt(3),sqrt(3)), add=TRUE,-bord,bord)







size<-1000
bord<-4
n <- rnorm(size,0,1)
hist(n,xlim=c(-bord, bord), freq = FALSE, col = "lightblue",breaks=50,
     xlab = "n=1000",
     ylab = "Плотность вероятности",
     main = "Normal") 
curve(dnorm(x, 0,1), add=TRUE,-bord,bord)

bord<-300
c <- rcauchy(size,0,1)
hist(c,xlim=c(-bord, bord),ylim=c(0,bord/1300), freq = FALSE, col = "lightblue",
     xlab = "n=1000",
     ylab = "Плотность вероятности",
     main = "Cauchy",
     breaks=100) 
curve(dcauchy(x, 0,1), add=TRUE,-bord,bord)

bord<-7
l <- rlaplace(size,0,1/sqrt(2))
hist(l,xlim=c(-bord, bord),ylim=c(0,bord/7), freq = FALSE, col = "lightblue",breaks=50,
     xlab = "n=1000",
     ylab = "Плотность вероятности",
     main = "Laplace") 

curve(dlaplace(x, 0,1/sqrt(2)), add=TRUE,-6,6)

p<-rpois(size,10)
x=0:(30)
hist(p,xlim=c(0, 30), freq = FALSE, col = "lightblue",breaks=20,
     xlab = "n=1000",
     ylab = "Плотность вероятности",
     main = "Poisson") 
lines(x,dpois(x, 10))

bord<-2
r<-runif(size,-sqrt(3),sqrt(3))
hist(r,xlim=c(-bord, bord), freq = FALSE, col = "lightblue",breaks=50,
     xlab = "n=1000",
     ylab = "Плотность вероятности",
     main = "Uniform") 
curve(dunif(x, -sqrt(3),sqrt(3)), add=TRUE,-bord,bord)
