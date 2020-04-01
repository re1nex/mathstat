meanOfNum<-function(mtx){
  return(c(mean(mtx[1,]),mean(mtx[2,]),mean(mtx[3,]),mean(mtx[4,]),mean(mtx[5,])))
}

varOfNum<-function(mtx){
  return(c(var(mtx[1,]),var(mtx[2,]),var(mtx[3,]),var(mtx[4,]),var(mtx[5,])))
}

f<-function(number) { 
library(LaplacesDemon)
times <- 1000
means <- matrix(0, 5 , times)
meds <- matrix(0, 5 , times)
zR <- matrix(0, 5 , times)
zQ <- matrix(0, 5 , times)
zTr <- matrix(0, 5 , times)

for (i in 1:times){
  dataNorm <- rnorm(number, 0, 1)
  dataPois <- rpois(number, 10)
  dataLapl <- rlaplace(number, 0, 1)
  dataCauch <- rcauchy(number, 0, 1)
  dataUnif <- runif(number, -sqrt(3), sqrt(3))
  
  means[1, i] <- mean(dataNorm)
  means[2, i] <- mean(dataPois)
  means[3, i] <- mean(dataLapl)
  means[4, i] <- mean(dataCauch)
  means[5, i] <- mean(dataUnif)
  
  meds[1, i] <- median(dataNorm)
  meds[2, i] <- median(dataPois)
  meds[3, i] <- median(dataLapl)
  meds[4, i] <- median(dataCauch)
  meds[5, i] <- median(dataUnif)
  
  zR[1, i] <- (min(dataNorm) + max(dataNorm)) / 2
  zR[2, i] <- (min(dataPois) + max(dataPois)) / 2
  zR[3, i] <- (min(dataLapl) + max(dataLapl)) / 2
  zR[4, i] <- (min(dataCauch) + max(dataCauch)) / 2
  zR[5, i] <- (min(dataUnif) + max(dataUnif)) / 2
  
  zQ[1, i] <- (quantile(dataNorm, 0.25) + quantile(dataNorm, 0.75)) / 2
  zQ[2, i] <- (quantile(dataPois, 0.25) + quantile(dataPois, 0.75)) / 2
  zQ[3, i] <- (quantile(dataLapl, 0.25) + quantile(dataLapl, 0.75)) / 2
  zQ[4, i] <- (quantile(dataCauch, 0.25) + quantile(dataCauch, 0.75)) / 2
  zQ[5, i] <- (quantile(dataUnif, 0.25) + quantile(dataUnif, 0.75)) / 2
  
  zTr[1, i] <- mean(dataNorm, trim = 0.25)
  zTr[2, i] <- mean(dataPois, trim = 0.25)
  zTr[3, i] <- mean(dataLapl, trim = 0.25)
  zTr[4, i] <- mean(dataCauch, trim = 0.25)
  zTr[5, i] <- mean(dataUnif, trim = 0.25)
}

meanS <- round(meanOfNum(means), digits=7)
medS <- round(meanOfNum(meds), digits=7)
zRS <- round(c(meanOfNum(zR)), digits=7)
zQS <- round(c(meanOfNum(zQ)), digits=7)
zTrS <- round(c(meanOfNum(zTr)), digits=7)

meanSV <- round(varOfNum(means), digits=7)
medSV <- round(varOfNum(meds), digits=7)
zRSV <- round(c(varOfNum(zR)), digits=7)
zQSV <- round(c(varOfNum(zQ)), digits=7)
zTrSV <- round(c(varOfNum(zTr)), digits=7)


normF <- data.frame(Chars = c("Mean", "Var"), Means = c(meanS[1], meanSV[1]), 
                      Medians = c(medS[1],  medSV[1]), zR = c(zRS[1], zRSV[1]), 
                      zQ = c(zQS[1], zQSV[1]), zTr = c(zTrS[1], zTrSV[1]))

poisF <- data.frame(Chars = c("Mean", "Var"), Means = c(meanS[2], meanSV[2]), 
                       Medians = c(medS[2], medSV[2]), zR = c(zRS[2], zRSV[2]), 
                       zQ = c(zQS[2], zQSV[2]), zTr = c(zTrS[2], zTrSV[2]))

laplF <- data.frame(Chars = c("Mean", "Var"), Means = c(meanS[3], meanSV[3]), 
                        Medians = c(medS[3], medSV[3]), zR = c(zRS[3], zRSV[3]), 
                        zQ = c(zQS[3], zQSV[3]), zTr = c(zTrS[3], zTrSV[3]))

cauchF <- data.frame(Chars = c("Mean", "Var"), Means = c(meanS[4], meanSV[4]),
                        Medians = c(medS[4], medSV[4]), zR = c(zRS[4], zRSV[4]),
                        zQ = c(zQS[4], zQSV[4]), zTr = c(zTrS[4], zTrSV[4]))

unifF <- data.frame(Chars = c("Mean", "Var"), Means = c(meanS[5], meanSV[5]), 
                     Medians = c(medS[5], medSV[5]), zR = c(zRS[5], zRSV[5]), 
                     zQ = c(zQS[5], zQSV[5]), zTr = c(zTrS[5], zTrSV[5]))

print(number)
print("Normal")
print(normF)
print("Poisson")
print(poisF)
print("Laplace")
print(laplF)
print("Cauchy")
print(cauchF)
print("Unif")
print(unifF)
}

f(10)
f(100)
f(1000)
