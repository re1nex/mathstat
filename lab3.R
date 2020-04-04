library(LaplacesDemon)

nums <- c(20, 100)
times <- 1000

boxplot(rnorm(nums[1], 0, 1), rnorm(nums[2], 0, 1), horizontal = TRUE, outline = TRUE, names =nums)
cauchy1 <- rcauchy(nums[1], 0, 1) 
cauchy2 <- rcauchy(nums[2], 0, 1)
boxplot(cauchy1, cauchy2, horizontal = TRUE, outline = TRUE, names =nums)
boxplot(cauchy1, cauchy2, horizontal = TRUE, outline = FALSE, names =nums)
boxplot(rlaplace(nums[1], 0, 1/sqrt(2)), rlaplace(nums[2], 0, 1/sqrt(2)), horizontal = TRUE, outline = TRUE, names =nums)
boxplot(rpois(nums[1], 10), rpois(nums[2], 10), horizontal = TRUE, outline = TRUE, names =nums)
boxplot(runif(nums[1], -sqrt(3), sqrt(3)), runif(nums[2], -sqrt(3), sqrt(3)), horizontal = TRUE, outline = TRUE, names =nums)



for (num in nums) {
  overshoot <- matrix(nrow = 5, ncol = times)
  
  for (i in 1:times) {
    n <- rnorm(num, 0, 1)
    p <- rpois(num, 10)
    l <- rlaplace(num, 0, 1)
    c <- rcauchy(num, 0, 1)
    u <- runif(num, -sqrt(3), sqrt(3))
    
    quantileN=quantile(n)
    quantileP=quantile(p)
    quantileL=quantile(l)
    quantileC=quantile(c)
    quantileU=quantile(u)
    
    LQN <- quantileN[2]
    LQP <- quantileP[2]
    LQL <- quantileL[2]
    LQC <- quantileC[2]
    LQU <- quantileU[2]
    
    UQN <- quantileN[4]
    UQP <- quantileP[4]
    UQL <- quantileL[4]
    UQC <- quantileC[4]
    UQU <- quantileU[4]
    
    LBN <- LQN - 1.5 * (UQN - LQN)
    LBP <- LQP - 1.5 * (UQP - LQP)
    LBL <- LQL - 1.5 * (UQL - LQL)
    LBC <- LQC - 1.5 * (UQC - LQC)
    LBU <- LQU - 1.5 * (UQU - LQU)
    
    UBN <- UQN + 1.5 * (UQN - LQN)
    UBP <- UQP + 1.5 * (UQP - LQP)
    UBL <- UQL + 1.5 * (UQL - LQL)
    UBC <- UQC + 1.5 * (UQC - LQC)
    UBU <- UQU + 1.5 * (UQU - LQU)

    overshoot[1, i] <- length(n[n<LBN|n>UBN]) / num
    overshoot[2, i] <- length(p[p<LBP|p>UBP]) / num
    overshoot[3, i] <- length(l[l<LBL|l>UBL]) / num
    overshoot[4, i] <- length(c[c<LBC|c>UBC]) / num
    overshoot[5, i] <- length(u[u<LBU|u>UBU]) / num
  }

  rate <- c(mean(overshoot[1,]),
            mean(overshoot[2,]),
            mean(overshoot[3,]),
            mean(overshoot[4,]),
            mean(overshoot[5,]))
  dataFrame <- data.frame(rate, row.names = c("Norm", "Pois", "Lapl", "Cauchy", "Unif"))
  print(num)
  print(dataFrame)
}

LQ <- qnorm(0.25, 0, 1)
UQ <- qnorm(0.75, 0, 1)
X1 <- LQ - 1.5 * (UQ - LQ)
X2 <- LQ + 1.5 * (UQ - LQ)
Gaussovershoot <- pnorm(X1, 0, 1) + (1 - pnorm(X2, 0, 1))


LQ <- qcauchy(0.25, 0, 1)
UQ <- qcauchy(0.75, 0, 1)
X1 <- LQ - 1.5 * (UQ - LQ)
X2 <- LQ + 1.5 * (UQ - LQ)
Cauchyovershoot <- pcauchy(X1, 0, 1) + (1 - pcauchy(X2, 0, 1))


LQ <- qlaplace(0.25, 0, 1/sqrt(2))
UQ <- qlaplace(0.75, 0, 1/sqrt(2))
X1 <- LQ - 1.5 * (UQ - LQ)
X2 <- LQ + 1.5 * (UQ - LQ)
Laplaceovershoot <- plaplace(X1, 0, 1/sqrt(2)) + (1 - plaplace(X2, 0, 1/sqrt(2)))


LQ <- qpois(0.25, 10)
UQ <- qpois(0.75, 10)
X1 <- LQ - 1.5 * (UQ - LQ)
X2 <- LQ + 1.5 * (UQ - LQ)
Poissonovershoot <- ppois(X1, 10) - dpois(X1, 10) + (1 - ppois(X2, 10))


LQ <- qunif(0.25, -sqrt(3), sqrt(3))
UQ <- qunif(0.75, -sqrt(3), sqrt(3))
X1 <- LQ - 1.5 * (UQ - LQ)
X2 <- LQ + 1.5 * (UQ - LQ)
Unifovershoot <- punif(X1, -sqrt(3), sqrt(3)) + (1 - punif(X2, -sqrt(3), sqrt(3)))