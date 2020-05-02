#includes 
library(stringr)
library(LaplacesDemon)
library(moments)


qn <- 1 - 0.05 / 2

n <- 20
data <- rnorm(n)
st <- qst(qn, nu = n - 1)

mean_ <- mean(data)
var_ <- var(data)

lb <-mean_ - (var_ * st) / sqrt(n - 1)
rb <-mean_ + (var_ * st) / sqrt(n - 1)

chisql <- qchisq(qn, n - 1)
chisqr <- qchisq(alpha / 2, n - 1)

lbv <- var_ * sqrt(n) / sqrt(chisql)
rbv <- var_ * sqrt(n) / sqrt(chisqr)

print(n)
print(str_c(toString(lb)," < m < ",toString(rb)))
print(str_c(toString(lbv)," < sigma < ",toString(rbv)))

n_quant <- qnorm(qn)

lb <-mean_ - (var_ * n_quant) / sqrt(n)
rb <-mean_ + (var_ * n_quant) / sqrt(n)

kurtosis_ <- kurtosis(data) 

lbv <- var_*(1 - 0.5*n_quant*sqrt(kurtosis_ + 2)/sqrt(n))
rbv <- var_*(1 + 0.5*n_quant*sqrt(kurtosis_ + 2)/sqrt(n))

print(n)
print(str_c(toString(lb)," < m < ",toString(rb)))
print(str_c(toString(lbv)," < sigma < ",toString(rbv)))




n <- 100


data <- rnorm(n)

st <- qst(qn, nu = n - 1)

mean_ <- mean(data)
var_ <- var(data)

lb <-mean_ - (var_ * st) / sqrt(n - 1)
rb <-mean_ + (var_ * st) / sqrt(n - 1)

chisql <- qchisq(qn, n - 1)
chisqr <- qchisq(alpha / 2, n - 1)

lbv <- var_ * sqrt(n) / sqrt(chisql)
rbv <- var_ * sqrt(n) / sqrt(chisqr)

print(n)
print(str_c(toString(lb)," < m < ",toString(rb)))
print(str_c(toString(lbv)," < sigma < ",toString(rbv)))

n_quant <- qnorm(qn)

lb <-mean_ - (var_ * n_quant) / sqrt(n)
rb <-mean_ + (var_ * n_quant) / sqrt(n)

kurtosis_ <- kurtosis(data) 

lbv <- var_*(1 - 0.5*n_quant*sqrt(kurtosis_ + 2)/sqrt(n))
rbv <- var_*(1 + 0.5*n_quant*sqrt(kurtosis_ + 2)/sqrt(n))

print(n)
print(str_c(toString(lb)," < m < ",toString(rb)))
print(str_c(toString(lbv)," < sigma < ",toString(rbv)))