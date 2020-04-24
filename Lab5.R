
library(stringr)
library(car)
library(MASS)

ro <- c(0, 0.5, 0.9)
means <- c(0, 0)
size <- c(20, 60, 100)
num <- 1000
coeff <- c("select_quad", "pearson", "spearman")
corr <- array(data = numeric(27000), dim = c(3, 3, 3, 1000))
covar_m <- array(data = c( matrix(c(NaN, NaN, NaN, NaN), 2, 2),  matrix(c(NaN, NaN, NaN, NaN), 2, 2),  matrix(c(NaN, NaN, NaN, NaN), 2, 2)), dim = c(2, 2, length(ro))) 
for (i in 1:length(ro)) {
  covar_m[,,i] <- matrix(c(1, ro[i], ro[i], 1), 2, 2)
}


corr_f <- function(c_name, vec) {
  if (c_name == "select_quad") {
    n1 <- length(vec[vec[,1] >= 0 & vec[,2] >= 0])
    n2 <- length(vec[vec[,1] < 0 & vec[,2] >= 0])
    n3 <- length(vec[vec[,1] < 0 & vec[,2] < 0])
    n4 <- length(vec[vec[,1] >= 0 & vec[,2] < 0])
    
    sum <- length(vec)
    return (((n1 + n3) - (n2 + n4))/sum)
    
  } else {
    mtx <- cor(vec, method = c_name)
    return (mtx[1, 2])
  }
}


for (ro_num in 1:length(ro)) {
  for (i in 1:length(coeff)) {
    for (j in 1:length(size)) {
      for (k in 1:num) {
        vec <- mvrnorm(n = size[j], mu = means, covar_m[,,ro_num])
        corr[i, ro_num, j, k] <- corr_f(c_name = coeff[i], vec = vec)
      }
    }
  }
}





func_df <- function(experimentsData) {
  mean_z <- numeric(length(coeff))
  sqr_mean <- numeric(length(coeff))
  var_ro <- numeric(length(coeff))
  for (i in 1:length(coeff)) {

    mean_z[i] <- mean(experimentsData[i,])

    sqr_mean[i] <- mean(experimentsData[i,] ^ 2)

    var_ro[i] <- var(experimentsData[i,])
  }
  
  row_names <- coeff
  df <- data.frame(Means = mean_z, SqrMeans = sqr_mean, Variances = var_ro, row.names = row_names)  
  return (df)
}


for (i in 1:length(size)) {
  for (j in 1:length(ro)) {
    print(size[i])
    print(ro[j])
    print(func_df(corr[,j,i,]))
  }
}




corr <- array(data = numeric(9000), dim = c(3, 3, 1000))



mix_f <- function(size, means) {
  return (0.9 * mvrnorm(size, means, covar_m[,,3]) + 0.1 * mvrnorm(size, means,matrix(c(1, -0.9, -0.9, 1), 2, 2)))
}




for (i in 1 : length(coeff)) {
  for (j in 1 : length(size)) {
    for (k in 1 : num) {
      vec <- mix_f(size[j], means)
      corr[i,j,k] <- corr_f(coeff[i], vec)
    }
  }
}



for (i in 1 : length(size)) {
  print(size[i])
  print(func_df(corr[i,,]))
}




func_ellipse <- function(size, cov_mtx,ro) {
  vec <- mvrnorm(size, c(0, 0), cov_mtx)
  name<- str_c("n=", toString(size),"  ro=", toString(ro))
  dataEllipse(vec, levels = 0.95, center.pch =c(0,0),xlim=1.5*range(vec[,1]),ylim=1.5*range(vec[,2]),main=name)
}


func_ellipse(size[1], covar_m[,,1],0)
func_ellipse(size[1], covar_m[,,2],0.5)
func_ellipse(size[1], covar_m[,,3],0.9)


func_ellipse(size[2], covar_m[,,1],0)
func_ellipse(size[2], covar_m[,,2],0.5)
func_ellipse(size[2], covar_m[,,3],0.9)

func_ellipse(size[3], covar_m[,,1],0)
func_ellipse(size[3], covar_m[,,2],0.5)
func_ellipse(size[3], covar_m[,,3],0.9)


