library(ggplot2)
library(L1pack)


linear_reg <- function(x) {
  return (2 + 2*x + rnorm(length(x)))
}


outlier_func <- function(y_vec) {
  y_vec[1] <- y_vec[1] + 10
  y_vec[length(y_vec)] <- y_vec[length(y_vec)] - 10
  return(y_vec)
}

create_linear_reg <- function(size, from, to) {
  x <- seq(from, to, length.out = size)
  y <- linear_reg(x)
  return (data.frame(x = x, y = y))
}

create_linear_reg_outliers <- function(size, from, to) {
  x <- seq(from, to, length.out = size)
  y <- linear_reg(x)
  y <- outlier_func(y)
  return (data.frame(x = x, y = y))
}


data_lin_reg <- create_linear_reg(size = 20, from = -1.8, to = 2)
data_lin_reg_out <- create_linear_reg_outliers(size = 20, from = -1.8, to = 2)

data_lm <- lm(y ~ x, data_lin_reg)
data_lm_out <- lm(y ~ x, data_lin_reg_out)

data_lad <- lad(y ~ x, data_lin_reg)
data_lad_out <- lad(y ~ x, data_lin_reg_out)

print(data_lm$coefficients)
print(data_lad$coefficients)

print(data_lm_out$coefficients)
print(data_lad_out$coefficients)


ggplot(data_lm, aes(x, y))+
  geom_point() +
  stat_smooth(method = "lm",col="black",formula = y ~ x,se=FALSE)+
  stat_function(fun = function (x) (data_lad_out$coefficients[1] + data_lad_out$coefficients[2] * x), col = "blue")+
  stat_function(fun = function (x) (2 + 2 * x), col = "red")


ggplot(data_lm_out, aes(x, y)) +
  geom_point() +
  stat_smooth(method = "lm",col="black",formula = y ~ x,se=FALSE)+
  stat_function(fun = function (x) (data_lad_out$coefficients[1] +  data_lad_out$coefficients[2] * x), col = "blue")+
  stat_function(fun = function (x) (2 + 2 * x), col = "red")