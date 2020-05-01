n <- 100
data <- rnorm(n, 0, 1)

print(mean(data))
print(var(data))

inval <- hist(data,breaks=6)
breaks <- inval$breaks[0:-1]
p_i <- pnorm(inval$breaks)

pi <- c()
for (i in 1:(length(p_i)-1)) {
  pi[i] <- p_i[i + 1] - p_i[i]
}

df <- data.frame(interv = breaks,  n = inval$counts,p = pi, npi = n * pi, ni_np = inval$counts - n * pi, frac = (inval$counts - n * pi) ** 2 / (n * pi))
print(sum(df$p))
print(sum(df$n))
print(sum(df$np))
print(sum(df$ni_np))
print(sum(df$frac))

