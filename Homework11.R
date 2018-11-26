# Q1
rbinomial <- function(n, p){
  x <- runif(n, min = 0, max = 1)
  return(sum(x < p))
}
rbinomial(25, 0.4)

# Q2
microbenchmark::microbenchmark(replicate(1000, {
  rbinomial(25, 0.4)
}))
microbenchmark::microbenchmark(rbinom(1000, 25, 0.4))


# Q3
set.seed(123)
xi <- runif(50, min = 20, max = 40)
beta0 <- 15
beta1 <- 0.4
error <- rnorm(50, 0, 3)
y_est <- beta0 + beta1 * xi + error
y_true <- beta0 + beta1 * xi
dataq3 <- data.frame(y_est, error)

ggplot(data = dataq3) +
  geom_point(aes(x = y_est, y = error)) +
  labs(x = "Fitted Value", y = "Residuals")


# Q4
q4 <- function(n){
  u1 = runif(n)
  u2 = runif(n)
  R = sqrt(-2 * log(u1))
  theta = 2 * pi * u2
  X = R * cos(theta)
  Y = R * sin(theta)
return(data.frame(X, Y))
}

plotdata <- q4(1000)

ggplot(data = plotdata)+
  geom_histogram(aes(x = X, y = ..density..), bins = 20, col = 1, fill = NA)+
  stat_density(aes(x = rnorm(1000, 0,1)), col = 3, fill = NA)+
  theme_bw()


ggplot(data = plotdata)+
  geom_histogram(aes(x = Y, y = ..density..), bins = 20, col = 2, fill = NA)+
  stat_density(aes(x = rnorm(1000, 0,1)), col = 3, fill = NA)+
  theme_bw()
