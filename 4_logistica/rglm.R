n = 100
X = cbind(rep(1,n), seq(-1,1,length.out=n))
beta = c(.3, 2)

eta = X %*% beta
mu = pnorm(eta)
Y = rbinom(n, 1, mu)

plot(Y~X[,2])

mod = glm(Y~X[,2], family = binomial(link = "logit"))

beta = c(.3, 1, 1.3, -2)

dados = tibble(
  x1 = runif(n),
  x2 = runif(n),
  x3 = runif(n),
  eta = c(cbind(rep(1,n), x1, x2, x3) %*% beta),
  y   = rbinom(n, 1, pnorm(eta))
) 

mod = glm(y ~ . -eta, family = binomial("probit"), data = dados)
summary(mod)
