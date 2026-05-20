n = 100
X = cbind(rep(1,n), seq(-1,1,length.out=n))
beta = c(.3, 2)

eta = X %*% beta
mu = exp(eta)/(exp(eta)+1)
Y = rbinom(n, 1, mu)

plot(Y~X[,2])

