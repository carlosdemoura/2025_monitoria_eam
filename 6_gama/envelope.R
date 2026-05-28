library(tidyverse)
dados =
  read.csv("dados/quantal.dat", sep="",stringsAsFactors = TRUE) |>
  as_tibble()

fit = glm(Response ~ ., family=binomial(), data = dados)
beta_hat = coef(fit)
X = model.matrix(fit)
eta_hat = X%*%beta_hat
mu_hat = exp(eta_hat) / (exp(eta_hat) + 1)
m = 200

residuos = matrix(nrow = nrow(dados), ncol = m)
for (i in 1:m) {
  dados_i =
    dados |>
    select(-Response) |>
    mutate(
      y_gerado = rbinom(nrow(dados), 1, mu_hat) |> factor()
    ) 
  fit_i = glm(y_gerado ~ ., family=binomial(), data = dados_i)
  residuos[,i] = residuals(fit_i)
  
}
n = nrow(dados)

e1 <- numeric(n)		# limites inferiores
e2 <- numeric(n)		# limites superiores
for(i in 1:n){
  e0    <- sort(residuos[i,])	# ordena cada percentil
  e1[i] <- e0[floor(m*.05)]		# Percentil 5
  e2[i] <- e0[floor(m*.95)]		# Percentil 95
}

med   <- apply(residuos,1,mean)	# Define as medias para cada percentil
td <- residuals(fit)
faixa <- range(td,e1,e2)	# Define os minimos e maximos

par(pty="s")			# square plotting region
qqnorm(td,xlab="Percentis da N(0,1)",ylab="Componente do Desvio Padronizado",ylim=faixa)
par(new=T)
qqnorm(e1,xlab="",ylab="",type="l",ylim=faixa,lty=1)		# Limite Inferior
par(new=T)
qqnorm(e2,xlab="",ylab="",type="l",ylim=faixa,lty=1)		# Limite Superior
par(new=T)
qqnorm(med,xlab="",ylab="",type="l",ylim=faixa,lty=2)	# Linha Central