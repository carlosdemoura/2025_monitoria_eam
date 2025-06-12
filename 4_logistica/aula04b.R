###############################################
## Scripts Elementos de Aprendizado de Máquina
###############################################

###############################################
## Analise de Residuos Regressao Logistica
###############################################
rm(list=ls())

#seleciona o arquivo "quantal.dat"
dados <- read.csv("quantal.dat", sep="",stringsAsFactors = TRUE)
names(dados)

## Analise Exploratoria
plot(Response ~ Subject, data=dados)
plot(Response ~ Subject, data=dados, col=c("light blue","orange"))
plot(Volume ~ Response, data=dados)
plot(Rate ~ Response, data=dados)

## Ajuste do Modelo Logistico (Binomial)
## Binomial Link: logit
modelo <- glm(Response ~ Volume + Rate, 
              family = binomial(link = "logit"), data = dados)
summary(modelo)
1 - pchisq(modelo$deviance,modelo$df.residual) ## p-value "Deviance"

confint(modelo)
exp(coefficients(modelo))
exp(confint(modelo))

## Analise dos residuos
dados$ajuste.linear   <- predict(modelo, type="link")
dados$ajuste   <- predict(modelo, type="response")
dados$Deviance <- residuals(modelo, type="deviance")
dados$Pearson  <- residuals(modelo, type="pearson")

par(mfrow=c(2,2))
plot(modelo, pch=19, col="blue")

par(mfrow=c(1,1))
plot(Deviance ~ ajuste.linear, data=dados, col="blue", pch=19)

####################################################################
## EXERCICIO
## Usando a aula03b.R tenta implementar a análise de resíduos por 
## envelope para a regressão binária ajustada.
####################################################################


#################################################################
## Envelope de Probabilidade - Modelo Bernoulli (logístico)

fit.model <- modelo

X <- model.matrix(fit.model)
n <- nrow(X)
p <- ncol(X)
w <- fit.model$weights
W <- diag(w)
H <- solve(t(X)%*%W%*%X)
H <- sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
h <- diag(H)

# Desvio Residual - "Ajustado"
td <- resid(fit.model,type="deviance")/sqrt(1-h)

# Simulacao do envelope
m <- 1000
e <- matrix(0,n,m)	# Armazena as simulacoes
for(i in 1:m){
  # Gera novas saidas a partir dos valores ajustados (originais)
  nresp <- rbinom(n, 1, fitted(fit.model))
  fit   <- glm(nresp ~ X, binomial)
  w     <- fit$weights
  W     <- diag(w)
  H     <- solve(t(X)%*%W%*%X)
  H     <- sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
  h     <- diag(H)
  e[,i] <- sort( resid(fit,type="deviance")/sqrt(1-h) )
}

# Construcao do envelope com 95% de confianca
e1 <- numeric(n)		# limites inferiores
e2 <- numeric(n)		# limites superiores
for(i in 1:n){
  e0    <- sort(e[i,])	# ordena cada percentil
  e1[i] <- e0[floor(m*.005)]		# Percentil 5
  e2[i] <- e0[floor(m*.995)]		# Percentil 95
}

med   <- apply(e,1,mean)	# Define as medias para cada percentil
faixa <- range(td,e1,e2)	# Define os minimos e maximos
par(pty="s")			# square plotting region
qqnorm(td,xlab="Percentis da N(0,1)",ylab="Componente do Desvio Padronizado",ylim=faixa)
par(new=T)									# Permite inserir novas linhas
qqnorm(e1,xlab="",ylab="",type="l",ylim=faixa,lty=1)		# Limite Inferior
par(new=T)
qqnorm(e2,xlab="",ylab="",type="l",ylim=faixa,lty=1)		# Limite Superior
par(new=T)
qqnorm(med,xlab="",ylab="",type="l",ylim=faixa,lty=2)	# Linha Central

lines(x=seq(-2,2,.01), y=seq(-2,2,.01))
