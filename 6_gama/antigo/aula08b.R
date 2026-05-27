###############################################
## Scripts Elementos de Aprendizado de Máquina
###############################################

###############################################
## EXEMPLODADOS OXIDATION
###############################################
rm(list=ls(all=TRUE))
require(MASS)

#seleciona o arquivo 
dt <- read.table("oxidation_gamma.dat", header=TRUE, sep="")
hist(dt$Rate, prob=TRUE, main="Rate density", xlab = "Rate", ylab="density")

# Analise exploratoria das variaveis (Graficos de Dispersao) 
par(mfrow = c(2,3))
plot(Rate ~ Conc.O , data=dt, xlab="Conc.O", ylab="Rate", col="blue", pch=19); grid()
plot(Rate ~ Conc.B , data=dt, xlab="Conc.B", ylab="Rate", col="red", pch=19); grid()
plot(Rate ~ Temp, data=dt, xlab="Temp", ylab="Rate", pch=19); grid()
plot(Rate ~ O.per.B, data=dt, xlab="Temp", ylab="Rate", pch=19); grid()
plot(Rate ~ O.per.B, data=dt, xlab="Temp", ylab="Rate", pch=19, log="xy"); grid()

# Monta a matriz de correlacao
cor(dt)

# Testando Modelo Normal
fit.model <- lm(Rate ~ Temp + Conc.O + Conc.B + O.per.B, data = dt) 
summary(fit.model)
plot(fit.model)
shapiro.test(fit.model$res)

## Modelo Completo
fit.model <- glm(Rate ~ Conc.O + Conc.B + Temp ,
                 family=Gamma(link=log), data = dt)
summary(fit.model)
plot(fit.model)

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
## Modelo Completo e Final - Testando a possibilidade de uma Poisson
fit.model <- glm(Rate ~ Conc.O + Conc.B + Temp ,family=poisson(link=log), 
                 data = dt)
summary(fit.model)
plot(fit.model)
hist(fit.model$res)
shapiro.test(fit.model$res)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Uso do StepAIC
model.AIC <- stepAIC(fit.model, trace=FALSE)
summary(model.AIC)

# Qualidade do Modelo
1-pchisq(fit.model$deviance,fit.model$df.residual) ## p-value "Deviance"

# Analise da Funcao de Ligacao
fit.m2 <- glm(Rate ~ Conc.O + Conc.B + Temp ,family=Gamma(link=identity), 
              data = dt)

fit.m3 <- glm(Rate ~ Conc.O + Conc.B + Temp ,family=Gamma(link=inverse), 
               data = dt)

cat("\n", c(AIC(fit.model), AIC(fit.m2), AIC(fit.m3)), "\n")

# Analise dos Residuos
res <- residuals(fit.model,type="deviance")
ordem <- 1:length(res)
# Desvio Residual
plot(fit.model$fitted.values, res, xlab='fitted', ylab = 'Desvio Residual')
plot(ordem, res, xlab='ordem', ylab = 'Desvio Residual')
hist(res)
qqnorm(res); qqline(res);

# Analise de Normalidade
shapiro.test(res)

###################################################################################
###################################################################################
## Envelope de Probabilidade - Modelo Gamma
X <- model.matrix(fit.model)
n <- nrow(X)
p <- ncol(X)
w <- fit.model$weights
W <- diag(w)
H <- solve(t(X)%*%W%*%X)
H <- sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
h <- diag(H)

ro <- resid(fit.model, type="response")
fi <- sum( (ro/fitted(fit.model))^2 )/(n-p)
nu <- 1/fi
# Desvio Residual - "Ajustado"
td <- resid(fit.model,type="deviance")*sqrt(nu/(1-h))

# Simulacao do envelope
m <- 1000
e <- matrix(0,n,m)	# Armazena as simulacoes
 for(i in 1:m){
    # Gera novas saidas a partir dos valores ajustados (originais)
    resp <- rgamma(n,shape=nu,rate=fitted(fit.model)/nu)
    fit   <- glm(resp ~ X, Gamma(link=log))
    w     <- fit$weights
    W     <- diag(w)
    H     <- solve(t(X)%*%W%*%X)
    H     <- sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
    h     <- diag(H)
    ro    <- resid(fit,type="response")
    phi   <- sum( (ro/fitted(fit))^2 )/(n-p)
    nu.sim <- 1/phi
    e[,i] <- sort( resid(fit,type="deviance")*sqrt(nu.sim/(1-h)) )
 }

# Construcao do envelope com 95% de confianca
e1 <- numeric(n)		# limites inferiores
e2 <- numeric(n)		# limites superiores
for(i in 1:n){
    e0    <- sort(e[i,])	# ordena cada percentil
    e1[i] <- e0[floor(m*.025)]		# Percentil 2,5
    e2[i] <- e0[floor(m*.975)]		# Percentil 97,5
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
