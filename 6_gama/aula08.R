###############################################
## Scripts Elementos de Aprendizado de Máquina
###############################################

###############################################
## EXEMPLO BOSTON HOUSING
## Precificacao de Imoveis
###############################################
rm(list=ls(all=TRUE))
require(MASS)

#seleciona o arquivo
dt <- read.table( "mlg_boston.csv" , header=TRUE, sep=",")

#faz analises descritivas com o histograma e cdf empirica
hist(dt$MEDV, prob=TRUE, main="MEDV density", xlab = "MEDV", ylab="density")
rug(dt$MEDV)
 
plot(ecdf(dt$MEDV), do.points=FALSE, verticals=TRUE, main="ECDF MEDV")
x <- seq(min(dt$MEDV), max(dt$MEDV), 0.01)
lines(x, pnorm(x, mean=mean(dt$MEDV), sd=sd(dt$MEDV)), lty=3)

## Modelo Completo
dt$LSTAT2 <- dt$LSTAT^2
fit.model <- lm(MEDV..y. ~ ., data = dt)
summary(fit.model)
hist(fit.model$res,breaks=30)
plot(fit.model)
shapiro.test(fit.model$res)

## Modelo Completo
#link identity
fit.model <- glm(MEDV..y. ~ . ,family=Gamma(link=identity), data = dt)
summary(fit.model)
1 - pchisq(fit.model$deviance,fit.model$df.residual) ##

#link inverse
fit.model <- glm(MEDV..y. ~ . ,family=Gamma(link=inverse), data = dt)
summary(fit.model)
1 - pchisq(fit.model$deviance,fit.model$df.residual) ##

#link log
fit.model <- glm(MEDV..y. ~ . ,family=Gamma(link=log), data = dt)
summary(fit.model)
1 - pchisq(fit.model$deviance,fit.model$df.residual) ##

# Uso do StepAIC
model.AIC <- stepAIC(fit.model, trace=FALSE)
summary(model.AIC)

## Modelo Final
fit.model <- glm(MEDV..y. ~ CRIM + CHAS + NOX + RM + AGE + DIS + 
                    RAD + TAX + PTRATIO + B + LSTAT + LSTAT2 ,
                 family=Gamma(link=log), data = dt)
summary(fit.model)

## Qualidade do Modelo
1-pchisq(fit.model$deviance,fit.model$df.residual) ## p-value "Deviance"

## Analise dos Residuos
res <- residuals(fit.model,type="deviance")
ordem <- 1:length(res)
# Desvio Residual
plot(fit.model$fitted.values, res, xlab='fitted', ylab = 'Desvio Residual')
plot(ordem, res, xlab='ordem', ylab = 'Desvio Residual')
hist(res, breaks=20)
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
#Pela definicao usada no R temos que fi = 1/nu 
fi <- sum((ro/fitted(fit.model))^2 )/(n-p) 
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
   e1[i] <- e0[floor(m*.025)]		# Percentil 2.5
   e2[i] <- e0[floor(m*.975)]		# Percentil 97.5
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
 
