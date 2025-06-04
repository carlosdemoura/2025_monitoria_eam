###############################################
## Scripts Elementos de Aprendizado de Máquina
###############################################

## Analise de Residuos

##Exemplo Birth data
#seleciona o arquivo "mlg_birth.csv"
dt <- read.table("mlg_birth.csv", header=TRUE, sep=",", stringsAsFactors = TRUE)

names(dt) ## Mostra os nomes das colunas da base de dados

## Ajuste do Modelo
## Poisson Link: log
modelo <- glm(children ~ age, family = poisson(link = "log"), data = dt)

plot(dt$age,dt$children,xlab='age',ylab='children')
x   <- seq(min(dt$age),max(dt$age),by=0.1)
nd  <- list(age=x)
lines(x,predict(modelo,newdata=nd,type='response'),col='red')

1-pchisq(modelo$deviance,modelo$df.residual) ## p-value "Deviance"

## Deviance Residual - Modelo Poisson
## residuals(object, type = c("deviance", "pearson", "working","response", "partial"), ...)
R  <- residuals(modelo, type = "deviance")
plot(modelo$fitted.values, R, xlab='fitted', ylab = 'Desvio Residual')
plot(1:length(R), R, xlab='ordem', ylab = 'Desvio Residual')
hist(R)
qqnorm(R); qqline(R);
shapiro.test(R)

################################################################################
## Para distribuições de resposta não normais em modelos lineares generalizados, 
## a distribuição dos resíduos de Pearson é frequentemente assimétrica. Anscombe 
## propôs um resíduo usando uma função A(y) no lugar de y na derivação dos 
## resíduos (Anscombe 1953, McCullagh e Nelder 1989). A função A(y) é escolhida 
## para tornar a distribuição de A(y) o mais normal possível.
################################################################################
## Residuo de Anscombe - Modelo Poisson
hy    <- 3/2*(modelo$y^(2/3)) 
hyhat <- 3/2*(modelo$fitted.value^(2/3))
Ra    <- (hy - hyhat)/(modelo$fitted.value^(-1/3)*sqrt(modelo$fitted.value)  )
plot(modelo$fitted.values, Ra, xlab='fitted', ylab = 'Residuo Anscombe')
plot(1:length(Ra), Ra, xlab='ordem', ylab = 'Residuo Anscombe')
hist(Ra)
qqnorm(Ra); qqline(Ra);
shapiro.test(Ra)

#################################################################
## Envelope de Probabilidade - Modelo Poisson
 fit.model <- modelo	## MUITO IMPORTANTE !!!!
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
    nresp <- rpois(n, fitted(fit.model))
    fit   <- glm(nresp ~ X, poisson(link = "log"))
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
    e1[i] <- e0[floor(m*.05)]		# Percentil 5
    e2[i] <- e0[floor(m*.95)]		# Percentil 95
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
 