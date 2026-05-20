mlg.envelope = function(fit.model, sample.fun) {
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
    nresp <- sample.fun(n, fitted(fit.model))
    fit   <- glm(nresp ~ X, fit.model$family)
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
}

mod = glm(Y~X[,2])

mlg.envelope(mod, {\(n,eta) rbinom(n,1,eta)})

hnp::hnp(mod, resid.type = "deviance")