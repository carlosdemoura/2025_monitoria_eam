###############################################
## Scripts Elementos de Aprendizado de Máquina
###############################################

##Exemplo Regressao Poisson
y   <- c(2, 3, 6, 7, 8, 9, 10, 12, 15)
x   <- c(-1, -1, 0, 0, 0, 0, 1, 1, 1)
plot(x,y)
trn <- list(X=x,Y=y)
modelo <- glm(Y ~ X, family = poisson(link = "log"), data = trn)
summary(modelo)

## Resposta do Modelo para -1 <= x <= 1
dt  <- list( X = seq(-1,1,by=0.1) )
out <- predict(modelo, newdata = dt, type = "response")

plot(x,y,xlab="x",ylab="y")
lines(dt$X, out)

## ligacao "identity"
modelo <- glm(Y ~ X, family = poisson(link = "identity"), data = trn)
##summary(modelo)
out <- predict(modelo, newdata = dt, type = "response")
lines(dt$X, out, col="blue")

## ligacao "sqrt"
modelo <- glm(Y ~ X, family = poisson(link = "sqrt"), data = trn)
##summary(modelo)
out <- predict(modelo, newdata = dt, type = "response")
lines(dt$X, out, col="red")
