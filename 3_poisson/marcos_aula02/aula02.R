###############################################
## Scripts Elementos de Aprendizado de Máquina
###############################################

##Exemplo Regressao Poisson
y   <- c(2, 3, 6, 7, 8, 9, 10, 12, 15)
x   <- c(-1, -1, 0, 0, 0, 0, 1, 1, 1)
plot(x,y)
trn <- list(X=x,Y=y)
modelo_log <- glm(Y ~ X, family = poisson(link="log"), data = trn)
summary(modelo_log)

## Resposta do Modelo para -1 <= x <= 1
dt  <- list( X = seq(-1,1,by=0.1) )
out <- predict(modelo_log, newdata = dt, type = "response")

plot(x,y,xlab="x",ylab="y")
lines(dt$X, out)

## ligacao "identity"
modelo_id <- glm(Y ~ X, family = poisson(link = "identity"), data = trn)
##summary(modelo)
out <- predict(modelo_id, newdata = dt, type = "response")
lines(dt$X, out, col="blue")
summary(modelo_id)

## ligacao "sqrt"
modelo_sqrt <- glm(Y ~ X, family = poisson(link = "sqrt"), data = trn)
##summary(modelo)
out <- predict(modelo_sqrt, newdata = dt, type = "response")
summary(modelo_sqrt)
lines(dt$X, out, col="red")
