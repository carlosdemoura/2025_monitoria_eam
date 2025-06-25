###############################################
## Scripts Elementos de Aprendizado de Máquina
###############################################

###############################################
## EXEMPLO CART - Regressao e Classificacao
###############################################
rm(list=ls(all=TRUE))

# Carrega os pacote
if(!require(faraway)){install.packages("faraway"); require(faraway)}
if(!require(rpart))  {install.packages("rpart");  require(rpart)}

# Carrega uma base de dados especifica
data(ozone)

summary(ozone)
pairs(ozone)
 
################################################################
### Exemplo de Ajuste Univariado                             ###
################################################################
plot(O3 ~ doy, data=ozone, pch=19, col="blue"); grid()

# Ajuste do modelo
modelo <- rpart(O3 ~ doy, data=ozone)

doy    <- 25:400
lines(doy, predict(modelo,newdata=data.frame(doy=doy)),
      col="red",lwd=2)

# ver ?plot.rpart
plot(modelo, compress=T, uniform=T, branch= 0.4, margin=0.10)
text(modelo)

## Uso de Penalizacao e Validacao Cruzada
model <- rpart(O3 ~ doy, data=ozone, cp=0.01)
## cp: complexity parameter

plot(model, compress=T, uniform=T, branch= 0.4, margin=0.10)
text(model)
printcp(model)
plotcp(model)

## Modelo Final
model <- prune.rpart(model,0.018)
plot(model,compress=T,uniform=T,branch= 0.4,margin=0.10)
text(model)

plot(O3 ~ doy, data=ozone, pch=19, col="blue"); grid()
lines(doy, predict(model,newdata=data.frame(doy=doy)),
      col="red",lwd=2)

################################################################
### Exemplo de Ajuste Multivariado                           ###
################################################################
roz <- rpart(O3 ~ ., data=ozone)
## Verificando o modelo
roz

## Diferentes visulizacoes
plot(roz, margin=0.10)
text(roz)
plot(roz,compress=T,uniform=T,branch= 0.4,margin=0.10)
text(roz)

## Qualidade de Ajuste do Modelo
plot(predict(roz), residuals(roz), xlab="Fitted", ylab="Residuals")
qqnorm(residuals(roz)); qqline(residuals(roz))
shapiro.test(residuals(roz))
