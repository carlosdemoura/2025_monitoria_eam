###############################################
## Scripts Elementos de Aprendizado de Máquina
###############################################

###############################################
## Binomial versus Poisson - Potency
###############################################

rm(list=ls())

dados <- read.csv("potency.dat", sep="", stringsAsFactors = TRUE)

## Analise Exploratoria
plot(Kill/Number ~ Poison, data=dados)
plot(Kill/Number ~ LogDose, data=dados)

## Ajuste do Modelo de Poisson - com offset
## Poisson Link: log
mpoisson <- glm(Kill ~ offset(log(Number)) + Poison*LogDose, 
               family = poisson(link = "log"), data = dados)
summary(mpoisson)
1 - pchisq(mpoisson$deviance,mpoisson$df.residual) ## p-value "Deviance"
dados$Poisson <- predict(mpoisson, type="response")
#Akaike information criterion
AIC(mpoisson)

## - - - - - - - - - - - - - - - - - - - - - - - - - -
## Ajuste do Modelo Binomial Link: logit
mbinom <- glm(cbind(Kill, Number-Kill) ~ Poison*LogDose, 
               family = binomial(link = "logit"), data = dados)
#Akaike information criterion
AIC(mbinom)

summary(mbinom)
1 - pchisq(mbinom$deviance,mbinom$df.residual) ## p-value "Deviance"
dados$Binomial <- predict(mbinom, type="response") * dados$Number

## Comentarios Poisson versus Binomial
## VAMOS SUPOR NUMBER = 50 and Poison = D
new.dt  <- data.frame(LogDose = seq(0.2, 10, length.out=500))
new.dt$Number <- 50
new.dt$Poison <- "D"
  
## Poisson
new.dt$poisson <- predict(mpoisson, newdata=new.dt, type="response")
new.dt$SupeP <- qpois(0.95, new.dt$poisson)
new.dt$InfeP <- qpois(0.05, new.dt$poisson)
  
## Binomial
new.dt$binomial <- predict(mbinom, newdata=new.dt, type="response")
new.dt$SupeB <- qbinom(0.95, new.dt$Number, new.dt$binom)
new.dt$InfeB <- qbinom(0.05, new.dt$Number, new.dt$binom)


par(mfrow=c(2,1))
#plot Poisson
plot(Kill ~ LogDose, data=dados, col="dark green", pch=15, xlim=c(0.2, 2))
lines(poisson ~ LogDose, data=new.dt, col="blue", lwd=2)
lines(SupeP ~ LogDose, data=new.dt, col="blue", lty=2)
lines(InfeP ~ LogDose, data=new.dt, col="blue", lty=2)


##plot Binomial
plot(Kill ~ LogDose, data=dados, col="dark green", pch=15, xlim=c(0.2, 2))
lines(50*binomial ~ LogDose, data=new.dt, col="red", lwd=2)
lines(SupeB ~ LogDose, data=new.dt, col="red", lty=2)
lines(InfeB ~ LogDose, data=new.dt, col="red", lty=2)
  
#plot 
par(mfrow=c(1,1))
plot(new.dt$poisson, 50*new.dt$binomial, xlab="Poisson fit", 
      ylab="Binomial fit", type="l")
abline(a=0,b=1,lty=2,col="red")