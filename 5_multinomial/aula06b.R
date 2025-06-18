###############################################
## Scripts Elementos de Aprendizado de Máquina
###############################################

###############################################
## Binomial versus Poisson - Diabetes
###############################################
rm(list=ls())

#seleciona o arquivo "diabetes.csv"
dados <- read.csv("diabetes.csv", stringsAsFactors = TRUE)
names(dados)

## Ajuste do Modelo de Poisson 
mpoisson <- glm(deaths ~ offset(l_popn) + age + gender, family = poisson, data = dados)
summary(mpoisson)
1-pchisq(mpoisson$deviance,mpoisson$df.residual) ## p-value "Deviance"
dados$Poisson <- mpoisson$fitted/dados$popn
#Akaike information criterion
AIC(mpoisson)
 
## Ajuste do Modelo
## Binomial Link: logit
mbinom <- glm(cbind(deaths, popn-deaths) ~ age + gender, family = binomial, data = dados)
summary(mbinom)
1-pchisq(mbinom$deviance,mbinom$df.residual) ## p-value "Deviance"
dados$Binomial <- mbinom$fitted
exp(coefficients(mbinom))
#Akaike information criterion
AIC(mbinom)
 
## Comentarios Poisson versus Binomial
## VAMOS SUPOR POP = 1000 and gender = F
new.dt  <- data.frame(age = unique(dados$age))
new.dt$popn <- 1000
new.dt$l_popn <- log(1000)
new.dt$gender <- "Female"
 
## Poisson
new.dt$poisson <- predict(mpoisson, newdata=new.dt, type="response")
 
## Binomial
new.dt$binomial <- new.dt$popn*predict(mbinom, newdata=new.dt, type="response")
 
#plot 
plot(new.dt$poisson, new.dt$binomial, xlab="Poisson fit", 
      ylab="Binomial fit", type="l", lwd=2)
abline(a=0,b=1,lty=2,col="red")
 