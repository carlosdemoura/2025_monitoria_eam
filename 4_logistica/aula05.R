###############################################
## Scripts Elementos de Aprendizado de Máquina
###############################################

###############################################
## Curva ROC
###############################################
rm(list=ls())
source("plot_roc.R")

#seleciona o arquivo "quantal.dat"
dados <- read.csv("quantal.dat", sep="",stringsAsFactors = TRUE)


## Ajuste do Modelo
modelo <- glm(Response ~ Volume + Rate, family = binomial(link = "logit"), 
               data = dados)
summary(modelo)
## Calculo do p-valor
c( modelo$deviance, 1-pchisq(modelo$deviance,modelo$df.residual) )

## Analise de Sensibilidade e Especificidade
s_e <- roc(modelo$y, modelo$fitted.values) 

## Plota as curvas de sensibilidade e especificidade
plot(s ~ tau, data=s_e, type="l", col="red"); grid()
lines(e ~ tau, data=s_e, col="blue", lty=2)
legend("bottomleft", c("sensibilidade","especificidade"),
        col=c("red","blue"), lty=c(1,2),bty="n", cex=1.3, lwd=2)

## Plota a Curva ROC
plot.roc(modelo$fitted.values,modelo$y)

## Plota a Curva ROC
plot(s ~ I(1 - e), data=s_e, ylab="sensibilidade", xlab="1-especificidade",
      type="l", col="red", lwd=2); grid()

