###############################################
## Scripts Elementos de Aprendizado de Máquina
###############################################

###############################################
## Regressao Logistica
###############################################
rm(list=ls())

# Dados do experimento sobre a influencia da razao e do volume de ar
# inspirado na ocorrencia de vaso-constricao da pele dos dedos da mao

# y = ocorrencia (y=1) ou ausencia (y=0) de compressao de vasos
y  <- c(1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 1, 1, 1, 0, 0, 1)

# x1 = log do volume de ar inspirado
x1 <- c(3.70, 3.50, 1.25, 0.75, 0.80, 0.70, 0.60, 1.10, 0.90, 0.90, 0.80, 0.55, 0.60, 1.40, 0.75, 2.30, 3.20, 0.85, 1.70, 1.80, 0.40, 0.95, 1.35, 1.50, 1.60, 0.60, 1.80, 0.95, 1.90, 1.60, 2.70, 2.35, 1.10, 1.10, 1.20, 0.80, 0.95, 0.75, 1.30)

# Com a funcao glm()
trn    <- data.frame(y=y, x1=x1);
modelo <- glm(y ~ x1, family = binomial, data = trn)
summary(modelo)
c( modelo$deviance, pchisq(modelo$deviance,modelo$df.residual, lower.tail = F) )

##plota residuos
par(mfrow=c(2,2))
plot(modelo, pch=19, col="blue")

#estuda parametros
confint(modelo)
exp(confint(modelo))

#verifica ajuste
par(mfrow=c(1,1))
xseq <- seq(0, 4, length=100)
plot(y ~ x1, data=trn, col="blue", pch=19, xlim=c(min(xseq),max(xseq)))
newdt <- data.frame(x1=xseq)
lines(predict(modelo, newdata=newdt, type="response")~xseq)
