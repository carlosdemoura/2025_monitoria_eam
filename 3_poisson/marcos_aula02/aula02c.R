###############################################
## Scripts Elementos de Aprendizado de Máquina
###############################################

##Exemplo Birth data
#seleciona o arquivo "mlg_birth.csv"
dt <- read.table("mlg_birth.csv", header=TRUE, sep=",")

## Poisson Link: log, identity, sqrt
## Ajusta o modelo Poisson com link log
modelo <- glm(children ~ age, family = poisson(link = "log"), data = dt)
summary(modelo)

modelo$deviance
1-pchisq(modelo$deviance,modelo$df.residual) ## p-value "Deviance"

#cria possíveis idades para previsão
x <- seq(min(dt$age),max(dt$age),by=0.1)
nd     <- list(age=x)

#grafica as observacoes
plot(dt$age,dt$children,xlab='age',ylab='children')

#adiciona a linha vermelha com o valor predito para cada idade
lines(x,predict(modelo,newdata=nd,type='response'),col='red')

#imprime o deviance e seu respectivo p-valor
c(modelo$deviance, 1-pchisq(modelo$deviance,modelo$df.residual))

## Ajusta e grafica o modelo Poisson com link identidade
modelo <- glm(children ~ -1 + age, family = poisson(link = "identity"), data = dt)
lines(x,predict(modelo,newdata=nd,type='response'),col='blue')

#imprime o deviance e seu respectivo p-valor
c(modelo$deviance, 1-pchisq(modelo$deviance,modelo$df.residual))

### Ajusta outras ligações e grafica o ajuste

## Ajusta e grafica o modelo Poisson com link raiz quadrada
modelo <- glm(children ~ age, family = poisson(link = "sqrt"), data = dt)
lines(x,predict(modelo,newdata=nd,type='response'),col='black')

#imprime o deviance e seu respectivo p-valor
c(modelo$deviance, 1-pchisq(modelo$deviance,modelo$df.residual))

## Ajusta e grafica o modelo de regressao linear
modelo.lm <- lm(children ~ age, data = dt)
lines(x,predict(modelo.lm,newdata=nd,type='response'),col='green')

