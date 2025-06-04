###############################################
## Scripts Elementos de Aprendizado de Máquina
###############################################

##Exemplo Diabetes data
#seleciona o arquivo "diabetes.csv"
dt <- read.table("diabetes.csv", header=TRUE, sep=",",stringsAsFactors = TRUE)

pairs(dt)
names(dt)
plot(dt$gender, dt$deaths/dt$popn)
plot(dt$gender, log10(dt$deaths/dt$popn))
plot(dt$age,    dt$deaths/dt$popn)
plot(dt$age,    log10(dt$deaths/dt$popn))

## Poisson Link: log
modelo <- glm(deaths ~ offset(log(popn)) + age + gender, 
              family = poisson, data = dt)
summary(modelo)
modelo$deviance
1-pchisq(modelo$deviance,modelo$df.residual) ## p-value "Deviance"

## Poisson Link: identity
modelo <- glm(deaths ~ offset(log(popn)) + age + gender, 
              family = poisson(link = "identity"), data = dt)
summary(modelo)
modelo$deviance
1-pchisq(modelo$deviance,modelo$df.residual) ## p-value "Deviance"

## Poisson Link: sqrt
modelo <- glm(deaths ~ offset(log(popn)) + age + gender, 
              family = poisson(link = "sqrt"), data = dt)
summary(modelo)
modelo$deviance
1-pchisq(modelo$deviance,modelo$df.residual) ## p-value "Deviance"
