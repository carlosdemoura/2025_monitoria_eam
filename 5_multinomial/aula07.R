## - - - - - - - - - - - - - - - - - - - - - - - - - ##
## Regressao Multinomial
## - - - - - - - - - - - - - - - - - - - - - - - - - ##
library(tidyverse)

## install.packages('mlogit', dependencies=TRUE)  # se necessário
library(mlogit)

#seleciona o arquivo "mlogit.csv"
mydata <-
  read.csv("dados/mlogit.csv") |>
  mutate(
    brand = factor(brand)
    #brand = relevel(brand, "2")
  ) |>
  as_tibble()

# Coloca a variavel "brand" como "fator" (categorica)
levels(mydata$brand) <- c("A", "B", "C")
#levels(mydata$brand) <- c("B", "A", "C")
mydata$male  <- 1 - mydata$female 

# Coloca os dados no formato da funcao "mlogit"
mldata.factor   <- mlogit.data(mydata, choice="brand", shape="wide")
mldata.numeric  <- mlogit.data(read.csv("dados/mlogit.csv"), choice="brand", shape="wide")
head(mldata.factor) 
head(mldata.numeric) 


mlogit.model.factor  <- mlogit(brand ~ 0|female+age, data = mldata.factor)
mlogit.model.numeric <- mlogit(brand ~ 0|female+age, data = mldata.numeric)

# Exibe um sumario do modelo
summary(mlogit.model.factor)
summary(mlogit.model.numeric)

fit1 = broom::tidy(mlogit.model.factor) 
fit2 = broom::tidy(mlogit.model.numeric)

all.equal(fit1, fit2)

# Predicao de novas observacoes (OBS: as entradas tem que ser "replicadas")

newdata = 
  data.frame(
    female = rep(0, 3),                        
    age    = rep(20, 3)
    )

saida <- predict(mlogit.model.factor, newdata=newdata)
round(saida, digits=5)



## - - - - - - - - - - - - - - - - - - - - - - - - - ##
## Regressao Multinomial
## - - - - - - - - - - - - - - - - - - - - - - - - - ##
rm(list=ls(all=TRUE))
## install.packages('mlogit', dependencies=TRUE)
library(mlogit)

#seleciona o arquivo "mlogit.csv"
mydata <- read.csv("dados/mlogit.csv")

# Coloca a variavel "brand" como "fator" (categorica)
mydata$brand <- as.factor(mydata$brand)
levels(mydata$brand) <- c("A", "B", "C")
mydata$male  <- 1 - mydata$female 

# Coloca os dados no formato da funcao "mlogit"
mldata  <- mlogit.data(mydata, choice="brand", shape="wide")
head(mldata) 
mlogit.model <- mlogit(brand ~ 0|female+age, data = mldata, 
                       reflevel="A")

# Exibe um sumario do modelo
summary(mlogit.model)

# Predicao de novas observacoes (OBS: as entradas tem que ser "replicadas")
saida <- predict(mlogit.model, newdata=data.frame(female=rep(1, 3),                        
                                                  age=rep(38, 3)))
round(saida, digits=5)

