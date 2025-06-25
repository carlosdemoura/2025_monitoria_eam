## - - - - - - - - - - - - - - - - - - - - - - - - - ##
## Regressao Multinomial
## - - - - - - - - - - - - - - - - - - - - - - - - - ##
library(tidyverse)

## install.packages('mlogit', dependencies=TRUE)  # se necessário
library(mlogit)

#seleciona o arquivo "mlogit.csv"
mydata <-
  read.csv("mlogit.csv") %>%
  mutate(
    brand = factor(brand)
  )

# Coloca a variavel "brand" como "fator" (categorica)
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

newdata = 
  data.frame(
    female = rep(0, 3),                        
    age    = rep(20, 3)
    )

saida <- predict(mlogit.model, newdata=newdata)
round(saida, digits=5)
