# Instalação (se necessário)
# install.packages(c("tidymodels", "nnet"))

library(tidymodels)
library(nnet)

mydata =
  read.csv("dados/mlogit.csv") |>
  mutate(
    brand = factor(brand)
  )

split = initial_split(mydata, prop = 0.8, strata = brand)
treino = training(split)
teste  = testing(split)

modelo =
  multinom_reg(penalty = NULL) |>
  set_engine("nnet") |>
  set_mode("classification")

wf = 
  workflow() |>
  add_recipe(recipe(brand ~ ., data = treino)) |>
  add_model(modelo)

fit = 
  fit(wf, data = treino)

fit

# broom::tidy(fit)
