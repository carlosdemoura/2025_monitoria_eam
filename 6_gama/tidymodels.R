library(tidymodels)

dados = read.table("dados/oxidation_gamma.dat", header=TRUE, sep="")
split = initial_split(dados, prop = 0.8, strata = Rate)

dados_treino = training(split)
dados_teste  = testing(split)

modelo = 
  linear_reg() |>
  set_engine(
    "glm",
    family = Gamma(link = "log")
  ) |>
  set_mode("regression")

wf =
  workflow() |>
  add_recipe(recipe(Rate ~ ., data = dados_treino)) |>
  add_model(modelo)

fit = 
  fit(wf, data = dados_treino)

predict =
  predict(fit, dados_teste) |>
  bind_cols(dados_teste["Rate"]) |>
  `colnames<-`(c("predito", "real"))
