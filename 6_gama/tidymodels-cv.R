library(tidymodels)
library(poissonreg)

dados = read.table("dados/oxidation_gamma.dat", header=TRUE, sep="") |> as_tibble()
split = initial_split(dados, prop = 0.8, strata = Rate)

dados_treino = training(split)
dados_teste  = testing(split)

modelos = list()

modelos[["gama"]] = 
  linear_reg() |>
  set_engine(
    "glm",
    family = Gamma(link = "log")
  ) |>
  set_mode("regression")

modelos[["poisson"]] = 
  poisson_reg() |>
  set_engine("glm") |>
  set_mode("regression")

modelos[["normal"]] = 
  linear_reg() |>
  set_engine(
    "glm",
    family = gaussian()
  ) |>
  set_mode("regression")


predict = list()
for (modelo in names(modelos)) {
  predict[[modelo]] =
    workflow() |>
    add_recipe(recipe(Rate ~ ., data = dados_treino)) |>
    add_model(modelos[[modelo]]) |>
    fit(data = dados_treino) |>
    predict(dados_teste) |>
    `colnames<-`(modelo)
}

predict[["real"]] = dados_teste[["Rate"]]

cv =
  do.call(cbind, predict) |>
  pivot_longer(cols = 1:3, names_to = "modelo", values_to = "predito") |>
  relocate(modelo) |>
  mutate(
    rb = (predito - real) / abs(real)
  ) |>
  group_by(modelo) |>
  summarise(
    mrb = mean(rb)
  )
