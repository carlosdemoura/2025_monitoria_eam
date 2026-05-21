library(tidymodels)
library(tidyverse)

dados =
  read.csv("dados/quantal.dat", sep="", stringsAsFactors = TRUE) |>
  as_tibble()

split = initial_split(dados, prop = 0.8, strata = Response)
df_treino = training(split)
df_teste  = testing(split)

mod =
  logistic_reg() %>%
  set_engine("glm")

wf =
  workflow() %>%
  add_recipe(recipe(Response ~ ., data = dados)) %>%
  add_model(mod)

fit = wf %>% fit(data = df_treino)
broom::tidy(fit)


predict =
  predict(fit, df_teste) %>% 
  bind_cols(df_teste["Response"]) %>%
  `colnames<-`(c("predito", "real"))

predict =
  predict(fit, df_teste, type = "prob") %>% 
  bind_cols(df_teste["Response"]) %>%
  `colnames<-`(c("predito"))
  rename(
    predito = ".pred_class",
    real = "Response"
  )

with(predict, plot(real,predito))
