library(tidymodels)
library(poissonreg)

dt = read.table("dados/mlg_birth.csv", header=TRUE, sep=",")

split = initial_split(dt, prop = 0.8, strata = children)
df_treino = training(split)
df_teste  = testing(split)

mod =
  poisson_reg() %>%
  set_engine("glm") %>%
  set_mode("regression")

wf =
  workflow() %>%
  add_recipe(recipe(children ~ age, data = dt)) %>%
  add_model(mod)

fit = wf %>% fit(data = df_treino)
broom::tidy(fit)

predict =
  predict(fit, df_teste) %>% 
  bind_cols(df_teste["children"]) %>%
  rename(
    predito = ".pred",
    real = "children"
  )

with(predict, plot(real,predito))
