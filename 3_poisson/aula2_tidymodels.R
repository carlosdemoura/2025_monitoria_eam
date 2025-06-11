library(tidymodels)
library(poissonreg)

dt = read.table("mlg_birth.csv", header=TRUE, sep=",")

mod =
  poisson_reg() %>%
  set_engine("glm") %>%
  set_mode("regression")

wf =
  workflow() %>%
  add_recipe(recipe(children ~ age, data = dt)) %>%
  add_model(mod)

wf %>% fit(data = dt)

