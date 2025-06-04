library(tidyverse)

n = 100

beta1 = 1
beta2 = -1

df = 
  data.frame(
    x1 = rnorm(n),
    x2 = rnorm(n)
  ) |>
  mutate(
    y = rpois(n, exp(beta1 * x1 + beta2 * x2))
  ) |>
  as_tibble()

glimpse(df)

glm(y ~ 0+x1+x2, family = poisson(link = "log"), data = df) |> summary()
