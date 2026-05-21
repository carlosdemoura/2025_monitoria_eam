library(ggplot2)

dados =
  read.csv("dados/quantal.dat", sep="", stringsAsFactors = TRUE) |>
  as_tibble() |>
  mutate(
    Response = as.numeric(Response == "P")
  )

ggplot(dados, aes(x = Rate, y = Response)) +
  geom_point(size=2) +
  geom_smooth(method = glm, formula = y ~ x, method.args = list(family = binomial(link="logit")), se = FALSE) +
  geom_smooth(method = glm, formula = y ~ x, method.args = list(family = binomial(link="probit")), se = FALSE, col = "red")
