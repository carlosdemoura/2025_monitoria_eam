library(ggplot2)

# aula04.R

ggplot(trn, aes(x = x1, y = y)) +
  geom_point(size=2) +
  geom_smooth(method = glm, formula = y ~ x, method.args = list(family = binomial(link="logit")))
