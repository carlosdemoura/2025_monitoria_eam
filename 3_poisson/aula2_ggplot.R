library(ggplot2)
library(tidyverse)

###############
##  parte 1  ##
###############

df =
  data.frame(
    x = c(-1, -1, 0, 0, 0, 0, 1, 1, 1),
    y = c(2, 3, 6, 7, 8, 9, 10, 12, 15)
    )

mod_log  = glm(y ~ x, family = poisson(link = "log"), data = df)
mod_id   = glm(y ~ x, family = poisson(link = "identity"), data = df)
mod_sqrt = glm(y ~ x, family = poisson(link = "sqrt"), data = df)

novos_dados =
  data.frame(
    x = seq(-1, 1, by = 0.1)
    )

novos_dados =
  novos_dados |>
  mutate(
    log  = predict(mod_log,  newdata = novos_dados, type = "response"),
    id   = predict(mod_id,   newdata = novos_dados, type = "response"),
    sqrt = predict(mod_sqrt, newdata = novos_dados, type = "response")
  ) |>
  pivot_longer(
    cols = c("log", "id", "sqrt"),
    names_to = "link",
    values_to = "predicao"
    )

ggplot() +
  geom_point(data = df, aes(x = x, y = y), size = 2) +
  geom_line(data = novos_dados, aes(x = x, y = predicao, color = link), size = 1) +
  labs(x = "x", y = "y", title = "Regressão de Poisson com Diferentes Ligações") +
  theme_minimal()


###############
##  parte 2  ##
###############

# modelo.glm = glm(children ~ age, family = poisson(link = "log"), data = dt)
# modelo.lm = lm(children ~ age, data = dt)
# nd = list(age=seq(min(dt$age),max(dt$age),by=0.1))
# 
# plot(dt$age,dt$children,xlab='age',ylab='children')
# lines(x,predict(modelo.glm,newdata=nd,type='response'),col='red')
# lines(x,predict(modelo.lm,newdata=nd,type='response'),col='green')

ggplot(dt, aes(x = age, y = children)) +
  geom_point(size=2) +
  geom_smooth(method = glm, formula = y ~ x, method.args = list(family = poisson(link="log")))
  #geom_smooth(method = glm, method.args = list(family = poisson(link="sqrt")))

ggplot(dt, aes(age, children)) +
  geom_point() +
  geom_smooth(method = "lm")

# predict(modelo.lm, newdata = nd, interval = "prediction", level = 0.95)
# predict(modelo.glm, newdata = nd, type = "link", se.fit = TRUE)
