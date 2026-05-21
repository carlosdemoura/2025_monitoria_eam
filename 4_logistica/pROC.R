library(tidyverse)
#install.packages("pROC")  # se necessário
library(pROC)

dados =
  read.csv("dados/quantal.dat", sep="", stringsAsFactors = TRUE) |>
  as_tibble() |>
  mutate(
    Response = as.numeric(Response == "P")
  )

modelo = glm(Response ~ Volume + Rate, family = binomial(link = "logit"), data = dados)
curva.roc = pROC::roc(dados$Response, predict(modelo, type="response"))

curva.roc$auc
plot(curva.roc)

ggroc(curva.roc) +
  labs(x = "Especificidade", y = "Sensibilidade")


modelo2 = glm(Response ~ Volume, family = binomial(link = "logit"), data = dados)
curva.roc2 = pROC::roc(dados$Response, predict(modelo2, type="response"))

ggroc(
  list(
    "y ~ vol + rate" = curva.roc,
    "y ~ vol" = curva.roc2
  )) +
  labs(x = "Especificidade", y = "Sensibilidade")

  







df = data.frame(
  tau = curva.roc$thresholds,
  sensibilidade = curva.roc$sensitivities,
  especificidade = curva.roc$specificities
)

plot(sensibilidade ~ tau, data = df, col = "red", type = "l")
lines(especificidade ~ tau, data = df, col = "blue")
legend(
  "bottomleft",
  legend = c("Sensibilidade", "Especificidade"),
  col = c("red", "blue"),
  lty = c(1, 1)
)
