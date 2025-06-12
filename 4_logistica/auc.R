# aula05.R

#install.packages("pROC")  # se necessário
library(pROC)

curva.roc = pROC::roc(dados$Response, predict(modelo, type="response"))
curva.roc$auc
