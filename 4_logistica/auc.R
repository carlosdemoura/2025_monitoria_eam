#install.packages("pROC")  # se necessário
library(pROC)

curva.roc = roc(trn$y, predict(modelo, type="response"))
plot(curva.roc)
curva.roc$auc
