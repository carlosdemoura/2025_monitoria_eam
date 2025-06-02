library(tidymodels)
library(ggplot2)
library(reglin)
library(tidyverse)

df =
  "mlg_prostate.csv" %>%
  read.csv() %>%
  select(-index) %>%
  rename(lpsa = "lpsa.y.")


##################
####  R BASE  ####
##################

hist(df$lpsa, prob=TRUE, main="lpsa density", xlab = "lpsa", ylab="density")
rug(df$lpsa)

mod_base = lm(lpsa ~ . -svi -gleason, df)
summary(mod_base)
plot(mod_base)

qqnorm(mod_base$residuals)
qqline(mod_base$residuals)


######################
####  TIDYMODELS  ####
######################

# Ajuste
set.seed(12345)

split = initial_split(df, prop = 0.8, strata = lpsa)
df_treino = training(split)
df_teste  = testing(split)

model_spec =
  linear_reg() %>% 
  set_engine("lm") %>% 
  set_mode("regression")

reg_workflow =
  workflow() %>% 
  add_model(model_spec) %>% 
  add_formula(lpsa ~ . -svi -gleason)

reg_fit = fit(reg_workflow, data = df_treino)
reg_fit %>% tidy()


# Previsões
predict =
  predict(reg_fit, df_teste) %>% 
  bind_cols(df_teste$lpsa) %>%
  rename(
    predito = ".pred",
    real = "...2"
  )

metrics(predict, truth = real, estimate = predito)


#################
####  PLOTS  ####
#################

ggplot(predict, aes(x = real, y = predito)) +
  geom_point() +
  geom_abline(linetype = "dashed", color = "blue") +
  labs(x = "Valor real", y = "Previsão") +
  theme_minimal()

mod = lm(lpsa ~ .-svi -gleason, df_treino)

car::crPlots(mod)

reglin::ggresiduals(mod)
