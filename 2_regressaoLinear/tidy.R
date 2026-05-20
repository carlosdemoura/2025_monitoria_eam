library(tidymodels)
library(ggplot2)
library(tidyverse)

df =
  read.csv("dados/mlg_prostate.csv") %>%
  select(-index) %>% 
  rename(lpsa = "lpsa.y.") %>%
  as_tibble()

################
####  lpsa  ####
################

# log of PSA (lpsa) 
# log cancer volume (lcavol)
# log prostate weight lweight,
# age
# log of benign prostatic hyperplasia amount lbph
# seminal vesicle invasion svi
# log of capsular penetration lcp
# Gleason score gleason
# percent of Gleason scores 4 or 5 pgg45

hist(df$lpsa, prob=TRUE, main="lpsa density", xlab = "lpsa", ylab="density")
rug(df$lpsa)

ggplot(df, aes(x=lpsa)) +
  geom_histogram(bins = 10, col = "white") +
  geom_rug() + 
  labs(title="Histograma")

mod = lm(lpsa ~ age, df)
summary(mod)

qqnorm(mod$residuals)
qqline(mod$residuals)

plot(mod, which = 2)
#?plot.lm

ggplot(df, aes(lpsa)) +
  stat_ecdf(geom = "step") +
  stat_function(
    fun = function(q) pnorm(q, mean(df$lpsa), sd(df$lpsa)),
    col = 2,
    linewidth = 2
  )

ggplot(df, aes(sample = lpsa)) +
  stat_qq() +
  stat_qq_line(col=2)

######################
####  TIDYMODELS  ####
######################

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
  bind_cols(df_teste["lpsa"]) %>%
  rename(
    predito = ".pred",
    real = "lpsa"
  )

metrics(predict, truth = real, estimate = predito)


#################
####  PLOTS  ####
#################

with(predict, plot(real, predito))

ggplot(predict, aes(x = real, y = predito)) +
  geom_point() +
  geom_abline(linetype = "dashed", color = "blue") +
  labs(x = "Valor real", y = "Previsão") +
  theme_minimal()

mod2 = lm(lpsa ~ .-svi -gleason, df_treino)

car::crPlots(mod2)
