###############################################
## Scripts Elementos de Aprendizado de Máquina
###############################################

##############################################
## Propriedades Assintoticas dos parametros
##############################################
set.seed(102910)
n   <- 1000
x   <- runif(n,-1,1)

beta0 <- 1.89
beta1 <- 0.67
nsim  <- 1000	# Numero de replicas
Y     <- c()
betas <- matrix(0,nsim,2)

for(j in 1:nsim){
   Y <- rpois(n, exp(beta0 + beta1*x))
   trn    <- list(X=x,Y=Y);
   modelo <- glm(Y ~ X, family = poisson(link = "log"), data = trn)
   betas[j,] <- c(modelo$coef[1], modelo$coef[2])
}

par(mfrow=c(1,2))
hist(betas[,1], main="Beta0")
hist(betas[,2], main="Beta1")
summary(betas[,1])
summary(betas[,2])
shapiro.test(betas[,1])
shapiro.test(betas[,2])
