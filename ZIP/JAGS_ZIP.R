library(rjags)
library(tidyverse)
library(VGAM)
library(pscl)

load(file = "BD_CDMX2.Rda")


CData_CDMX2_resample <- sample_n(CData_CDMX2, 5419)

# Ajustamos modelo
mod_dist_4 <- zeroinfl(Vic_Rob_As ~ Seg_Mun  + Region |
                         Edad + Mas_Pat_Vil + Region + Sit_Lab,
                       data= CData_CDMX2_resample, dist="poisson",link="logit")
summary(mod_dist_4)


####### Base sin 7s ########
CData_CDMX2_sin7 <- CData_CDMX2 %>% filter(Vic_Rob_As < 7)

CData_CDMX2_sin7_resample <- sample_n(CData_CDMX2_sin7, 5419)

# Ajustamos modelo
mod_ZIPsin7 <- zeroinfl(Vic_Rob_As ~ Seg_Mun  + Region |
                          Edad + Mas_Pat_Vil + Region + Sit_Lab,
                        data= CData_CDMX2_sin7, dist="poisson",link="logit")
summary(mod_ZIPsin7)



#### JAGS ######
attach(CData_CDMX2_sin7)

data <- list(
  y = Vic_Rob_As,
  X1 = Edad,
  X2 = Mas_Pat_Vil,
  X3 = as.numeric(Region),
  X4 = as.numeric(Sit_Lab),
  X5 = Seg_Mun,
  n =length(Vic_Rob_As)
)

param <- c("alpha0", "alpha1", "alpha2", "alpha3", "alpha4", "beta0", "beta1", "beta2")
inits <-  list(.RNG.name = "base::Wichmann-Hill", .RNG.seed =1
  # "alpha0" = rnorm(1),
  # "alpha1" = rnorm(1),
  # "beta0" = rnorm(1),
  # "alpha2" = c(NA, rnorm(1)),
  # "alpha3" = c(NA, rnorm(1), rnorm(1), rnorm(1)),
  # "alpha4" = c(NA, rnorm(1), rnorm(1)),
  # "beta1" = c(NA, rnorm(1)),
  # "beta2" = c(NA, rnorm(1), rnorm(1), rnorm(1))
  
)


#### Otra forma de poner el p[i] de JAGS 

# p[i] <- 1/(1.00001 + exp(-(alpha0 + alpha1*X1[i] + alpha2[X2[i]] + alpha3[X3[i]] + alpha4[X4[i]])))


######## como Viene en el articulo 
modelo = "model{
for(i in 1:n){

y[i] ~ dpois(mu[i])
mu[i] <- u[i]*lambda[i] + 0.00001
u[i] ~ dbern(p[i])

logit(p[i]) <- alpha0 + alpha1*X1[i] + alpha2[X2[i]] + alpha3[X3[i]] + alpha4[X4[i]]

log(lambda[i]) <- beta0 + beta1[X5[i]] + beta2[X3[i]]

zdp[i] <- (1 - p[i]) + p[i]*exp(-lambda[i])}
mzdp <- mean(zdp[])

#### Priors

alpha0 ~ dnorm(0.0, 0.001)
beta0 ~ dnorm(0.0, 0.001)

alpha1 ~ dnorm(0.0, 0.001)

alpha2[1] <- 0
alpha2[2] ~ dnorm(0.0, 0.001)

alpha3[1] <- 0
alpha3[2] ~ dnorm(0.0, 0.001)
alpha3[3] ~ dnorm(0.0, 0.001)
alpha3[4] ~ dnorm(0.0, 0.001)

alpha4[1] <- 0
alpha4[2] ~ dnorm(0.0, 0.001)
alpha4[3] ~ dnorm(0.0, 0.001)

beta1[1] <- 0
beta1[2] ~ dnorm(0.0, 0.001)

beta2[1] <- 0
beta2[2] ~ dnorm(0.0, 0.001)
beta2[3] ~ dnorm(0.0, 0.001)
beta2[4] ~ dnorm(0.0, 0.001)

}
"

fit <- jags.model(textConnection(modelo), data, inits, n.chains = 3)
update(fit, 10000)

sample <- coda.samples(fit, param, n.iter = 5000, thin = 2)

plot(sample[,18])
gelman.plot(sample[,1])
summary(sample)

#  Simulación 1
# sample_sem1 <- sample
sample_sem1 <- sample_sem1

plot(sample_sem1)
summary(sample_sem1)
gelman.plot(sample_sem1[,1])
gelman.diag(sample_sem1[,1])
traceplot(sample_sem1)


# Simulación 2 con 3 Cadenas
# sample_sem2 <- sample
sample_sem2 <- sample_sem2


# ###### 
# modelo1 = "model{
# for(i in 1:n){
# 
# y[i] ~ dpois(mu[i])
# mu[i] <- (1-u[i])*lambda[i] + 0.0000000001
# u[i] ~ dbern(p[i])
# 
# logit(p[i]) <- alpha0 + alpha1*X1[i] + alpha2[X2[i]] + alpha3[X3[i]] + alpha4[X4[i]] 
# 
# log(lambda[i]) <- beta0 + beta1[X5[i]] + beta2[X3[i]]
# 
# zdp[i] <- p[i] + (1-p[i])*exp(-lambda[i])}
# 
# #### Priors
# 
# alpha0 ~ dnorm(0.0, 0.0001)
# beta0 ~ dnorm(0.0, 0.0001)
# 
# alpha1 ~ dnorm(0.0, 0.0001)
# 
# alpha2[1] <- 0
# alpha2[2] ~ dnorm(0.0, 0.0001)
# 
# alpha3[1] <- 0
# alpha3[2] ~ dnorm(0.0, 0.0001)
# alpha3[3] ~ dnorm(0.0, 0.0001)
# alpha3[4] ~ dnorm(0.0, 0.0001)
# 
# alpha4[1] <- 0
# alpha4[2] ~ dnorm(0.0, 0.0001)
# alpha4[3] ~ dnorm(0.0, 0.0001)
# 
# beta1[1] <- 0
# beta1[2] ~ dnorm(0.0, 0.0001)
# 
# beta2[1] <- 0
# beta2[2] ~ dnorm(0.0, 0.0001)
# beta2[3] ~ dnorm(0.0, 0.0001)
# beta2[4] ~ dnorm(0.0, 0.0001)
# 
# }
# "
# fit1 <- jags.model(textConnection(modelo1), data, inits, n.chains = 2)
# update(fit1, 2000)
# 
# sample1 <- coda.samples(fit1, param, n.iter = 2000, thin = 1)
# 
# plot(sample1)
# gelman.plot(sample1)
# summary(sample1)}

# Simulación 2 con thin = 2
# sample2_sem2 <- sample2
# sample2_sem2 <- sample2_sem2
# 
# plot(sample2_sem2)
# summary(sample2_sem2)



save(sample_sem2,file="Sample_conv3_chida_JAGS.Rda")






