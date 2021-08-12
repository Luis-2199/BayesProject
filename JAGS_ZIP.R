library(rjags)
library(tidyverse)
library(VGAM)
library(pscl)

mod_dist_4 <- zeroinfl(Vic_Rob_As ~ Seg_Mun  + Region |
                         Edad + Mas_Pat_Vil + Region + Sit_Lab,
                       data= CData_CDMX2, dist="poisson",link="logit")
summary(mod_dist_4)

mnull <- update(mod_dist_4, . ~ 1)

pchisq(2*(logLik(mod_dist_4) - logLik(mnull)), df = 6, lower.tail = FALSE) # Es estadísticamente significativo que el poisson sin covariables 

summary(mod_pois_4 <- glm(Vic_Rob_As ~ Seg_Mun + Region, family = poisson, data = CData_CDMX2))

vuong(mod_dist_4, mod_pois_4) # Si es mejor aplicar un Poisson inflado



#### JAGS
attach(CData_CDMX2)

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
inits <- function(){ list(
  "alpha0" = rnorm(1),
  "alpha1" = rnorm(1),
  "beta0" = rnorm(1)
)
}

###### Con este sí convergen, pero difieren mucho los frecuentistas con el Bayesiano
modelo1 = "model{
for(i in 1:n){

y[i] ~ dpois(mu[i])
mu[i] <- (1-u[i]+0.0000001)*lambda[i]
u[i] ~ dbern(p[i])

logit(p[i]) <- alpha0 + alpha1*X1[i] + alpha2[X2[i]] + alpha3[X3[i]] + alpha4[X4[i]] 

log(lambda[i]) <- beta0 + beta1[X5[i]] + beta2[X3[i]]

zdp[i] <- p[i] + (1-p[i])*exp(-lambda[i])}

#### Priors

alpha0 ~ dnorm(0.0, 0.0001)
beta0 ~ dnorm(0.0, 0.0001)

alpha1 ~ dnorm(0.0, 0.0001)

alpha2[1] <- 0
alpha2[2] ~ dnorm(0.0, 0.0001)

alpha3[1] <- 0
alpha3[2] ~ dnorm(0.0, 0.0001)
alpha3[3] ~ dnorm(0.0, 0.0001)
alpha3[4] ~ dnorm(0.0, 0.0001)

alpha4[1] <- 0
alpha4[2] ~ dnorm(0.0, 0.0001)
alpha4[3] ~ dnorm(0.0, 0.0001)

beta1[1] <- 0
beta1[2] ~ dnorm(0.0, 0.0001)

beta2[1] <- 0
beta2[2] ~ dnorm(0.0, 0.0001)
beta2[3] ~ dnorm(0.0, 0.0001)
beta2[4] ~ dnorm(0.0, 0.0001)

}
"
fit1 <- jags.model(textConnection(modelo1), data, inits, n.chains = 2)
update(fit1, 5000)

sample1 <- coda.samples(fit1, param, n.iter = 5000, thin = 1)

plot(sample1)
gelman.plot(sample1)
summary(sample1)
######

######## como Viene en el libro (Creo que no convergen )
modelo2 = "model{
for(i in 1:n){

y[i] ~ dpois(mu[i])
mu[i] <- (u[i] + 0.00000001)*lambda[i]
u[i] ~ dbern(p[i])

logit(p[i]) <- alpha0 + alpha1*X1[i] + alpha2[X2[i]] + alpha3[X3[i]] + alpha4[X4[i]] 

log(lambda[i]) <- beta0 + beta1[X5[i]] + beta2[X3[i]]

zdp[i] <- (1 - p[i]) + p[i]*exp(-lambda[i])}

#### Priors

alpha0 ~ dnorm(0.0, 0.0001)
beta0 ~ dnorm(0.0, 0.0001)

alpha1 ~ dnorm(0.0, 0.0001)

alpha2[1] <- 0
alpha2[2] ~ dnorm(0.0, 0.0001)

alpha3[1] <- 0
alpha3[2] ~ dnorm(0.0, 0.0001)
alpha3[3] ~ dnorm(0.0, 0.0001)
alpha3[4] ~ dnorm(0.0, 0.0001)

alpha4[1] <- 0
alpha4[2] ~ dnorm(0.0, 0.0001)
alpha4[3] ~ dnorm(0.0, 0.0001)

beta1[1] <- 0
beta1[2] ~ dnorm(0.0, 0.0001)

beta2[1] <- 0
beta2[2] ~ dnorm(0.0, 0.0001)
beta2[3] ~ dnorm(0.0, 0.0001)
beta2[4] ~ dnorm(0.0, 0.0001)

}
"


fit2 <- jags.model(textConnection(modelo2), data, inits, n.chains = 2)
update(fit2, 5000)

set.seed(1)
sample2 <- coda.samples(fit2, param, n.iter = 5000, thin = 1)

plot(sample2)
gelman.plot(sample2)
summary(sample2)
