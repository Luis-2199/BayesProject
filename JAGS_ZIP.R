library(rjags)
library(tidyverse)
library(VGAM)
library(pscl)

mod_dist_4 <- zeroinfl(Vic_Rob_As ~ Seg_Mun  + Region |
                         Edad + Mas_Pat_Vil + Region + Sit_Lab,
                       data= CData_CDMX2, dist="poisson",link="logit")
summary(mod_dist_4)

p <- predict(mod_dist_4, type = "zero")
lambda <- predict(mod_dist_4, type = "count")

sim <- rzipois(1000, lambda[359], p[359])
sim <- rpois(1000, lambda[359])

#### JAGS
attach(CData_CDMX2)

data <- list(
  y = as.numeric(Vic_Rob_As),
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

modelo = "model{
for(i in 1:n){

y[i] ~ dpois(mu[i])
mu[i] <- (u[i]+0.000001)*lambda[i]
u[i] ~ dbern(1-p[i])

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
fit <- jags.model(textConnection(modelo), data, inits, n.chains = 2)
update(fit, 5000)

sample <- coda.samples(fit, param, n.iter = 5000, thin = 1)

plot(sample)
gelman.plot(sample)
summary(sample)
