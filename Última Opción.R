library(rjags)
library(tidyverse)

CData_CDMX3 <- CData_CDMX2 %>% 
  mutate(Vic_Rob_As = ifelse(Vic_Rob_As>0,1,0))
CData_CDMX3$Vic_Rob_As <- as_factor(CData_CDMX3$Vic_Rob_As)

sum(CData_CDMX2$Vic_Rob_As>0)
sum(CData_CDMX3$Vic_Rob_As)

modlog <- glm(Vic_Rob_As ~ .,family = "binomial", data=CData_CDMX3)
summary(modlog)

library(car)
vif(modlog)

r <- ifelse(modlog$fitted.values>=0.5,1,0)
table(r,CData_CDMX3$Vic_Rob_As)

(4330+1)/5419

modlog1 <- glm(Vic_Rob_As ~ Edad + Sit_Lab + Seg_Mun + Mas_Pat_Vil + Nivel_Edu + Region, family = "binomial",data=CData_CDMX3)
summary(modlog1)

vif(modlog1)

r1 <- ifelse(modlog1$fitted.values>=0.5,1,0)
table(r1,CData_CDMX3$Vic_Rob_As)

(4332)/5419

modlog2 <- glm(Vic_Rob_As ~ Edad + Sit_Lab + Seg_Mun + Mas_Pat_Vil + Region, family = "binomial",data=CData_CDMX3)
summary(modlog2)

vif(modlog2)

r2 <- ifelse(modlog2$fitted.values>=0.4,1,0)
table(r2,CData_CDMX3$Vic_Rob_As)


y<-exp(-.297618+-0.023261*36+0.522345+0.147011)

vec <- rbinom(length(modlog2$fitted.values),1,modlog2$fitted.values)

table(vec,CData_CDMX3$Vic_Rob_As)
y/(1+y)
0.38564676

modlog3 <- glm(Vic_Rob_As ~ Edad + Sit_Lab + Seg_Mun + Mas_Pat_Vil + Region + Nivel_Edu, family = "binomial",data=CData_CDMX3)
summary(modlog3)

vec1 <- rbinom(length(modlog3$fitted.values),1,modlog3$fitted.values)
table(vec1,CData_CDMX3$Vic_Rob_As)

## Jags

data <- list(
  y = CData_CDMX3$Vic_Rob_As,
  x1 = CData_CDMX3$Edad,
  x2 = CData_CDMX3$Sit_Lab,
  x3 = CData_CDMX3$Seg_Mun,
  x4 = CData_CDMX3$Mas_Pat_Vil,
  x5 = CData_CDMX3$Region,
  x6 = CData_CDMX3$Nivel_Edu,
  n = nrow(CData_CDMX3)
)

param <- c("alpha","beta1","beta2","beta3","beta4","beta5","beta6")

inits<-function(){list(
  "alpha" = rnorm(1),
  "beta1" = rnorm(1),
  "beta2" = rnorm(1),
  "beta3" = rnorm(1),
  "beta4" = rnorm(1),
  "beta5" = rnorm(1),
  "beta6" = rnorm(1),
)
}

fit <- jags.model("Modelo.bug", data, n.chains=3)
update(fit,1000)
sample <- coda.samples(fit,param,n.iter = 4000)
plot(sample)
summary(sample)