library(rjags)
library(tidyverse)
library(ROCR)
library(pROC)
library(car)

##Creamos base con respuesta 1 y 0
CData_CDMX3 <- CData_CDMX2 %>% 
  mutate(Vic_Rob_As = ifelse(Vic_Rob_As>0,1,0))
CData_CDMX3$Vic_Rob_As <- as_factor(CData_CDMX3$Vic_Rob_As)

## Primer modelo con todas las covariables
modlog1 <- glm(Vic_Rob_As ~ .,family = "binomial", data=CData_CDMX3)
summary(modlog1)


## Hallamos punto de corte gr?fica y te?ricamente
predictions1 <- prediction(modlog1$fitted.values,CData_CDMX3$Vic_Rob_As)

plot(unlist(performance(predictions1, "sens")@x.values), unlist(performance(predictions1, "sens")@y.values),
     type="l", lwd=2, ylab="Sensitivity", xlab="Punto de Corte")
par(new=TRUE)
plot(unlist(performance(predictions1, "spec")@x.values), unlist(performance(predictions1, "spec")@y.values),
     type="l", lwd=2, col='red', ylab="", xlab="")

rest1 <- abs(unlist(performance(predictions1, "sens")@y.values)-unlist(performance(predictions1, "spec")@y.values))
opt1 <- unlist(performance(predictions1, "sens")@x.values)[which(rest1==min(rest1))]


vif(modlog1)

#Matriz de Confusi?n
r <- ifelse(modlog1$fitted.values>=opt1,1,0)
table(r,CData_CDMX3$Vic_Rob_As)  ###62.32%


## Segundo modelo sin Importancia de la Seguridad
modlog2 <- glm(Vic_Rob_As ~ Edad + Seg_Mun + Mas_Pat_Vil, family = "binomial",data=CData_CDMX3)
summary(modlog2)

#Punto de Corte
predictions2 <- prediction(modlog2$fitted.values,CData_CDMX3$Vic_Rob_As)

plot(unlist(performance(predictions2, "sens")@x.values), unlist(performance(predictions2, "sens")@y.values),
     type="l", lwd=2, ylab="Sensitivity", xlab="Punto de Corte")
par(new=TRUE)
plot(unlist(performance(predictions2, "spec")@x.values), unlist(performance(predictions2, "spec")@y.values),
     type="l", lwd=2, col='red', ylab="", xlab="")

rest2 <- abs(unlist(performance(predictions2, "sens")@y.values)-unlist(performance(predictions2, "spec")@y.values))
opt2 <- unlist(performance(predictions2, "sens")@x.values)[which(rest2==min(rest2))]


vif(modlog2)

r <- ifelse(modlog2$fitted.values>=opt2,1,0)
table(r,CData_CDMX3$Vic_Rob_As)  ###62.02%


## Nivel Educativo
modlog3 <- glm(Vic_Rob_As ~ Edad + Sit_Lab + Seg_Mun + Mas_Pat_Vil + Region, family = "binomial",data=CData_CDMX3)
summary(modlog3)

predictions3 <- prediction(modlog3$fitted.values,CData_CDMX3$Vic_Rob_As)

plot(unlist(performance(predictions3, "sens")@x.values), unlist(performance(predictions3, "sens")@y.values),
     type="l", lwd=2, ylab="Sensitivity", xlab="Punto de Corte")
par(new=TRUE)
plot(unlist(performance(predictions3, "spec")@x.values), unlist(performance(predictions3, "spec")@y.values),
     type="l", lwd=2, col='red', ylab="", xlab="")

rest3 <- abs(unlist(performance(predictions3, "sens")@y.values)-unlist(performance(predictions3, "spec")@y.values))
opt3 <- unlist(performance(predictions3, "sens")@x.values)[which(rest3==min(rest3))]


vif(modlog3)

r3 <- ifelse(modlog2$fitted.values>=opt3,1,0)
table(r3,CData_CDMX3$Vic_Rob_As)  ##62.26%


y<-exp(-.297618+-0.023261*36+0.522345+0.147011)

vec <- rbinom(length(modlog2$fitted.values),1,modlog2$fitted.values)

table(vec,CData_CDMX3$Vic_Rob_As)
y/(1+y)
0.38564676

## log log  clog log

modlog3 <- glm(Vic_Rob_As ~ Edad + Sit_Lab + Seg_Mun + Mas_Pat_Vil + Region + Nivel_Edu, family = "binomial",data=CData_CDMX3)
summary(modlog3)

vec1 <- rbinom(length(modlog3$fitted.values),1,modlog3$fitted.values)
table(vec1,CData_CDMX3$Vic_Rob_As)


outlierTest(modlog2)

CData_CDMX4 <- CData_CDMX3[c(-824,-755),]

modlog4 <- glm(Vic_Rob_As ~ Edad + Sit_Lab + Seg_Mun + Mas_Pat_Vil + Nivel_Edu + Region, family = "binomial",data=CData_CDMX4)
summary(modlog4)

#Punto de Corte
predictions4 <- prediction(modlog4$fitted.values,CData_CDMX4$Vic_Rob_As)

plot(unlist(performance(predictions4, "sens")@x.values), unlist(performance(predictions4, "sens")@y.values),
     type="l", lwd=2, ylab="Sensitivity", xlab="Punto de Corte")
par(new=TRUE)
plot(unlist(performance(predictions4, "spec")@x.values), unlist(performance(predictions4, "spec")@y.values),
     type="l", lwd=2, col='red', ylab="", xlab="")

rest4 <- abs(unlist(performance(predictions4, "sens")@y.values)-unlist(performance(predictions4, "spec")@y.values))
opt4 <- unlist(performance(predictions4, "sens")@x.values)[which(rest4==min(rest4))]


vif(modlog4)

r <- ifelse(modlog2$fitted.values>=opt2,1,0)
table(r,CData_CDMX3$Vic_Rob_As)  ###62.02%

outlierTest(modlog4)


## Jags
attach(CData_CDMX3)
# data <- list(
#   y = as.numeric(Vic_Rob_As)-1,
#   x1 = Edad,
#   x3 = as.numeric(Seg_Mun)-1,
#   x4 = as.numeric(Mas_Pat_Vil)-1,
#   n = nrow(CData_CDMX3)
# )

# data <- list(
#   y = as.numeric(Vic_Rob_As)-1,
#   x1 = as.numeric(Nivel_Edu == "Centro Poniente"),
#   x2 = as.numeric(Nivel_Edu == "Sur"),
#   x3 = as.numeric(Nivel_Edu == "Norte"),
#   x4 = as.numeric(Nivel_Edu == "Oriente"),
#   n = nrow(CData_CDMX3)
# )

data <- list(
  y = as.numeric(Vic_Rob_As)-1,
  X = Imp_Seg,
  n = nrow(CData_CDMX3)
)


param <- c("alpha","beta")

inits<-function(){list(
  "alpha" = rnorm(1)
)
}

fit <- jags.model("Modelo.bug", data, inits, n.chains=3)
update(fit,5000)
sample <- coda.samples(fit,param,n.iter = 4000)
plot(sample)
summary(sample)

#Comparar
modlog4 <- glm(Vic_Rob_As ~ Region, family = "binomial",data=CData_CDMX3)
summary(modlog4)


  
data <- list(
  y = as.numeric(Vic_Rob_As)-1,
  X1 = Edad,
  X2 = as.numeric(Region),
  n = nrow(CData_CDMX3)
)


param <- c("alpha","beta1", "beta2")

inits<-function(){list(
  "alpha" = rnorm(1),
  "beta1" = rnorm(1)
)
}

fit <- jags.model("Modelo.bug", data, inits, n.chains=3)
update(fit,5000)
sample <- coda.samples(fit,param,n.iter = 1000)
plot(sample)
summary(sample)

glm(Vic_Rob_As ~ Edad + Region, family = "binomial",data=CData_CDMX3) %>% 
  summary()





data <- list(
  y = as.numeric(Vic_Rob_As)-1,
  X1 = Edad,
  X2 = as.numeric(Sit_Lab),
  X3 = Seg_Mun,
  X4 = Mas_Pat_Vil,
  X5 = as.numeric(Nivel_Edu),
  X6 = as.numeric(Region),
  n = nrow(CData_CDMX3)
)


param <- c("alpha","beta1", "beta2", "beta3","beta4","beta5","beta6")

inits<-function(){list(
  "alpha" = rnorm(1),
  "beta1" = rnorm(1)
)
}

fit <- jags.model("Modelo.bug", data, inits, n.chains=3)
update(fit,1000)
sample <- coda.samples(fit,param,n.iter = 1000)
plot(sample)
summary(sample)

glm(Vic_Rob_As ~ Edad + Region, family = "binomial",data=CData_CDMX3) %>% 
  summary()

modlog4 <- glm(Vic_Rob_As ~ Edad + Sit_Lab + Seg_Mun + Mas_Pat_Vil + Nivel_Edu + Region, family = "binomial",data=CData_CDMX3)
summary(modlog4)
