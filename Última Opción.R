library(rjags)
library(tidyverse)
library(ROCR)
library(pROC)
library(car)

load("BD_CDMX2.Rda")
##Creamos base con respuesta 1 y 0
CData_CDMX3 <- CData_CDMX2 %>% 
  mutate(Vic_Rob_As = ifelse(Vic_Rob_As>0,1,0))
CData_CDMX3$Vic_Rob_As <- as_factor(CData_CDMX3$Vic_Rob_As)

## Primer modelo con todas las covariables
modlog1 <- glm(Vic_Rob_As ~ .,family = "binomial", data=CData_CDMX3)
summary(modlog1)


## Hallamos punto de corte gráfica y teóricamente
predictions1 <- prediction(modlog1$fitted.values,CData_CDMX3$Vic_Rob_As)

plot(unlist(performance(predictions1, "sens")@x.values), unlist(performance(predictions1, "sens")@y.values),
     type="l", lwd=2, ylab="Sensitivity", xlab="Punto de Corte")
par(new=TRUE)
plot(unlist(performance(predictions1, "spec")@x.values), unlist(performance(predictions1, "spec")@y.values),
     type="l", lwd=2, col='red', ylab="", xlab="")

rest1 <- abs(unlist(performance(predictions1, "sens")@y.values)-unlist(performance(predictions1, "spec")@y.values))
opt1 <- unlist(performance(predictions1, "sens")@x.values)[which(rest1==min(rest1))]

##Checamos multicolinealidad
vif(modlog1)

#Matriz de Confusión
r1 <- ifelse(modlog1$fitted.values>=opt1,1,0)
table(r1,CData_CDMX3$Vic_Rob_As)  ###62.32%


## Modelo Sin Importancia de la Seguridad
modlog2 <- glm(Vic_Rob_As ~ Edad + Sit_Lab + Seg_Mun + Mas_Pat_Vil + Nivel_Edu + Region, family = "binomial",data=CData_CDMX3)
smod2 <- summary(modlog2)
fittedfrec <- modlog2$fitted.values

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

r2 <- ifelse(modlog2$fitted.values>=opt2,1,0)
table(r2,CData_CDMX3$Vic_Rob_As)  ###61.88%

##Distancias de Cook
cooks.distance(modlog2)
plot(modlog2,4)

outlierTest(modlog2)

set.seed(3)
vec2 <- rbinom(length(modlog2$fitted.values),1,modlog2$fitted.values)
table(vec2,CData_CDMX3$Vic_Rob_As) ###69.82%


##Preparamos datos para ejemplos
betas1<-smod2$coefficients[,1]
CData_CDMX4 <- CData_CDMX3 %>%
  mutate(Empleado = ifelse(Sit_Lab == "Empleado",1,0), SinOcup = ifelse(Sit_Lab == "Sin Ocupación",1,0),
         EstDom = ifelse(Sit_Lab == "Estudiantes y Domésticos",1,0), CP = ifelse(Region == "Centro Poniente",1,0),
         S = ifelse(Region == "Sur",1,0), N = ifelse(Region == "Norte",1,0), O = ifelse(Region == "Oriente",1,0),
         NB = ifelse(Nivel_Edu == "No Básica",1,0), Sec =ifelse(Nivel_Edu == "Secundaria",1,0),
         MS = ifelse(Nivel_Edu == "Medio Superior",1,0), Sup = ifelse(Nivel_Edu == "Superior",1,0),
         Pos = ifelse(Nivel_Edu == "Posgrado",1,0), Seg = ifelse(Seg_Mun == 1,1,0), NoSeg = ifelse(Seg_Mun ==2,1,0),
         Mas = ifelse(Mas_Pat_Vil == 1,1,0), NoMas = ifelse(Mas_Pat_Vil == 2,1,0), Alpha = 1) %>%
  select(Alpha, Edad, Empleado, SinOcup, EstDom, Seg, NoSeg, Mas, NoMas, NB, Sec, MS, Sup, Pos, CP, S, N, O)

CData_CDMX6 <- CData_CDMX4 %>% 
  select(-Empleado,-Seg,-Mas,-NB,-CP)


Ejemplo2_1 <- CData_CDMX6 %>% 
  filter(Edad == 22, EstDom == 1, Sup == 1, NoSeg ==1, NoMas ==0, S==0, N==0, O ==0)
Ejemplo4_1 <- CData_CDMX6 %>% 
  filter(Edad == 22, EstDom == 1, Sup == 1, NoSeg ==1, NoMas ==1, S==0, N==0, O ==0)
Ejemplo6_1 <- CData_CDMX6 %>% 
  filter(Edad == 22, EstDom == 1, Sup == 1, NoSeg ==1, NoMas ==0, S==0, N==1, O ==0)
Ejemplo8_1 <- CData_CDMX6 %>% 
  filter(Edad == 22, EstDom == 1, Sup == 1, NoSeg ==1, NoMas ==1, S==0, N==1, O ==0)
Ejemplo9_1 <- CData_CDMX6 %>% 
  filter(Edad == 22, EstDom == 1, Sup == 1, NoSeg ==0, NoMas ==0, S==1, N==0, O ==0)
Ejemplo10_1 <- CData_CDMX6 %>% 
  filter(Edad == 22, EstDom == 1, Sup == 1, NoSeg ==1, NoMas ==0, S==1, N==0, O ==0)
Ejemplo12_1 <- CData_CDMX6 %>% 
  filter(Edad == 22, EstDom == 1, Sup == 1, NoSeg ==1, NoMas ==1, S==1, N==0, O ==0)
Ejemplo14_1 <- CData_CDMX6 %>% 
  filter(Edad == 22, EstDom == 1, Sup == 1, NoSeg ==1, NoMas ==0, S==0, N==0, O ==1)
Ejemplo16_1 <- CData_CDMX6 %>% 
  filter(Edad == 22, EstDom == 1, Sup == 1, NoSeg ==1, NoMas ==1, S==0, N==0, O ==1)

##Ejemplo
Ejemplo2 <- CData_CDMX3 %>% 
  filter(Edad == 22, Seg_Mun == 2, Mas_Pat_Vil == 1, Region == "Centro Poniente", Nivel_Edu == "Superior",Sit_Lab == "Estudiantes y Domésticos")
Ejemplo4 <- CData_CDMX3 %>% 
  filter(Edad == 22, Seg_Mun == 2, Mas_Pat_Vil == 2, Region == "Centro Poniente", Nivel_Edu == "Superior",Sit_Lab == "Estudiantes y Domésticos")
Ejemplo6 <- CData_CDMX3 %>% 
  filter(Edad == 22, Seg_Mun == 2, Mas_Pat_Vil == 1, Region == "Norte", Nivel_Edu == "Superior",Sit_Lab == "Estudiantes y Domésticos")
Ejemplo8 <- CData_CDMX3 %>% 
  filter(Edad == 22, Seg_Mun == 2, Mas_Pat_Vil == 2, Region == "Norte", Nivel_Edu == "Superior",Sit_Lab == "Estudiantes y Domésticos")
Ejemplo9 <- CData_CDMX3 %>% 
  filter(Edad == 22, Seg_Mun == 1, Mas_Pat_Vil == 1, Region == "Sur", Nivel_Edu == "Superior",Sit_Lab == "Estudiantes y Domésticos")
Ejemplo10 <- CData_CDMX3 %>% 
  filter(Edad == 22, Seg_Mun == 2, Mas_Pat_Vil == 1, Region == "Sur", Nivel_Edu == "Superior",Sit_Lab == "Estudiantes y Domésticos")
Ejemplo12 <- CData_CDMX3 %>% 
  filter(Edad == 22, Seg_Mun == 2, Mas_Pat_Vil == 2, Region == "Sur", Nivel_Edu == "Superior",Sit_Lab == "Estudiantes y Domésticos")
Ejemplo14 <- CData_CDMX3 %>% 
  filter(Edad == 22, Seg_Mun == 2, Mas_Pat_Vil == 1, Region == "Oriente", Nivel_Edu == "Superior",Sit_Lab == "Estudiantes y Domésticos")
Ejemplo16 <- CData_CDMX3 %>% 
  filter(Edad == 22, Seg_Mun == 2, Mas_Pat_Vil == 2, Region == "Oriente", Nivel_Edu == "Superior",Sit_Lab == "Estudiantes y Domésticos")

##Obtenemos los valores ajustados de nuestra probabilidad
Tabla <- rbind(Ejemplo2_1[1,],Ejemplo4_1[1,],Ejemplo6_1[1,],Ejemplo8_1[1,],Ejemplo9_1[1,],Ejemplo10_1[1,],Ejemplo12_1[1,],Ejemplo14_1[1,],
      Ejemplo16_1[1,])
p_i <- rep(0,9)
for(i in 1:9){
  exponente <- sum(betas1 * Tabla[i,])
  p_i[i] <- 1/(1+exp(-exponente))
}

##Realizamos nuestra simulación
n_i <-c(1,3,3,5,2,6,1,5,1)
Sim <- rbinom(9,n_i,p_i)

##Graficamos los simulados y los reales.
Reales <- c(rep(0,20),rep(1,7))
Simu <- c(27-sum(Sim),sum(Sim))

ggplot() +
  geom_histogram(aes(x=Reales),binwidth = 1, col = "red", fill = "orange") + 
  geom_point(aes(x=c(0,1) , y=Simu)) + geom_point(aes(x=c(0,1) , y=Sim))


## Código JAGS
attach(CData_CDMX3)
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
sample <- coda.samples(fit,param,n.iter = 2000,thin =2)


load("sampleCC.Rdata")
plot(sample)
ssample <- summary(sample)

betas <- ssample[[1]][,1]


Ejemplo2_1 <- CData_CDMX4 %>% 
  filter(Edad == 22, EstDom == 1, Sup == 1, NoSeg ==1, NoMas ==0, S==0, N==0, O ==0)
Ejemplo4_1 <- CData_CDMX4 %>% 
  filter(Edad == 22, EstDom == 1, Sup == 1, NoSeg ==1, NoMas ==1, S==0, N==0, O ==0)
Ejemplo6_1 <- CData_CDMX4 %>% 
  filter(Edad == 22, EstDom == 1, Sup == 1, NoSeg ==1, NoMas ==0, S==0, N==1, O ==0)
Ejemplo8_1 <- CData_CDMX4 %>% 
  filter(Edad == 22, EstDom == 1, Sup == 1, NoSeg ==1, NoMas ==1, S==0, N==1, O ==0)
Ejemplo9_1 <- CData_CDMX4 %>% 
  filter(Edad == 22, EstDom == 1, Sup == 1, NoSeg ==0, NoMas ==0, S==1, N==0, O ==0)
Ejemplo10_1 <- CData_CDMX4 %>% 
  filter(Edad == 22, EstDom == 1, Sup == 1, NoSeg ==1, NoMas ==0, S==1, N==0, O ==0)
Ejemplo12_1 <- CData_CDMX4 %>% 
  filter(Edad == 22, EstDom == 1, Sup == 1, NoSeg ==1, NoMas ==1, S==1, N==0, O ==0)
Ejemplo14_1 <- CData_CDMX4 %>% 
  filter(Edad == 22, EstDom == 1, Sup == 1, NoSeg ==1, NoMas ==0, S==0, N==0, O ==1)
Ejemplo16_1 <- CData_CDMX4 %>% 
  filter(Edad == 22, EstDom == 1, Sup == 1, NoSeg ==1, NoMas ==1, S==0, N==0, O ==1)

##Obtenemos los valores ajustados de nuestra probabilidad
Tabla1 <- rbind(Ejemplo2_1[1,],Ejemplo4_1[1,],Ejemplo6_1[1,],Ejemplo8_1[1,],Ejemplo9_1[1,],Ejemplo10_1[1,],Ejemplo12_1[1,],Ejemplo14_1[1,],
               Ejemplo16_1[1,])
p_i1 <- rep(0,9)
for(i in 1:9){
  exponente <- sum(betas1 * Tabla[i,])
  p_i1[i] <- 1/(1+exp(-exponente))
}

set.seed(17)
##Realizamos nuestra simulación
Sim1 <- rbinom(9,n_i,p_i1)

##Graficamos los simulados y los reales.
Reales <- c(rep(0,20),rep(1,7))
Simu1 <- c(27-sum(Sim1),sum(Sim1))

ggplot() +
  geom_histogram(aes(x=Reales),binwidth = 1, col = "black", fill = "#25B7BC") + 
  geom_point(aes(x=c(0,1) , y=Simu), col = "#B225BC", size = 2.8) + geom_point(aes(x=c(0,1),y=Simu1), col ="#2528BC",size=2.8) +
  theme_light() + ylab("Casos") + xlab("Robo (1) o No (0)")


##Comparamos lo obtenido del clásico al bayesiano
fittedfrec <- modlog2$fitted.values

#Conseguimos nuestras probas para cada individuo
fitted <- rep(0,5417)
for(i in 1:5417){
  expo <- sum(betas*CData_CDMX4[i,])
  p <- 1/(1+exp(-expo))
  fitted[i] <- p
}


#Obtenemos el error entre ambas
error <- (fitted - fittedfrec)^2
sum(error)

#### Modelo Bayesiano Resultados:

predictions <- prediction(fitted,CData_CDMX3$Vic_Rob_As)

plot(unlist(performance(predictions, "sens")@x.values), unlist(performance(predictions, "sens")@y.values),
     type="l", lwd=2, ylab="Sensitivity", xlab="Punto de Corte")
par(new=TRUE)
plot(unlist(performance(predictions, "spec")@x.values), unlist(performance(predictions, "spec")@y.values),
     type="l", lwd=2, col='red', ylab="", xlab="")

rest <- abs(unlist(performance(predictions, "sens")@y.values)-unlist(performance(predictions, "spec")@y.values))
opt <- unlist(performance(predictions, "sens")@x.values)[which(rest==min(rest))]

r <- ifelse(fitted>=opt,1,0)
table(r,CData_CDMX3$Vic_Rob_As) ##61.86%


vec2 <- rbinom(length(fitted),1,fitted)
table(vec2,CData_CDMX3$Vic_Rob_As) ###70.24%