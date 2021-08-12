load("BD_CDMX5.Rda")


modlog3 <- glm(Vic_Rob_As ~ Edad + Sit_Lab + Seg_Mun + Mas_Pat_Vil + Nivel_Edu + Region, family = "binomial",data=CData_CDMX5)
smod3 <- summary(modlog3)

#Punto de Corte
predictions3 <- prediction(modlog3$fitted.values,CData_CDMX5$Vic_Rob_As)

plot(unlist(performance(predictions3, "sens")@x.values), unlist(performance(predictions3, "sens")@y.values),
     type="l", lwd=2, ylab="Sensitivity", xlab="Punto de Corte")
par(new=TRUE)
plot(unlist(performance(predictions3, "spec")@x.values), unlist(performance(predictions3, "spec")@y.values),
     type="l", lwd=2, col='red', ylab="", xlab="")

rest3 <- abs(unlist(performance(predictions3, "sens")@y.values)-unlist(performance(predictions3, "spec")@y.values))
opt3 <- unlist(performance(predictions3, "sens")@x.values)[which(rest3==min(rest3))]

##Checamos Multicolinealidad
vif(modlog3)

##Matriz de Confusión
r3 <- ifelse(modlog3$fitted.values>=opt3,1,0)
table(r3,CData_CDMX5$Vic_Rob_As)  ###61.84%

##Distancia de Cook
cooks.distance(modlog3)
plot(modlog3,4)

vec3 <- rbinom(length(modlog3$fitted.values),1,modlog3$fitted.values)
table(vec3,CData_CDMX5$Vic_Rob_As) ###69.71%


##Preparamos datos para ejemplos
betas1<-smod3$coefficients[,1]
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
Ejemplo2 <- CData_CDMX5 %>% 
  filter(Edad == 22, Seg_Mun == 2, Mas_Pat_Vil == 1, Region == "Centro Poniente", Nivel_Edu == "Superior",Sit_Lab == "Estudiantes y Domésticos")
Ejemplo4 <- CData_CDMX5 %>% 
  filter(Edad == 22, Seg_Mun == 2, Mas_Pat_Vil == 2, Region == "Centro Poniente", Nivel_Edu == "Superior",Sit_Lab == "Estudiantes y Domésticos")
Ejemplo6 <- CData_CDMX5 %>% 
  filter(Edad == 22, Seg_Mun == 2, Mas_Pat_Vil == 1, Region == "Norte", Nivel_Edu == "Superior",Sit_Lab == "Estudiantes y Domésticos")
Ejemplo8 <- CData_CDMX5 %>% 
  filter(Edad == 22, Seg_Mun == 2, Mas_Pat_Vil == 2, Region == "Norte", Nivel_Edu == "Superior",Sit_Lab == "Estudiantes y Domésticos")
Ejemplo9 <- CData_CDMX5 %>% 
  filter(Edad == 22, Seg_Mun == 1, Mas_Pat_Vil == 1, Region == "Sur", Nivel_Edu == "Superior",Sit_Lab == "Estudiantes y Domésticos")
Ejemplo10 <- CData_CDMX5 %>% 
  filter(Edad == 22, Seg_Mun == 2, Mas_Pat_Vil == 1, Region == "Sur", Nivel_Edu == "Superior",Sit_Lab == "Estudiantes y Domésticos")
Ejemplo12 <- CData_CDMX5 %>% 
  filter(Edad == 22, Seg_Mun == 2, Mas_Pat_Vil == 2, Region == "Sur", Nivel_Edu == "Superior",Sit_Lab == "Estudiantes y Domésticos")
Ejemplo14 <- CData_CDMX5 %>% 
  filter(Edad == 22, Seg_Mun == 2, Mas_Pat_Vil == 1, Region == "Oriente", Nivel_Edu == "Superior",Sit_Lab == "Estudiantes y Domésticos")
Ejemplo16 <- CData_CDMX5 %>% 
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
set.seed(5)
Sim <- rbinom(9,n_i,p_i)

##Graficamos los simulados y los reales.
Reales <- c(rep(0,20),rep(1,7))
Simu <- c(27-sum(Sim),sum(Sim))

ggplot() +
  geom_histogram(aes(x=Reales),binwidth = 1, col = "red", fill = "orange") + geom_point(aes(x=c(0,1) , y=Simu))


## Código JAGS
attach(CData_CDMX5)
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
update(fit,2000)
sample <- coda.samples(fit,param,n.iter = 2500,thin =3)

plot(sample)
ssample <- summary(sample)

betas <- ssample[[1]][,1]



##Comparamos lo obtenido del clásico al bayesiano
fittedfrec <- modlog3$fitted.values

#Conseguimos nuestras probas para cada individuo
fitted <- rep(0,5418)
for(i in 1:5418){
  expo <- sum(betas*CData_CDMX4[i,])
  p <- 1/(1+exp(-expo))
  fitted[i] <- p
}


#Obtenemos el error entre ambas
error <- (fitted - fittedfrec)^2
sum(error)