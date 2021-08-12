library(tidyverse)
library(corrplot)
library(polycor)
library(glm2)
library(pscl)
library(boot)

setwd("C:/Users/edson/Documents/BayesProject")
load(file="BD_CDMX_Vic.Rda")

##Unimos categorías similares
for(i in 1:nrow(Data_CDMX)){
  if(Data_CDMX$Pandill[i]==3){
    Data_CDMX$Pandill[i]=2
  }
  if(Data_CDMX$Robos[i]==3){
    Data_CDMX$Robos[i]=2
  }
  if(Data_CDMX$Del_Esc[i]==3){
    Data_CDMX$Del_Esc[i]=2
  }
  if(Data_CDMX$Mas_Op_Del[i]==2){
    Data_CDMX$Mas_Op_Del[i]=9
  }
  if(Data_CDMX$Mas_Op_Del[i]==3){
    Data_CDMX$Mas_Op_Del[i]=2
  }
  if(Data_CDMX$Mas_Pat_Vil[i]==2){
    Data_CDMX$Mas_Pat_Vil[i]=9
  }
  if(Data_CDMX$Mas_Pat_Vil[i]==3){
    Data_CDMX$Mas_Pat_Vil[i]=2
  }
}

##Se eliminan datos irrelevantes
Data_CDMX <- Data_CDMX %>% 
  filter(Edad!=98) %>% 
  select(-Nom_Ent,-Vic_Sex)

##Creamos un respaldo con los factores para crear la correlación
Data_CDMX2 <- Data_CDMX %>% 
  filter(Data_CDMX$Nom_Mun != 9, Data_CDMX$Sexo != 9,Data_CDMX$Niv_Edu != 9,Data_CDMX$Imp_Seg != 9,Data_CDMX$Seg_Loc!= 9,
          Data_CDMX$Seg_Mun!= 9,Data_CDMX$Alum!= 9,Data_CDMX$Agua!= 9,Data_CDMX$Pandill!= 9,Data_CDMX$Robos!= 9,Data_CDMX$Del_Esc!= 9,
          Data_CDMX$Mas_Op_Del!= 9, Data_CDMX$Mas_Pat_Vil!= 9, Data_CDMX$Vehic!= 9)

Data_CDMX2$Nom_Mun <- as_factor(Data_CDMX2$Nom_Mun)
Data_CDMX2$Sexo <- as_factor(Data_CDMX2$Sexo)
Data_CDMX2$Niv_Edu <- as_factor(Data_CDMX2$Niv_Edu)
Data_CDMX2$Sit_Lab_Act <- as_factor(Data_CDMX2$Sit_Lab_Act)
Data_CDMX2$Pos_OCup <- as_factor(Data_CDMX2$Pos_OCup)
Data_CDMX2$Imp_Seg <- as_factor(Data_CDMX2$Imp_Seg)
Data_CDMX2$Seg_Loc <- as_factor(Data_CDMX2$Seg_Loc)
Data_CDMX2$Seg_Mun <- as_factor(Data_CDMX2$Seg_Mun)
Data_CDMX2$Alum <- as_factor(Data_CDMX2$Alum)
Data_CDMX2$Agua <- as_factor(Data_CDMX2$Agua)
Data_CDMX2$Pandill <- as_factor(Data_CDMX2$Pandill)
Data_CDMX2$Robos <- as_factor(Data_CDMX2$Robos)
Data_CDMX2$Del_Esc <- as_factor(Data_CDMX2$Del_Esc)
Data_CDMX2$Mas_Op_Del <- as_factor(Data_CDMX2$Mas_Op_Del)
Data_CDMX2$Mas_Pat_Vil <- as_factor(Data_CDMX2$Mas_Pat_Vil)
Data_CDMX2$Vehic <- as_factor(Data_CDMX2$Vehic)

sum_ini <- summary(Data_CDMX2)


##Separamos a las personas que sufrieron siniestros y las que no.(Paso 1 para la imputación)
d1 <- Data_CDMX %>% filter(Vic_Rob_As != 0)
d2 <- Data_CDMX %>% filter(Vic_Rob_As == 0)

#d1. Obtenemos proporciones de gente asaltada en todas las categorías que no fueran desconocidas
{
  sum(d1$Seg_Loc==1) #262
  sum(d1$Seg_Loc==2) #815/1077
  
  sum(d1$Seg_Mun==1) #142
  sum(d1$Seg_Mun==2) #937/1079
  
  sum(d1$Alum==1) #635
  sum(d1$Alum==2) #447/1082
  
  sum(d1$Agua==1) #576
  sum(d1$Agua==2) #506/1082
  
  sum(d1$Pandill==1) #468
  sum(d1$Pandill==2) #595/1063
  
  sum(d1$Robos==1) #937
  sum(d1$Robos==2) #142/1079
  
  sum(d1$Del_Esc==1) #537
  sum(d1$Del_Esc==2) #351/888
  
  sum(d1$Mas_Op_Del==1) #301
  sum(d1$Mas_Op_Del==2) #504/805
  
  sum(d1$Mas_Pat_Vil==1) #365
  sum(d1$Mas_Pat_Vil==2) #518/883
  
  sum(d1$Vehic==1) #425
  sum(d1$Vehic==2) #656/1081
}

#d2. Obtenemos proporciones de gente no asaltada en todas las categorías que no fueran desconocidas
{
  sum(d2$Seg_Loc==1) #1517
  sum(d2$Seg_Loc==2) #2802/4319
  
  sum(d2$Seg_Mun==1) #923
  sum(d2$Seg_Mun==2) #3387/4310
  
  sum(d2$Alum==1) #2135
  sum(d2$Alum==2) #2190/4325
  
  sum(d2$Agua==1) #2030
  sum(d2$Agua==2) #2299/4329
  
  sum(d2$Pandill==1) #1337
  sum(d2$Pandill==2) #2943/4280
  
  sum(d2$Robos==1) #3228
  sum(d2$Robos==2) #1068/4296
  
  sum(d2$Del_Esc==1) #1540
  sum(d2$Del_Esc==2) #1941/3481
  
  sum(d2$Mas_Op_Del==1) #998
  sum(d2$Mas_Op_Del==2) #2226/3214
  
  sum(d2$Mas_Pat_Vil==1) #1723
  sum(d2$Mas_Pat_Vil==2) #1934/3657
  
  sum(d2$Vehic==1) #1833
  sum(d2$Vehic==2) #2496/4329
}


set.seed(3)

##Realizamos la imputación
for(i in 1:nrow(d1)){
  d1$Seg_Loc[i] <- ifelse(d1$Seg_Loc[i]==9,rbernoulli(1,815/1077)+1,d1$Seg_Loc[i])
  d1$Seg_Mun[i] <- ifelse(d1$Seg_Mun[i]==9,rbernoulli(1,937/1079)+1, d1$Seg_Mun[i])
  d1$Alum[i] <- ifelse(d1$Alum[i]==9,rbernoulli(1,447/1082)+1,d1$Alum[i])
  d1$Agua[i] <- ifelse(d1$Agua[i]==9,rbernoulli(1,506/1082)+1,d1$Agua[i])
  d1$Pandill[i] <- ifelse(d1$Pandill[i]==9,rbernoulli(1,595/1063)+1,d1$Pandill[i])
  d1$Robos[i] <- ifelse(d1$Robos[i]==9,rbernoulli(1,142/1079)+1,d1$Robos[i])
  d1$Del_Esc[i] <- ifelse(d1$Del_Esc[i]==9,rbernoulli(1,351/888)+1,d1$Del_Esc[i])
  d1$Mas_Op_Del[i] <- ifelse(d1$Mas_Op_Del[i]==9,rbernoulli(1,504/805)+1,d1$Mas_Op_Del[i])
  d1$Mas_Pat_Vil[i] <- ifelse(d1$Mas_Pat_Vil[i]==9,rbernoulli(1,518/883)+1,d1$Mas_Pat_Vil[i])
  d1$Vehic[i] <- ifelse(d1$Vehic[i]==9,rbernoulli(1,656/1081)+1,d1$Vehic[i])
}

for (i in 1:nrow(d2)){
  d2$Seg_Loc[i] <- ifelse(d2$Seg_Loc[i]==9,rbernoulli(1,2802/4319)+1,d2$Seg_Loc[i])
  d2$Seg_Mun[i] <- ifelse(d2$Seg_Mun[i]==9,rbernoulli(1,3387/4310)+1,d2$Seg_Mun[i])
  d2$Alum[i] <- ifelse(d2$Alum[i]==9,rbernoulli(1,2190/4325)+1,d2$Alum[i])
  d2$Agua[i] <- ifelse(d2$Agua[i]==9,rbernoulli(1,2299/4329)+1,d2$Agua[i])
  d2$Pandill[i] <- ifelse(d2$Pandill[i]==9,rbernoulli(1,2943/4280)+1,d2$Pandill[i])
  d2$Robos[i] <- ifelse(d2$Robos[i]==9,rbernoulli(1,1068/4296)+1,d2$Robos[i])
  d2$Del_Esc[i] <- ifelse(d2$Del_Esc[i]==9,rbernoulli(1,1941/3481)+1,d2$Del_Esc[i])
  d2$Mas_Op_Del[i] <- ifelse(d2$Mas_Op_Del[i]==9,rbernoulli(1,2226/3214)+1,d2$Mas_Op_Del[i])
  d2$Mas_Pat_Vil[i] <- ifelse(d2$Mas_Pat_Vil[i]==9,rbernoulli(1,1934/3657)+1,d2$Mas_Pat_Vil[i])
  d2$Vehic[i] <- ifelse(d2$Vehic[i]==9,rbernoulli(1,2496/4329)+1,d2$Vehic[i])
}

##Juntamos las 2 tablas
CData_CDMX <- rbind(d1,d2)

##Categorizamos con factores las columnas correspondientes
CData_CDMX$Nom_Mun <- as_factor(CData_CDMX$Nom_Mun)
CData_CDMX$Sexo <- as_factor(CData_CDMX$Sexo)
CData_CDMX$Niv_Edu <- as_factor(CData_CDMX$Niv_Edu)
CData_CDMX$Sit_Lab_Act <- as_factor(CData_CDMX$Sit_Lab_Act)
CData_CDMX$Pos_OCup <- as_factor(CData_CDMX$Pos_OCup)
CData_CDMX$Imp_Seg <- as_factor(CData_CDMX$Imp_Seg)
CData_CDMX$Seg_Loc <- as_factor(CData_CDMX$Seg_Loc)
CData_CDMX$Seg_Mun <- as_factor(CData_CDMX$Seg_Mun)
CData_CDMX$Alum <- as_factor(CData_CDMX$Alum)
CData_CDMX$Agua <- as_factor(CData_CDMX$Agua)
CData_CDMX$Pandill <- as_factor(CData_CDMX$Pandill)
CData_CDMX$Robos <- as_factor(CData_CDMX$Robos)
CData_CDMX$Del_Esc <- as_factor(CData_CDMX$Del_Esc)
CData_CDMX$Mas_Op_Del <- as_factor(CData_CDMX$Mas_Op_Del)
CData_CDMX$Mas_Pat_Vil <- as_factor(CData_CDMX$Mas_Pat_Vil)
CData_CDMX$Vehic <- as_factor(CData_CDMX$Vehic)

##Checamos que las correlaciones no cambiaran
au <- hetcor(Data_CDMX2)
au2 <- au$correlations
corrplot(au2)

DF1<-Data_CDMX %>% 
  select(Vic_Rob_As,Seg_Loc) %>% 
  filter(Seg_Loc != 9)
DF1$Seg_Loc <- as_factor(DF1$Seg_Loc)

au <- hetcor(Data_CDMX2)
au2 <- au$correlations
corrplot(au2)


DF2<-Data_CDMX %>% 
  select(Vic_Rob_As,Seg_Mun) %>% 
  filter(Seg_Mun != 9)
DF2$Seg_Mun <- as_factor(DF2$Seg_Mun)

au <- hetcor(DF2)
au2 <- au$correlations
corrplot(au2)

DF3<-Data_CDMX %>% 
  select(Vic_Rob_As,Alum) %>% 
  filter(Alum != 9)
DF3$Alum <- as_factor(DF3$Alum)

au <- hetcor(DF3)
au2 <- au$correlations
corrplot(au2)

DF4<-Data_CDMX %>% 
  select(Vic_Rob_As,Agua) %>% 
  filter(Agua != 9)
DF4$Agua <- as_factor(DF4$Agua)

au <- hetcor(DF4)
au2 <- au$correlations
corrplot(au2)

DF5<-Data_CDMX %>% 
  select(Vic_Rob_As,Pandill) %>% 
  filter(Pandill != 9)
DF5$Pandill <- as_factor(DF5$Pandill)

au <- hetcor(DF5)
au2 <- au$correlations
corrplot(au2)

DF6<-Data_CDMX %>% 
  select(Vic_Rob_As,Robos) %>% 
  filter(Robos != 9)
DF6$Robos <- as_factor(DF6$Robos)

au <- hetcor(DF6)
au2 <- au$correlations
corrplot(au2)

DF7<-Data_CDMX %>% 
  select(Vic_Rob_As, Del_Esc) %>% 
  filter(Del_Esc != 9)
DF7$Del_Esc <- as_factor(DF7$Del_Esc)

au <- hetcor(DF7)
au2 <- au$correlations
corrplot(au2)


DF8<-Data_CDMX %>% 
  select(Vic_Rob_As, Mas_Op_Del) %>% 
  filter(Mas_Op_Del != 9)
DF8$Mas_Op_Del <- as_factor(DF8$Mas_Op_Del)

au <- hetcor(DF8)
au2 <- au$correlations
corrplot(au2)


DF9<-Data_CDMX %>% 
  select(Vic_Rob_As, Mas_Pat_Vil) %>% 
  filter(Mas_Pat_Vil != 9)
DF9$Mas_Pat_Vil <- as_factor(DF9$Mas_Pat_Vil)

au <- hetcor(DF9)
au2 <- au$correlations
corrplot(au2)

DF10<-Data_CDMX %>% 
  select(Vic_Rob_As, Vehic) %>% 
  filter(Vehic != 9)
DF10$Vehic <- as_factor(DF10$Vehic)

au <- hetcor(DF10)
au2 <- au$correlations
corrplot(au2)

##Sacamos la correlación general
sum_fin <- summary(CData_CDMX)
au_1 <- hetcor(CData_CDMX)
au2_1 <- au_1$correlations
corrplot(au2_1)
  
##Quitamos variables que se representaran lo mismo en el modelo
  CData_CDMX1 <- CData_CDMX %>% 
    select(-Pos_OCup,-Seg_Loc,-Alum,-Agua,-Pandill,-Robos,-Del_Esc,-Mas_Op_Del)
  
  au <- hetcor(CData_CDMX1)
  au2 <- au$correlations
  corrplot(au2)
  
  Data_C <- Data_CDMX2 %>% 
    select(-Pos_OCup,-Seg_Loc,-Alum,-Agua,-Pandill,-Robos,-Del_Esc,-Mas_Op_Del)
  
  au <- hetcor(Data_C)
  au2 <- au$correlations
  corrplot(au2)
  
  # Eliminamos sexo y agrupamos las alcaldías, niveles educativos y situación laboral
  Region <- rep(0,length(CData_CDMX1$Nom_Mun))
  for(i in 1:length(CData_CDMX1$Nom_Mun)){
    if(CData_CDMX1$Nom_Mun[i] %in% c("Gustavo A. Madero","Venustiano Carranza","Iztacalco")){
      Region[i] <- "Norte"
    }else if(CData_CDMX1$Nom_Mun[i] %in% c("Cuauhtemoc","Miguel Hidalgo","Azcapotzalco","Alvaro Obregon","Cuajimalpa de Morelos")){
      Region[i] <- "Centro Poniente"
    }else if(CData_CDMX1$Nom_Mun[i] %in% c("Benito Juarez","Coyoacan","Tlalpan","La Magdalena Contreras")){
      Region[i] <- "Sur"
    }else if(CData_CDMX1$Nom_Mun[i] %in% c("Iztapalapa","Tlahuac","Xochimilco","Milpa Alta")){
      Region[i] <- "Oriente"
    }
  }

  Nivel_Edu <- rep("",length(CData_CDMX1$Niv_Edu))
  for(i in 1:length(CData_CDMX1$Niv_Edu)){
    if(CData_CDMX1$Niv_Edu[i] %in% c(0,1,2)){
      Nivel_Edu[i]<-"No Básica"
    }else if(CData_CDMX1$Niv_Edu[i] %in% c(3,4,5)){
      Nivel_Edu[i]<-"Secundaria"
    }else if(CData_CDMX1$Niv_Edu[i] %in% c(6,7)){
      Nivel_Edu[i]<-"Medio Superior"
    }else if(CData_CDMX1$Niv_Edu[i] == 8){
      Nivel_Edu[i]<-"Superior"
    }else if(CData_CDMX1$Niv_Edu[i] == 9){
      Nivel_Edu[i]<-"Posgrado"
    }
  }
  
  Sit_Lab <- rep("",length(CData_CDMX1$Sit_Lab_Act))
  for(i in 1:length(CData_CDMX1$Sit_Lab_Act)){
    if(CData_CDMX1$Sit_Lab_Act[i] %in% c(1,2,3)){
      Sit_Lab[i] <- "Empleado"
    }else if(CData_CDMX1$Sit_Lab_Act[i] %in% c(4,5)){
      Sit_Lab[i] <- "Estudiantes y Domésticos"
    }else if(CData_CDMX1$Sit_Lab_Act[i] %in% c(6,7,8)){
      Sit_Lab[i] <- "Sin Ocupación"
    }
  }
  
  CData_CDMX2 <- CData_CDMX1 %>% 
    select(-Sexo, -Nom_Mun, -Niv_Edu, -Sit_Lab_Act, -Vehic) %>% 
    mutate(Region =Region, Nivel_Edu=Nivel_Edu, Sit_Lab = Sit_Lab)
  CData_CDMX2$Region <- as_factor(CData_CDMX2$Region)
  CData_CDMX2$Nivel_Edu <- factor(CData_CDMX2$Nivel_Edu,levels=c("No Básica","Secundaria","Medio Superior","Superior","Posgrado"),ordered = T)
  CData_CDMX2$Sit_Lab <- as_factor(CData_CDMX2$Sit_Lab)
  
  au <- hetcor(CData_CDMX2)
  au2 <- au$correlations
  corrplot(au2)
  
  
  ####Guardamos bases de datos
  save(CData_CDMX2,file="BD_CDMX2.Rda")
  