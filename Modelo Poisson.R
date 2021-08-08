library(tidyverse)
library(corrplot)
library(polycor)
library(glm2)
library(pscl)
library(boot)

CData_CDMX1 <- CData_CDMX %>% 
  select(-Pos_OCup,-Seg_Loc,-Alum,-Agua,-Pandill,-Robos,-Del_Esc,-Mas_Op_Del)

au <- hetcor(CData_CDMX)
au2 <- au$correlations
corrplot(au2)

r <- xtabs(~Vic_Rob_As + Vehic, data=CData_CDMX1)
chisq.test(r)


graf <- ggplot(data = CData_CDMX, aes(x=Edad, y=Vic_Rob_As))+
  geom_point(color="#B32821", size = 2)+
  geom_line(data = CData_CDMX, aes(x=Edad, y=exp(aMLE+bMLE*Edad)), color="#1B5583", size = 0.5)+
  labs(x = "Temperatura", y = NULL) +
  theme_light()


m1<-zeroinfl(Vic_Rob_As ~ Sexo + Edad + Niv_Edu + Nom_Mun + Sit_Lab_Act + Imp_Seg + Seg_Mun + Mas_Pat_Vil + Vehic,data=CData_CDMX1,dist="poisson",link="logit")
sglm1<-summary(m1)
b0_1 <- sglm$coefficients[[1]][1,1]
b1_1 <- sglm$coefficients[[1]][2,1]
b0_2 <- sglm$coefficients[[2]][1,1]
b1_2 <- sglm$coefficients[[2]][2,1]

## Eliminamos sexo
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
  if(CData_CDMX1$Niv_Edu[i] %in% c(1,2)){
    Nivel_Edu[i]<-"Prepri/Prim"
  }else if(CData_CDMX1$Niv_Edu[i] %in% c(3,4,5)){
    Nivel_Edu[i]<-"Secundaria"
  }else if(CData_CDMX1$Niv_Edu[i] %in% c(6,7)){
    Nivel_Edu[i]<-"Medio Superior"
  }else if(CData_CDMX1$Niv_Edu[i] == 8){
    Nivel_Edu[i]<-"Superior"
  }else if(CData_CDMX1$Niv_Edu[i] == 9){
    Nivel_Edu[i]<-"Posgrado"
  }else if(CData_CDMX1$Niv_Edu[i] == 0){
    Nivel_Edu[i]<-"Ninguno"
  }
}
CData_CDMX2 <- CData_CDMX1 %>% 
  select(-Sexo, -Nom_Mun, -Niv_Edu) %>% 
  mutate(Region =Region, Nivel_Edu=Nivel_Edu)
CData_CDMX2$Region <- as_factor(CData_CDMX2$Region)
CData_CDMX2$Nivel_Edu <- factor(CData_CDMX2$Nivel_Edu,levels=c("Ninguno","Prepri/Prim","Secundaria","Medio Superior","Superior","Posgrado"),ordered = T)

r <- xtabs(~Edad + Sit_Lab_Act, data=CData_CDMX2)
chisq.test(r)

au <- hetcor(CData_CDMX2)
au2 <- au$correlations
corrplot(au2)

m2<-zeroinfl(Vic_Rob_As ~ Region + Edad + Nivel_Edu + Sit_Lab_Act + Imp_Seg + Seg_Mun + Mas_Pat_Vil + Vehic,data=CData_CDMX2,dist="poisson",link="logit")
sglm2<-summary(m2)
sglm2

logit<-exp(b0_2+b1_2*42)/(1+exp(b0_2+b1_2*42))
lam<-exp(b0_1+b1_1*42)
vec<-rep(0,10)
for(t in 0:9){
  if(t==0){
    vec[t+1]<-logit+(1-logit)*dpois(t,lam)
  }else{
    vec[t+1]<-(1-logit)*dpois(t,lam)
  }
}

Edad42<-CData_CDMX %>% 
  filter(Edad == 42)

ggplot() + geom_point(aes(x=0:9, y=vec)) geom_histogram() + geom_point(aes(x=Vic_Rob_As, y=vec*length(Vic_Rob_As)),col="#FF6666")





# mod<-glm(Vic_Rob_As ~ Edad + Pos_OCup, family = "poisson",data = CData_CDMX)
# sglm <- summary(mod)
# aMLE <- as.vector(sglm$coef[1,1])
# bMLE <- as.vector(sglm$coef[2,1])