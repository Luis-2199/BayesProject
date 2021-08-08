library(tidyverse)
library(corrplot)
library(polycor)
library(glm2)
library(pscl)
library(boot)

setwd("C:/Users/edson/Documents/BayesProject")
load(file="BD_CDMX_Vic.Rda")

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

Data_CDMX <- Data_CDMX %>%
  filter(Edad !=98) %>%
  select(-Nom_Ent,-Vic_Sex)

Data_CDMX2 <- Data_CDMX

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


colnames(Data_CDMX)

levels(Data_CDMX$Imp_Seg) #"0" "1"
levels(Data_CDMX$Seg_Loc) #"1" "2" "9"
levels(Data_CDMX$Seg_Mun) #"1" "2" "9"
levels(Data_CDMX$Alum) #"1" "2" "9"
levels(Data_CDMX$Agua) #"1" "2" "9"
levels(Data_CDMX$Pandill) #"1" "2" "9"
levels(Data_CDMX$Robos) #"1" "2" "9"
levels(Data_CDMX$Del_Esc) #"1" "2" "9"
levels(Data_CDMX$Mas_Op_Del) #"1" "2" "9"
levels(Data_CDMX$Mas_Pat_Vil) #"1" "2" "9"
levels(Data_CDMX$Vehic) #"1" "2" "9"

d1 <- Data_CDMX %>% filter(Vic_Rob_As != 0)

d2 <- Data_CDMX %>% filter(Vic_Rob_As == 0)

#d1
{
  sum(d1$Seg_Loc==1) #263
  sum(d1$Seg_Loc==2) #823/1086
  
  sum(d1$Seg_Mun==1) #142
  sum(d1$Seg_Mun==2) #946/1088
  
  sum(d1$Alum==1) #643
  sum(d1$Alum==2) #448/1091
  
  sum(d1$Agua==1) #582
  sum(d1$Agua==2) #509/1091
  
  sum(d1$Pandill==1) #473
  sum(d1$Pandill==2) #599/1072
  
  sum(d1$Robos==1) #946
  sum(d1$Robos==2) #142/1088
  
  sum(d1$Del_Esc==1) #543
  sum(d1$Del_Esc==2) #352/895
  
  sum(d1$Mas_Op_Del==1) #302
  sum(d1$Mas_Op_Del==2) #509/811
  
  sum(d1$Mas_Pat_Vil==1) #367
  sum(d1$Mas_Pat_Vil==2) #523/890
  
  sum(d1$Vehic==1) #426
  sum(d1$Vehic==2) #664/1090
}

#d2
{
  sum(d2$Seg_Loc==1) #1536
  sum(d2$Seg_Loc==2) #2849/4385
  
  sum(d2$Seg_Mun==1) #930
  sum(d2$Seg_Mun==2) #3447/4377
  
  sum(d2$Alum==1) #2172
  sum(d2$Alum==2) #2221/4393
  
  sum(d2$Agua==1) #2053
  sum(d2$Agua==2) #2344/4397
  
  sum(d2$Pandill==1) #1355
  sum(d2$Pandill==2) #2988/4343
  
  sum(d2$Robos==1) #3280
  sum(d2$Robos==2) #10812/14092
  
  sum(d2$Del_Esc==1) #1555
  sum(d2$Del_Esc==2) #1978/3533
  
  sum(d2$Mas_Op_Del==1) #998
  sum(d2$Mas_Op_Del==2) #2268/3266
  
  sum(d2$Mas_Pat_Vil==1) #1753
  sum(d2$Mas_Pat_Vil==2) #1962/3715
  
  sum(d2$Vehic==1) #1857
  sum(d2$Vehic==2) #2537/4394
}


set.seed(3)
for(i in 1:nrow(d1)){
  d1$Seg_Loc[i] <- ifelse(d1$Seg_Loc[i]==9,rbernoulli(1,823/1086)+1,d1$Seg_Loc[i])
  d1$Seg_Mun[i] <- ifelse(d1$Seg_Mun[i]==9,rbernoulli(1,946/1088)+1, d1$Seg_Mun[i])
  d1$Alum[i] <- ifelse(d1$Alum[i]==9,rbernoulli(1,448/1091)+1,d1$Alum[i])
  d1$Agua[i] <- ifelse(d1$Agua[i]==9,rbernoulli(1,509/1091)+1,d1$Agua[i])
  d1$Pandill[i] <- ifelse(d1$Pandill[i]==9,rbernoulli(1,599/1072)+1,d1$Pandill[i])
  d1$Robos[i] <- ifelse(d1$Robos[i]==9,rbernoulli(1,142/1088)+1,d1$Robos[i])
  d1$Del_Esc[i] <- ifelse(d1$Del_Esc[i]==9,rbernoulli(1,352/895)+1,d1$Del_Esc[i])
  d1$Mas_Op_Del[i] <- ifelse(d1$Mas_Op_Del[i]==9,rbernoulli(1,509/811)+1,d1$Mas_Op_Del[i])
  d1$Mas_Pat_Vil[i] <- ifelse(d1$Mas_Pat_Vil[i]==9,rbernoulli(1,523/890)+1,d1$Mas_Pat_Vil[i])
  d1$Vehic[i] <- ifelse(d1$Vehic[i]==9,rbernoulli(1,664/1090)+1,d1$Vehic[i])
}

for (i in 1:nrow(d2)){
  d2$Seg_Loc[i] <- ifelse(d2$Seg_Loc[i]==9,rbernoulli(1,2849/4385)+1,d2$Seg_Loc[i])
  d2$Seg_Mun[i] <- ifelse(d2$Seg_Mun[i]==9,rbernoulli(1,3447/4377)+1,d2$Seg_Mun[i])
  d2$Alum[i] <- ifelse(d2$Alum[i]==9,rbernoulli(1,2221/4393)+1,d2$Alum[i])
  d2$Agua[i] <- ifelse(d2$Agua[i]==9,rbernoulli(1,2344/4397)+1,d2$Agua[i])
  d2$Pandill[i] <- ifelse(d2$Pandill[i]==9,rbernoulli(1,2988/4343)+1,d2$Pandill[i])
  d2$Robos[i] <- ifelse(d2$Robos[i]==9,rbernoulli(1,10812/14092)+1,d2$Robos[i])
  d2$Del_Esc[i] <- ifelse(d2$Del_Esc[i]==9,rbernoulli(1,1978/3533)+1,d2$Del_Esc[i])
  d2$Mas_Op_Del[i] <- ifelse(d2$Mas_Op_Del[i]==9,rbernoulli(1,2268/3266)+1,d2$Mas_Op_Del[i])
  d2$Mas_Pat_Vil[i] <- ifelse(d2$Mas_Pat_Vil[i]==9,rbernoulli(1,1962/3715)+1,d2$Mas_Pat_Vil[i])
  d2$Vehic[i] <- ifelse(d2$Vehic[i]==9,rbernoulli(1,2537/4394)+1,d2$Vehic[i])
}

CData_CDMX <- rbind(d1,d2)

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

sum_fin <- summary(CData_CDMX)

CData_CDMX2<-CData_CDMX %>% 
  mutate(Vic_Rob_As=ifelse(Vic_Rob_As>0,1,0))

DF1<-Data_CDMX %>% 
  select(Vic_Rob_As,Seg_Loc) %>% 
  filter(Seg_Loc != 9)
DF1$Seg_Loc <- as_factor(DF1$Seg_Loc)

au <- hetcor(DF1)
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

sum_fin <- summary(CData_CDMX2)
au <- hetcor(CData_CDMX2)
au2 <- au$correlations
corrplot(au2)

  
  # Modelo Edson
  
  D1 <- Data_CDMX.9 %>% filter(Vic_Rob_As != 0)
  
  D2 <- Data_CDMX.9 %>% filter(Vic_Rob_As == 0)
  
  sum(D1$Vehic == 1)
  sum(D2$Vehic == 1)
  
  sum(D1$Mas_Pat_Vil == 1)
  sum(D2$Mas_Pat_Vil == 1)
  
  sum(D1$Mas_Op_Del == 1)
  sum(D2$Mas_Op_Del == 1)
  
  sum(D1$Del_Esc == 1)
  sum(D2$Del_Esc == 1)
  
  sum(D1$Robos == 1)
  sum(D2$Robos == 1)
  
  sum(D1$Pandill == 1)
  sum(D2$Pandill == 1)
  
  sum(D1$Agua == 1)
  sum(D2$Agua == 1)
  
  sum(D1$Alum == 1)
  sum(D2$Alum == 1)
  
  sum(D1$Seg_Mun == 1)
  sum(D2$Seg_Mun == 1)
  
  sum(D1$Seg_Loc == 1)
  sum(D2$Seg_Loc == 1)
  
  sum(D1$Imp_Seg == 1)
  sum(D2$Imp_Seg == 1)
  
  save(CData_CDMX,file="BD_CDMX_Imp.Rda")