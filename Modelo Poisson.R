library(tidyverse)
library(corrplot)
library(polycor)
library(glm2)
library(pscl)
library(boot)

mod<-zeroinfl(Vic_Rob_As ~ Edad,data=CData_CDMX2,dist="poisson",link="logit")
sglm<-summary(mod)
sglm


#Ejemplo
b0_1 <- sglm$coefficients[[1]][1,1]
b1_1 <- sglm$coefficients[[1]][2,1]
b0_2 <- sglm$coefficients[[2]][1,1]
b1_2 <- sglm$coefficients[[2]][2,1]

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



Edad42<-CData_CDMX2 %>%
  filter(Edad == 42)

vec1 <- rep(vec[1:8],775)[1:5419]
v<-rep(0:7,775)[1:5419]


ggplot(CData_CDMX2) +
  geom_bar(aes(x=Vic_Rob_As,y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels=scales::percent) + geom_point(aes(x=v , y=vec1))


CData_CDMX2_1 <-CData_CDMX2 %>%
  select(-Vic_Rob_As)

tidy(mod)

summary(predict(m2,newdata = CData_CDMX2_1))

mod<-glm(Vic_Rob_As ~ Nivel_Edu + Sit_Lab, family = "poisson",data = CData_CDMX2)

# sglm <- summary(mod)
# aMLE <- as.vector(sglm$coef[1,1])
# bMLE <- as.vector(sglm$coef[2,1])