################################ Matriz Confusion ###########################

library(tidyverse)
library(corrplot)
library(polycor)
library(glm2)
library(pscl)
library(boot)
library(VGAM)

# Para simulaciones
set.seed(27)

# Tamaño de muestra
n_muestra <- dim(CData_CDMX2[1])[1]

# Cargamos modelo ganador
mod_dist_4 <- zeroinfl(Vic_Rob_As ~ Seg_Mun  + Region |
                         Edad + Mas_Pat_Vil + Region + Sit_Lab,
                       data= CData_CDMX2, dist="poisson",link="logit")
summary(mod_dist_4)

# Cargamos los valores estimados para p y lambda
p <- predict(mod_dist_4, type = "zero")
lambda <- predict(mod_dist_4, type = "count")

# Simulamos
sim <- rzipois(n_muestra, lambda = lambda, pstr0 = p)

conf <- table(sim,CData_CDMX2$Vic_Rob_As)
sum(diag(conf))/n_muestra



######### Funcion Multiples simulaciones ###########################
sim_conf_mat_zeroinfl <- function(mod, res=CData_CDMX2$Vic_Rob_As, n=100,
                         muest.size=n_muestra){
  
  # Cargamos los valores estimados para p y lambda
  p <- predict(mod, type = "zero") # Proba de 0
  lambda <- predict(mod, type = "count") # Esperanza de la cuenta
  
  aux <- rep(0, times = n) # Vector que almacenara la prop de exito
  
  # Ciclo para simular
  for(i in 1:n){
    sim <- rzipois(muest.size, lambda = lambda, pstr0 = p) # Simulaciones
    conf <- table(sim, res) # Matriz de confusion
    aux[i] <- sum(diag(conf))/muest.size # Guardamos la proporcion de exito
  }
  
  return(mean(aux)) # Proporcion de exito promedio
}

#####  Aplicamos diferente num de simulaciones #####################
sim_conf_mat_zeroinfl(mod_dist_4, n = 100)        # 0.6822144
sim_conf_mat_zeroinfl(mod_dist_4, n = 1000)       # 0.681764
sim_conf_mat_zeroinfl(mod_dist_4, n = 10000)
