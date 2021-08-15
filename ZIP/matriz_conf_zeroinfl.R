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
  sim <- array(NA, dim = c(n,muest.size))
  
  # Ciclo para simular
  
    for (j in 1:muest.size) {
      sim[,j] <- rzipois(n, lambda = lambda[j], pstr0 = p[j])
    
    # Simulaciones
    conf <- table(round(colMeans(sim)), res) # Matriz de confusion
    aux[i] <- sum(diag(conf))/muest.size # Guardamos la proporcion de exito
    }

  
  return(mean(sim)) # Proporcion de exito promedio
}


######### ESTA ES LA CHIDA Funcion Multiples simulaciones SEGUNDA versión######
sim_conf_mat_zeroinfl2 <- function(mod, res=CData_CDMX2$Vic_Rob_As, n=100,
                                  muest.size=n_muestra){
  
  # Cargamos los valores estimados para p y lambda
  p <- predict(mod, type = "zero") # Proba de 0
  lambda <- predict(mod, type = "count") # Esperanza de la cuenta
  
  aux <- rep(0, times = n) # Vector que almacenara la prop de exito
  sim <- array(NA, dim = c(muest.size))
  
  # Ciclo para simular
  for (i in 1:n) {
   
    for (j in 1:muest.size) {
      sim[j] <- rzipois(1, lambda = lambda[j], pstr0 = p[j])
      }
    
    # Simulaciones
    conf <- table(sim, res) # Matriz de confusion
    aux[i] <- sum(diag(conf))/muest.size # Guardamos la proporcion de exito
  }
  
  return(mean(aux)) # Proporcion de exito promedio
}



######### Funcion Multiples simulaciones MEDIA########################
sim_media_zeroinfl <- function(mod, muest.size=n_muestra){
  
  # Cargamos los valores estimados para p y lambda
  p <- predict(mod, type = "zero") # Proba de 0
  lambda <- predict(mod, type = "count") # Esperanza de la cuenta

  sim <- array(NA, dim = c(muest.size))
  
  # Ciclo para simular
    
    for (j in 1:muest.size) {
      sim[j] <- rzipois(1, lambda = lambda[j], pstr0 = p[j])
    }
  
  return(mean(sim)) # Media de la simulacion
}



######### Funcion Multiples simulaciones REGPOISSON ##############
sim_conf_mat_poi <- function(mod, res=CData_CDMX2$Vic_Rob_As, n=100,
                                   muest.size=n_muestra){
  
  ####### Simulacion Reg Poisson
  mu <- exp(predict(mod_PoiSin7))
  sim_Poi <- rep(0, times = N)
  for(i in 1:muest.size){
    sim_Poi[i] <- rpois(1, lambda = mu[i])
  }
  
  aux <- rep(0, times = n) # Vector que almacenara la prop de exito
  sim <- array(NA, dim = c(muest.size))
  
  # Ciclo para simular
  for (i in 1:n) {
    
    for (j in 1:muest.size) {
      sim[j] <- rzipois(1, lambda = lambda[j], pstr0 = p[j])
    }
    
    # Simulaciones
    conf <- table(sim, res) # Matriz de confusion
    aux[i] <- sum(diag(conf))/muest.size # Guardamos la proporcion de exito
  }
  
  return(mean(aux)) # Proporcion de exito promedio
}

######### Funcion Multiples simulaciones BAYES ##############
sim_conf_mat_bayes <- function(pred, res=CData_CDMX2$Vic_Rob_As, n=100,
                             muest.size=n_muestra){
  
  aux <- rep(0, times = n) # Vector que almacenara la prop de exito
  sim_aux <- array(NA, dim = c(muest.size))
  
  # Ciclo para simular
  for (i in 1:n) {
    
    for (j in 1:muest.size) {
      sim_aux[j] <- rzipois(1, lambda = pred$lambda[j], pstr0 = pred$p[j])
    }
    
    # Simulaciones
    conf <- table(sim_aux, res) # Matriz de confusion
    aux[i] <- sum(diag(conf))/muest.size # Guardamos la proporcion de exito
  }
  
  return(mean(aux)) # Proporcion de exito promedio
}



######### Funcion Multiples simulaciones BAYES MEDIA ########################
sim_media_bayes <- function(pred, muest.size=n_muestra){
  sim <- array(NA, dim = c(muest.size))
  
  # Ciclo para simular
  
  for (j in 1:muest.size) {
    sim[j] <- rzipois(1, lambda = pred$lambda[j], pstr0 = pred$p[j])
  }
  
  return(mean(sim)) # Media de la simulacion
}


