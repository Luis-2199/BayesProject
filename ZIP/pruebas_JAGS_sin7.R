################# Simulaciones JAGS sin 7s ##################################

library(rjags)
library(tidyverse)
library(VGAM)
library(pscl)
library(car)

# Base sin 7s
CData_CDMX2_sin7 <- CData_CDMX2 %>% filter(Vic_Rob_As < 7)
N <- length(CData_CDMX2_sin7$Vic_Rob_As)
attach(CData_CDMX2_sin7)

# Base con estimaciones
predictions <- CData_CDMX2_sin7 %>%  mutate(p = 1/(1 +
                                exp(-(3.69453 - 0.04343*Edad + 0.48986*(Mas_Pat_Vil == 2)
                                    - 1.14794*(Region == "Sur") - 1.39458*(Region == "Norte")
                                    - 2.28340*(Region == "Oriente")
                                    - 0.57888*(Sit_Lab == "Sin Ocupación")
                                    - 0.87063*(Sit_Lab == "Estudiantes y Domésticos")))),
                             lambda = exp(- 1.30127 + 0.47804*(Seg_Mun == 2)
                                          - 0.45385*(Region == "Sur")
                                          - 0.19114*(Region == "Norte")
                                          + 0.75256*(Region == "Oriente"))) %>% 
  select(Vic_Rob_As, p, lambda)

# Ajustamos modelo
mod_ZIPsin7 <- zeroinfl(Vic_Rob_As ~ Seg_Mun  + Region |
                          Edad + Mas_Pat_Vil + Region + Sit_Lab,
                        data= CData_CDMX2_sin7, dist="poisson",link="logit")
summary(mod_ZIPsin7)

{
  # Cargamos las simulaciones Bayes
  sim_bayes <- rep(0, times=N)
  for(i in 1:N){
    sim_bayes[i] <- rzipois(1, lambda = predictions$lambda[i], pstr0 = predictions$p[i])
  }
  
  # Simulaciones Frec
  # Cargamos los valores estimados para p y lambda
  p <- predict(mod_ZIPsin7, type = "zero")
  lambda <- predict(mod_ZIPsin7, type = "count")
  N <- length(CData_CDMX2_sin7$Vic_Rob_As)
  sim_frec <- rep(0, times = N)
  for(i in 1:N){
    sim_frec[i] <- rzipois(1, lambda = lambda[i], pstr0 = p[i])
  }
  
  # Cargamos bases
  reales <- CData_CDMX2_sin7$Vic_Rob_As
  
  tib_sim_frec <- table(sim_frec) %>% as_tibble() %>%
    rename(count_frec=n, value=sim_frec); tib_sim_frec
  
  tib_reales <- table(reales) %>% as_tibble() %>%
    rename(count_real=n, value=reales); tib_reales
  
  tib_bayes <- table(sim_bayes) %>% as_tibble() %>%
    rename(count_bayes=n, value=sim_bayes); tib_bayes
  
  tib_completa <- tib_sim_frec %>% right_join(tib_reales,
                                             by = c("value" = "value")) %>%
    left_join(tib_bayes, by = c("value" = "value")) %>% 
    replace_na(list(count_frec = 0, count_bayes = 0)); tib_completa
}

# Graficamos
ggplot(tib_completa, aes(x=value, y=count_real)) +
  geom_bar(stat = "identity", fill = "#00B8C2") +
  geom_point(aes(y=count_frec, color="Clásico"), size = 2.5, alpha = .8) +
  geom_point(aes(y=count_bayes, color="Bayesiano"), size = 2.5, alpha = .8) +
  theme_bw() +
  scale_color_manual(values = c("#B225BC", "#2528BC")) +
  xlab("Número de robos o asaltos") +
  ylab("Personas") +
  labs(color = "Modelo")


############ Pruebas simulaciones (media y mat de confusion)
sim_conf_mat_bayes(predictions, res = CData_CDMX2_sin7$Vic_Rob_As,
                       muest.size = length(CData_CDMX2_sin7$Vic_Rob_As))
# 68.7973% de efectividad

sim_media_bayes(predictions,
                   muest.size = length(CData_CDMX2_sin7$Vic_Rob_As))   # 0.2268783
mean(CData_CDMX2_sin7$Vic_Rob_As)                                      # 0.2538305

