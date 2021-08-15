################# Modelo Ganador sin 7s ##################################
# Base sin 7s
CData_CDMX2_sin7 <- CData_CDMX2 %>% filter(Vic_Rob_As < 7)

# Ajustamos modelo
mod_ZIPsin7 <- zeroinfl(Vic_Rob_As ~ Seg_Mun  + Region |
                         Edad + Mas_Pat_Vil + Region + Sit_Lab,
                       data= CData_CDMX2_sin7, dist="poisson",link="logit")
summary(mod_ZIPsin7)

# loglik of zero-inflated model -3237.105 
# BIC of zero-inflated model 6585.975 
# AIC of zero-inflated model 6500.21
{
  cat("loglik of zero-inflated model", logLik(mod_ZIPsin7), "\n")
  cat("BIC of zero-inflated model", BIC(mod_ZIPsin7), "\n")
  cat("AIC of zero-inflated model", AIC(mod_ZIPsin7))
}

############# Ji cuadrada
mnull <- update(mod_ZIPsin7, . ~ 1) # Modelo Nulo
pchisq(2*(logLik(mod_ZIPsin7) - logLik(mnull)),
       df = 6, lower.tail = FALSE) # Es estadísticamente significativo
                                   # que el poisson sin covariables 
pchisq(2*(logLik(mod_ZIPsin7) - logLik(mnull)),
       df = 5, lower.tail = FALSE) # Es estadísticamente significativo
                                   # que el poisson sin covariables 


############# Vuong (contra poisson solo)
summary(mod_PoiSin7 <- glm(Vic_Rob_As ~ Seg_Mun + Region,
                          family = poisson, data = CData_CDMX2_sin7)) # Poisson solo
vuong(mod_ZIPsin7, mod_PoiSin7) # Si es mejor aplicar un Poisson inflado



############# Freq plot vs reales vs poisson solo
# Realizamos las simulaciones con base en el modelo y con base en el poisson
{
  ####### Simulacion ZIP
  # Cargamos los valores estimados para p y lambda
  p <- predict(mod_ZIPsin7, type = "zero")
  lambda <- predict(mod_ZIPsin7, type = "count")
  N <- length(CData_CDMX2_sin7$Vic_Rob_As)
  sim_ZIP <- rep(0, times = N)
  for(i in 1:N){
    sim_ZIP[i] <- rzipois(1, lambda = lambda[i], pstr0 = p[i])
  }
  
  ####### Simulacion Reg Poisson
  mu <- exp(predict(mod_PoiSin7))
  sim_Poi <- rep(0, times = N)
  for(i in 1:N){
      sim_Poi[i] <- rpois(1, lambda = mu[i])
  }
  
  reales <- CData_CDMX2_sin7$Vic_Rob_As
  tib_sim_ZIP <- table(sim_ZIP) %>% as_tibble() %>%
    rename(count_sim=n, value=sim_ZIP); tib_sim_ZIP
  
  tib_reales <- table(reales) %>% as_tibble() %>%
    rename(count_real=n, value=reales); tib_reales
  
  tib_poisson <- table(sim_Poi) %>% as_tibble() %>%
    rename(count_poi=n, value=sim_Poi); tib_poisson
  
  tib_completa <- tib_sim_ZIP %>% right_join(tib_reales,
                                             by = c("value" = "value")) %>%
    left_join(tib_poisson, by = c("value" = "value")) %>% 
    replace_na(list(count_sim = 0, count_poi = 0)); tib_completa
}

# Graficamos
ggplot(tib_completa, aes(x=value, y=count_real)) +
  geom_bar(stat = "identity") +
  geom_point(aes(y=count_sim, color="ZIP")) +
  geom_point(aes(y=count_poi, color="Poisson"))



############ Pruebas simulaciones (media y mat de confusion)
sim_conf_mat_zeroinfl2(mod_ZIPsin7, res = CData_CDMX2_sin7$Vic_Rob_As,
                       muest.size = length(CData_CDMX2_sin7$Vic_Rob_As))
                                                      # 68.42034% de efectividad

sim_media_zeroinfl(mod_ZIPsin7,
                   muest.size = length(CData_CDMX2_sin7$Vic_Rob_As))   # 0.2409083
mean(CData_CDMX2_sin7$Vic_Rob_As)                                      # 0.2538305


########### Pruebas RegPoi sola
sim_conf_mat_poi(mod_PoiSin7, res = CData_CDMX2_sin7$Vic_Rob_As,
                 muest.size = length(CData_CDMX2_sin7$Vic_Rob_As))
                                                      # 68.21968% de efectividad


################## Modelo sin Reg NO GANA ######################################
mod_ZIPsin7_2 <- zeroinfl(Vic_Rob_As ~ Seg_Mun|
                          Edad + Mas_Pat_Vil + Region + Sit_Lab,
                        data= CData_CDMX2_sin7, dist="poisson",link="logit")
summary(mod_ZIPsin7_2)

# loglik of zero-inflated model -3278.8 
# BIC of zero-inflated model 6643.572 
# AIC of zero-inflated model 6577.599
{
  cat("loglik of zero-inflated model", logLik(mod_ZIPsin7_2), "\n")
  cat("BIC of zero-inflated model", BIC(mod_ZIPsin7_2), "\n")
  cat("AIC of zero-inflated model", AIC(mod_ZIPsin7_2))
}
