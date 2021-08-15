################ ZIP sin 7s desde el inicio aaaaaaaahhh ####################
library(tidyverse)
library(corrplot)
library(polycor)
library(glm2)
library(pscl)
library(boot)
library(VGAM)

# Base sin 7s
CData_CDMX2_sin7 <- CData_CDMX2 %>% filter(Vic_Rob_As < 7)

################ Guardamos num de obs en n
N <- length(CData_CDMX2_sin7$Vic_Rob_As) ; N        # 5417

################ Modelo Nulo
mod_null <- zeroinfl(Vic_Rob_As ~ 1, data= CData_CDMX2_sin7,
                     dist="poisson")
summary(mod_null)


#################### Modelo todas covariables ############################
mod_full <- zeroinfl(Vic_Rob_As ~ ., data= CData_CDMX2_sin7,
                     dist="poisson",link="logit")
summary(mod_full)

# loglik of zero-inflated model -3225.546 
# BIC of zero-inflated model 6691.816 
# AIC of zero-inflated model 6507.091
{
  cat("loglik of zero-inflated model", logLik(mod_full), "\n")
  cat("BIC of zero-inflated model", BIC(mod_full), "\n")
  cat("AIC of zero-inflated model", AIC(mod_full))
}


################ Modelo sin Imp_Seg:  ###############################
#### Edad + Seg_Mun + Mas_Pat_Vil + Region + Nivel_Edu + Sit_Lab
mod2 <- zeroinfl(Vic_Rob_As ~ Edad + Seg_Mun + Mas_Pat_Vil
                 + Region + Nivel_Edu + Sit_Lab,
                 data= CData_CDMX2_sin7, dist="poisson",link="logit")
summary(mod2)

# loglik of zero-inflated model -3228.321 
# BIC of zero-inflated model 6680.171 
# AIC of zero-inflated model 6508.641
{
  cat("loglik of zero-inflated model", logLik(mod2), "\n")
  cat("BIC of zero-inflated model", BIC(mod2), "\n")
  cat("AIC of zero-inflated model", AIC(mod2))
}


################ Modelo distinto 1  ###############################
#### Poisson: Seg_Mun + Region + Nivel_Edu + Sit_Lab
#### Bern:    Edad + Mas_Pat_Vil + Region + Sit_Lab
mod_dist_1 <- zeroinfl(Vic_Rob_As ~ Seg_Mun + Region + Nivel_Edu + Sit_Lab |
                         Edad + Mas_Pat_Vil + Region + Sit_Lab,
                       data= CData_CDMX2_sin7, dist="poisson",link="logit")
summary(mod_dist_1)

# loglik of zero-inflated model -3229.856 
# BIC of zero-inflated model 6623.061 
# AIC of zero-inflated model 6497.712 <---------------------- MEJOR?
{
  cat("loglik of zero-inflated model", logLik(mod_dist_1), "\n")
  cat("BIC of zero-inflated model", BIC(mod_dist_1), "\n")
  cat("AIC of zero-inflated model", AIC(mod_dist_1))
}

############ Pruebas simulaciones (media y mat de confusion)
sim_conf_mat_zeroinfl2(mod_dist_1,
                       res = CData_CDMX2_sin7$Vic_Rob_As,
                       muest.size = N)                      # 0.6827377



################ Modelo distinto 2  ###############################
#### Poisson: Seg_Mun + Region + Nivel_Edu
#### Bern:    Edad + Mas_Pat_Vil + Region + Sit_Lab
mod_dist_2 <- zeroinfl(Vic_Rob_As ~ Seg_Mun + Region + Nivel_Edu |
                         Edad + Mas_Pat_Vil + Region + Sit_Lab,
                       data= CData_CDMX2_sin7, dist="poisson",link="logit")
summary(mod_dist_2)

# loglik of zero-inflated model -3231.696 
# BIC of zero-inflated model 6609.546 
# AIC of zero-inflated model 6497.392 <---------------------- MEJOR?
{
  cat("loglik of zero-inflated model", logLik(mod_dist_2), "\n")
  cat("BIC of zero-inflated model", BIC(mod_dist_2), "\n")
  cat("AIC of zero-inflated model", AIC(mod_dist_2))
}

############ Pruebas simulaciones (media y mat de confusion)
sim_conf_mat_zeroinfl2(mod_dist_2,
                       res = CData_CDMX2_sin7$Vic_Rob_As,
                       muest.size = N)                         # 0.6833856


################ Modelo distinto 3  ###############################
#### Poisson: Seg_Mun + Region
#### Bern:    Edad + Mas_Pat_Vil + Region + Sit_Lab
mod_dist_3 <- zeroinfl(Vic_Rob_As ~ Seg_Mun + Region |
                         Edad + Mas_Pat_Vil + Region + Sit_Lab,
                       data= CData_CDMX2_sin7, dist="poisson",link="logit")
summary(mod_dist_3)

# loglik of zero-inflated model -3237.105 
# BIC of zero-inflated model 6585.975 <---------------------- MEJOR
# AIC of zero-inflated model 6500.21 <----------------------- EL 2do MEJOR
{
  cat("loglik of zero-inflated model", logLik(mod_dist_3), "\n")
  cat("BIC of zero-inflated model", BIC(mod_dist_3), "\n")
  cat("AIC of zero-inflated model", AIC(mod_dist_3))
}

############ Pruebas simulaciones (media y mat de confusion)
sim_conf_mat_zeroinfl2(mod_dist_3,
                       res = CData_CDMX2_sin7$Vic_Rob_As,
                       muest.size = N)                         # 0.6833856


################ Modelo distinto 4  ###############################
#### Poisson: Seg_Mun
#### Bern:    Edad + Mas_Pat_Vil + Region + Sit_Lab
mod_dist_4 <- zeroinfl(Vic_Rob_As ~ Seg_Mun |
                         Edad + Mas_Pat_Vil + Region + Sit_Lab,
                       data= CData_CDMX2_sin7, dist="poisson",link="logit")
summary(mod_dist_4)

# loglik of zero-inflated model -3278.8 
# BIC of zero-inflated model 6643.572 
# AIC of zero-inflated model 6577.599
{
  cat("loglik of zero-inflated model", logLik(mod_dist_4), "\n")
  cat("BIC of zero-inflated model", BIC(mod_dist_4), "\n")
  cat("AIC of zero-inflated model", AIC(mod_dist_4))
}
