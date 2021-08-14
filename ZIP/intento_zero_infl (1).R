######################## Segundo intento para un Poisson Cero Inflado ##########

library(tidyverse)
library(corrplot)
library(polycor)
library(glm2)
library(pscl)
library(boot)
library(VGAM)
library(mpath)  # Para encontrar el mejor modelo
library(zic)    # x2

# NOTA: Cargar CData_CDMX2
# glimpse(CData_CDMX2)

# Semilla para lo que sea xd
# set.seed(27)

################ Guardamos num de obs en n
N <- length(CData_CDMX2$Vic_Rob_As) ; N # 5419

################ Modelo Nulo
mod_null <- zeroinfl(Vic_Rob_As ~ 1, data= CData_CDMX2,
                     dist="poisson")
summary(mod_null)


#################### Modelo todas covariables ############################
mod_full <- zeroinfl(Vic_Rob_As ~ ., data= CData_CDMX2,
                 dist="poisson",link="logit")
summary(mod1)

# loglik of zero-inflated model -3253.287
# BIC of zero-inflated model 6747.308
# AIC of zero-inflated model 6562.574
{
  cat("loglik of zero-inflated model", logLik(mod_full), "\n")
  cat("BIC of zero-inflated model", BIC(mod1), "\n")
  cat("AIC of zero-inflated model", AIC(mod_full))
}




################ Modelo sin Imp_Seg:  ###############################
#### Edad + Seg_Mun + Mas_Pat_Vil + Region + Nivel_Edu + Sit_Lab
mod2 <- zeroinfl(Vic_Rob_As ~ Edad + Seg_Mun + Mas_Pat_Vil
                 + Region + Nivel_Edu + Sit_Lab,
                 data= CData_CDMX2, dist="poisson",link="logit")
summary(mod2)

predict(mod2, type = "response")


# loglik of zero-inflated model -3255.292 
# BIC of zero-inflated model 6734.123 
# AIC of zero-inflated model 6562.584
{
  cat("loglik of zero-inflated model", logLik(mod2), "\n")
  cat("BIC of zero-inflated model", BIC(mod2), "\n")
  cat("AIC of zero-inflated model", AIC(mod2))
}





################ Modelo sin Imp_Seg y Nivel_Edu: ##########################
#### Edad + Seg_Mun + Mas_Pat_Vil + Region + Sit_Lab
mod3 <- zeroinfl(Vic_Rob_As ~ Edad + Seg_Mun + Mas_Pat_Vil
                 + Region + Sit_Lab,
                 data= CData_CDMX2, dist="poisson",link="logit")
summary(mod3)

# loglik of zero-inflated model -3262.21 
# BIC of zero-inflated model 6679.178 
# AIC of zero-inflated model 6560.42  <----------------------------- 2do mejor
{
  cat("loglik of zero-inflated model", logLik(mod3), "\n")
  cat("BIC of zero-inflated model", BIC(mod3), "\n")
  cat("AIC of zero-inflated model", AIC(mod3))
}




################ Modelo sin Imp_Seg, Nivel_Edu, Mas_Pat_Vil: #############
#### Edad + Seg_Mun + Region + Sit_Lab
mod4 <- zeroinfl(Vic_Rob_As ~ Edad + Seg_Mun + Region + Sit_Lab,
                 data= CData_CDMX2, dist="poisson",link="logit")
summary(mod4)

# loglik of zero-inflated model -3268.658 
# BIC of zero-inflated model 6674.879 
# AIC of zero-inflated model 6569.316
{
  cat("loglik of zero-inflated model", logLik(mod4), "\n")
  cat("BIC of zero-inflated model", BIC(mod4), "\n")
  cat("AIC of zero-inflated model", AIC(mod4))
}




################ Modelo distincion en covariables 1 #############
#### Poisson: Seg_Mun + Nivel_Edu + Sit_Lab
#### Bern:    Edad + Mas_Pat_Vil + Region + Sit_Lab
mod_dist_1 <- zeroinfl(Vic_Rob_As ~ Seg_Mun + Nivel_Edu + Sit_Lab |
                         Edad + Mas_Pat_Vil + Region + Sit_Lab,
                 data= CData_CDMX2, dist="poisson",link="logit")
summary(mod_dist_1)

# loglik of zero-inflated model -3300.818 
# BIC of zero-inflated model 6739.198 
# AIC of zero-inflated model 6633.636
{
  cat("loglik of zero-inflated model", logLik(mod_dist_1), "\n")
  cat("BIC of zero-inflated model", BIC(mod_dist_1), "\n")
  cat("AIC of zero-inflated model", AIC(mod_dist_1))
}




################ Modelo distincion en covariables 2 #############
#### Poisson: Seg_Mun
#### Bern:    Edad + Mas_Pat_Vil + Region + Sit_Lab
mod_dist_2 <- zeroinfl(Vic_Rob_As ~ Seg_Mun |
                         Edad + Mas_Pat_Vil + Region + Sit_Lab,
                       data= CData_CDMX2, dist="poisson",link="logit")
summary(mod_dist_2)

# loglik of zero-inflated model -3309.238
# BIC of zero-inflated model 6704.452 
# AIC of zero-inflated model 6638.475
{
  cat("loglik of zero-inflated model", logLik(mod_dist_2), "\n")
  cat("BIC of zero-inflated model", BIC(mod_dist_2), "\n")
  cat("AIC of zero-inflated model", AIC(mod_dist_2))
}




################ Modelo distincion en covariables 3 #############
#### Poisson: Seg_Mun + Region + Sit_Lab
#### Bern:    Edad + Mas_Pat_Vil + Region + Sit_Lab
mod_dist_3 <- zeroinfl(Vic_Rob_As ~ Seg_Mun  + Region + Sit_Lab |
                         Edad + Mas_Pat_Vil + Region + Sit_Lab,
                       data= CData_CDMX2, dist="poisson",link="logit")
summary(mod_dist_3)

# loglik of zero-inflated model -3263.308 
# BIC of zero-inflated model 6655.582 <------------------------------ 2do mejor
# AIC of zero-inflated model 6556.617 <------------------------------ mejor E
{
  cat("loglik of zero-inflated model", logLik(mod_dist_3), "\n")
  cat("BIC of zero-inflated model", BIC(mod_dist_3), "\n")
  cat("AIC of zero-inflated model", AIC(mod_dist_3))
}




################ Modelo distincion en covariables 4 MEJOR #############
#### Poisson: Seg_Mun + Region
#### Bern:    Edad + Mas_Pat_Vil + Region + Sit_Lab
mod_dist_4 <- zeroinfl(Vic_Rob_As ~ Seg_Mun  + Region |
                         Edad + Mas_Pat_Vil + Region + Sit_Lab,
                       data= CData_CDMX2, dist="poisson",link="logit")
summary(mod_dist_4)

# loglik of zero-inflated model -3265.013 
# BIC of zero-inflated model 6641.796 <------------------------------ mejor
# AIC of zero-inflated model 6556.026 <------------------------------ mejor E
{
  cat("loglik of zero-inflated model", logLik(mod_dist_4), "\n")
  cat("BIC of zero-inflated model", BIC(mod_dist_4), "\n")
  cat("AIC of zero-inflated model", AIC(mod_dist_4))
}




# ############ Intento de optimizacion de modelo fallido  ####################
# # Gradica de Barras de la frecuencia
# barplot(with(CData_CDMX2,table(Vic_Rob_As)),
#         ylab="Frecuencia",xlab="Robos o Asaltos")
# 
# 
# ########### Modelo completo (de nuevo xdddd) #################
m_full <- zeroinfl(Vic_Rob_As ~ .|., data=CData_CDMX2, dist="poisson")
summary(m_full)
# 
# # loglik of zero-inflated model -3253.287
# # BIC of zero-inflated model 6747.308
# # AIC of zero-inflated model 6562.574
# {
#   cat("loglik of zero-inflated model", logLik(m_full), "\n")
#   cat("BIC of zero-inflated model", AIC(m_full, k=log(dim(CData_CDMX2)[1])), "\n")
#   cat("AIC of zero-inflated model", AIC(m_full))
# }
# 
# 
# # # NO JALA
# # # Backward stepwise variable selection with significance level alpha=0.01
fitback <- be.zeroinfl(m_full, data=CData_CDMX2,
                       dist="poisson", alpha=0.05, trace=T)
summary(fitback)
# 
# # TAMPOCO JALA
# # # Compute LASSO estimates.
# # fit.lasso <- zipath(Vic_Rob_As~.|.,data = CData_CDMX2, family = "poisson", nlambda=100, trace = F)
# # # Estimated coefficient parameters with smallest BIC value.
# # minBic <- which.min(BIC(fit.lasso))
# # coef(fit.lasso, minBic)
# # # Compute standard errors of coefficients and theta:
# # se(fit.lasso, minBic, log=FALSE)
# 
# # SE FUE ALV LA SESION DE R JAJA
# # # Compute log-likelihood value via 10-fold cross-validation.
# # n <- dim(CData_CDMX2)[1]
# # K <- 10
# # set.seed(27)
# # foldid <- split(sample(1:n), rep(1:K, length = n))
# # fitcv <- cv.zipath(Vic_Rob_As ~ . | ., data = CData_CDMX2, family = "poisson", foldid=foldid)
