######################## Segundo intento para un Poisson Cero Inflado ##########

# library(tidyverse)
# library(corrplot)
# library(polycor)
# library(glm2)
# library(pscl)
# library(boot)

# NOTA: Cargar CData_CDMX2
# glimpse(CData_CDMX2)

# Semilla para el CV
# set.seed(27)

################ Guardamos num de obs en n
N <- length(CData_CDMX2$Vic_Rob_As) ; N # 5419

################ Modelo Nulo
mod_null <- zeroinfl(Vic_Rob_As ~ 1, data= CData_CDMX2,
                     dist="poisson")
summary(mod_null)


#################### Modelo todas covariables ############################
mod1 <- zeroinfl(Vic_Rob_As ~ ., data= CData_CDMX2,
                 dist="poisson",link="logit")
summary(mod1)

# # logLikelihood                     -3253
# # Pseudo R2 McFadden                0.06315549
# 1 - mod1$loglik/mod_null$loglik
# # Pseudo R2 McFadden adjusted       0.06113971
# 1 - (mod1$loglik - 7)/mod_null$loglik
# # Cross Validation                  0.3442906   <- mean error predict
# #                                   0.3439249   <- adjusted me
# cv.glm(CData_CDMX2, mod1, K = 4)$delta




################ Modelo sin Imp_Seg:  ###############################
#### Edad + Seg_Mun + Mas_Pat_Vil + Region + Nivel_Edu + Sit_Lab
mod2 <- zeroinfl(Vic_Rob_As ~ Edad + Seg_Mun + Mas_Pat_Vil
                 + Region + Nivel_Edu + Sit_Lab,
                 data= CData_CDMX2, dist="poisson",link="logit")
summary(mod2)

# logLikelihood                     -3255



################ Modelo sin Imp_Seg y Nivel_Edu: ##########################
#### Edad + Seg_Mun + Mas_Pat_Vil + Region + Sit_Lab
mod3 <- zeroinfl(Vic_Rob_As ~ Edad + Seg_Mun + Mas_Pat_Vil
                 + Region + Sit_Lab,
                 data= CData_CDMX2, dist="poisson",link="logit")
summary(mod3)

# logLikelihood                     -3262




################ Modelo sin Imp_Seg, Nivel_Edu, Mas_Pat_Vil: #############
#### Edad + Seg_Mun + Region + Sit_Lab
mod4 <- zeroinfl(Vic_Rob_As ~ Edad + Seg_Mun + Region + Sit_Lab,
                 data= CData_CDMX2, dist="poisson",link="logit")
summary(mod4)

# logLikelihood                     -3269
