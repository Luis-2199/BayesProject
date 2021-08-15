####### Identificar variables significativas con modelo poisson est. clasica

library(rsample) 



#### Modelo1: Region + Edad + Sit_Lab_Act + Nivel_Edu +
#               Imp_Seg + Seg_Mun + Mas_Pat_Vil + Vehic
mod1 <- zeroinfl(Vic_Rob_As ~ Region + Edad + Sit_Lab_Act + Nivel_Edu +
                   Imp_Seg + Seg_Mun + Mas_Pat_Vil + Vehic,
                 data= CData_CDMX2, dist="poisson",link="logit")
summary(mod1)


# LogLikelihood -3279.309

# R2 no se calcula para glms
#
# R2 <- 1 - sum(mod1$fitted.values^2)/(length(CData_CDMX2$Vic_Rob_As)*
#                                        var(CData_CDMX2$Vic_Rob_As))
# R2 # 0.7601229
# adjR2 <- 1 - ((1 - R2) * (length(CData_CDMX2$Vic_Rob_As) - 1) /
#                 (length(CData_CDMX2$Vic_Rob_As) - 7 - 1))
# adjR2 # 0.759817
# 
# with(summary(mod1), 1 - deviance/null.deviance)


#### Modelo2: Region + Edad + Sit_Lab_Act +
#               Imp_Seg + Seg_Mun + Mas_Pat_Vil + Vehic
mod2 <- zeroinfl(Vic_Rob_As ~ Region + Edad + Sit_Lab_Act +
           Imp_Seg + Seg_Mun + Mas_Pat_Vil + Vehic,
         data=CData_CDMX2,dist="poisson",link="logit")
summary(mod2)
# LogLikelihood -3287.651

cv.glm(CData_CDMX2, mod2, K = 4)$delta

###### Hay que cargar CData_CDMX3
#### Modelo3: 