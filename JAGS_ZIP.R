library(rjags)
library(tidyverse)
library(VGAM)


mod_dist_4.matrix_p <-  model.matrix(mod_dist_4)
mod_dist_4.matrix_b <- model.matrix(CData_CDMX2$Vic_Rob_As ~ CData_CDMX2$Edad + CData_CDMX2$Mas_Pat_Vil + CData_CDMX2$Sit_Lab)
mod_dist_4.matrix <- model.matrix(CData_CDMX2$Vic_Rob_As ~ CData_CDMX2$Seg_Mun +
                                    CData_CDMX2$Edad + CData_CDMX2$Mas_Pat_Vil + CData_CDMX2$Region + CData_CDMX2$Sit_Lab)

head(mod_dist_4.matrix)

mod_dist_4.matrix_list <- as.list(data.frame(mod_dist_4.matrix_p[,-1], mod_dist_4.matrix_b[,-1]))
mod_dist_4.matrix_list <- as.list(data.frame(mod_dist_4.matrix))
mod_dist_4.matrix_list$X.Intercept.

attach(CData_CDMX2)

data <- list(
  y = Vic_Rob_As,
  x1 = mod_dist_4.matrix_list$CData_CDMX2.Seg_Mun2,
  x21 = mod_dist_4.matrix_list$CData_CDMX2.RegionSur,
  x22 = mod_dist_4.matrix_list$CData_CDMX2.RegionNorte,
  x23 = mod_dist_4.matrix_list$CData_CDMX2.RegionOriente,
  x3 = mod_dist_4.matrix_list$CData_CDMX2.Edad,
  x4 = mod_dist_4.matrix_list$CData_CDMX2.Mas_Pat_Vil2,
  x51 = mod_dist_4.matrix_list$CData_CDMX2.Sit_LabSin.Ocupación,
  x52 = mod_dist_4.matrix_list$CData_CDMX2.Sit_LabEstudiantes.y.Domésticos,
  n =length(Vic_Rob_As)
)

param <- c("alpha00", "alpha1", "alpha21", "alpha22", "alpha23")
inits <- function(){ list(
  "alpha" = rnorm(1),
  "Beta1" = rnorm(1),
  "Beta2" = rnorm(1),
  "Beta3" = rnorm(1)
)
  
}
