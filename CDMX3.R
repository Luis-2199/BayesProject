##### CDMX3 es la base con la situacion laboral actual agrupada diferente

Sit_Lab <- rep("",length(CData_CDMX1$Sit_Lab_Act))
for(i in 1:length(CData_CDMX1$Sit_Lab_Act)){
  if(CData_CDMX1$Niv_Edu[i] %in% c(1,2,3)){
    Sit_Lab[i] <- "Empleado"
  }else if(CData_CDMX1$Niv_Edu[i] == 4){
    Sit_Lab[i] <- "Estudiante"
  }else if(CData_CDMX1$Niv_Edu[i] == 5){
    Sit_Lab[i] <- "Doméstico"
  }else if(CData_CDMX1$Niv_Edu[i] %in% c(6,7)){
    Sit_Lab[i] <- "Jub/Pens/Inc"
  }else if(CData_CDMX1$Niv_Edu[i] == 8){
    Sit_Lab[i] <- "Desempleado"
  }
}

CData_CDMX3 <- CData_CDMX1 %>% 
  select(-Sexo, -Nom_Mun, -Niv_Edu, -Sit_Lab_Act) %>% 
  mutate(Region =Region, Nivel_Edu=Nivel_Edu, Sit_Lab = Sit_Lab)
CData_CDMX3$Region <- as_factor(CData_CDMX3$Region)
CData_CDMX3$Nivel_Edu <- factor(CData_CDMX3$Nivel_Edu,levels=c("Ninguno","Prepri/Prim","Secundaria","Medio Superior","Superior","Posgrado"),ordered = T)
CData_CDMX3$Sit_Lab <- as_factor(CData_CDMX3$Sit_Lab)

save(CData_CDMX3,file="BD_CDMX3.Rda")