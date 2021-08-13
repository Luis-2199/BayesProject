#Analisis Descriptivo
library(tidyverse)
library(ggplot2)




#Descriptivo Liz 

#Tablas de contingencia
# Impresión Seguridad 
xtabs(~ Vic_Rob_As+ Imp_Seg, data = CData_CDMX2)
# Segurdad Municipal
xtabs(~ Vic_Rob_As + Seg_Mun , data = CData_CDMX2)
# Mas Patrullas Vigilando
xtabs(~ Vic_Rob_As + Mas_Pat_Vil , data = CData_CDMX2)
# Region
xtabs(~ Vic_Rob_As + Region , data = CData_CDMX2)
# Nivel Educativo
xtabs(~ Vic_Rob_As + Nivel_Edu , data = CData_CDMX2)
# Sit_Lab 
xtabs(~ Vic_Rob_As + Sit_Lab , data = CData_CDMX2)

# Tablas con gráfica
# Impresión Seguridad 
as_tibble(xtabs(~ Imp_Seg + Vic_Rob_As, data = CData_CDMX2)) %>% ggplot() + 
  geom_bar(aes(x=Vic_Rob_As, y = n, fill = Imp_Seg), stat = "identity", color = "black", position = position_dodge())
# Segurdad Municipal
as_tibble(xtabs(~ Seg_Mun + Vic_Rob_As , data = CData_CDMX2)) %>% ggplot() + 
  geom_bar(aes(x=Vic_Rob_As, y = n, fill = Seg_Mun), stat = "identity", color = "black", position = position_dodge())
# Mas Patrullas Vigilando
as_tibble(xtabs(~ Mas_Pat_Vil + Vic_Rob_As, data = CData_CDMX2)) %>% ggplot() + 
  geom_bar(aes(x=Vic_Rob_As, y = n, fill = Mas_Pat_Vil), stat = "identity", color = "black", position = position_dodge())
# Region
as_tibble(xtabs(~ Region + Vic_Rob_As , data = CData_CDMX2)) %>% ggplot() + 
  geom_bar(aes(x=Vic_Rob_As, y = n, fill = Region), stat = "identity", color = "black", position = position_dodge())
# Nivel Educativo
as_tibble(xtabs(~ Vic_Rob_As + Nivel_Edu , data = CData_CDMX2))  %>% ggplot() + 
  geom_bar(aes(x=Vic_Rob_As, y = n, fill = Nivel_Edu), stat = "identity", color = "black", position = position_dodge())
# Sit_Lab 
as_tibble(xtabs(~ Vic_Rob_As + Sit_Lab , data = CData_CDMX2))  %>% ggplot() + 
  geom_bar(aes(x=Vic_Rob_As, y = n, fill = Sit_Lab), stat = "identity", color = "black", position = position_dodge())


# Boxplot (Edad)

ggplot(CData_CDMX2, aes(x = Vic_Rob_As , y = Edad)) +
geom_boxplot() + facet_wrap(~ Vic_Rob_As)












Datos_Tasas <- CData_CDMX2 %>% group_by(Region) %>% 
  summarise(TotalDelitos = sum(Vic_Rob_As))

Tasas <- rep(0,4)
for (i in 1:4) {
  aux1 <- CData_CDMX2 %>% filter(Region == Datos_Tasas$Region[i])
  Tasas[i] <- length(aux1$Vic_Rob_As)
}

Tasas <- (Datos_Tasas$TotalDelitos/Tasas)*100

Datos_Tasas <- tibble(Datos_Tasas, Tasas = signif(Tasas, digits = 4))

# Gráfica por región # Modificar
Datos_Tasas %>% 
  ggplot(aes(x = Region, y = Tasas, fill = Region)) +
  geom_bar(stat = "identity", color = "White") +
  labs(x = NULL, y = NULL, fill = NULL) +
# theme(axis.text.x = element_text(size = 7, angle = 45))
  scale_fill_brewer(palette = "Reds") 



# Gráfica por Sexo
CData_CDMX1 %>% group_by(Sexo) %>% summarise(TotalDelitos = sum(Vic_Rob_As))%>%
  ggplot(aes(x = "", y = TotalDelitos, color = Sexo, fill = Sexo)) +
  geom_bar(stat = "identity", color = "White") +
  coord_polar(theta = "y")

# Gráfica por Situación Laboral
CData_CDMX2 %>% group_by(Sit_Lab) %>% summarise(TotalDelitos = sum(Vic_Rob_As))%>%
  ggplot(aes(x = "", y = TotalDelitos, color = Sit_Lab, fill = Sit_Lab)) +
  geom_bar(color = "White", stat = "identity") +
  coord_polar(theta = "y")

# Gráfica por Nivel Academico
CData_CDMX2 %>% group_by(Nivel_Edu) %>% summarise(TotalDelitos = sum(Vic_Rob_As))%>%
  ggplot(aes(x = "", y = TotalDelitos, color = Nivel_Edu, fill = Nivel_Edu)) +
  geom_bar(stat = "identity", color = "White") +
  coord_polar(theta = "y")

# Gráfica por Imp_Seg
CData_CDMX2 %>% group_by(Imp_Seg) %>% summarise(TotalDelitos = sum(Vic_Rob_As))%>%
  ggplot(aes(x = "", y = TotalDelitos, color = Imp_Seg, fill = Imp_Seg)) +
  geom_bar(stat = "identity", color = "White") +
  coord_polar(theta = "y")

# Gráfica por Seg_Mun
CData_CDMX2 %>% group_by(Seg_Mun) %>% summarise(TotalDelitos = sum(Vic_Rob_As))%>%
  ggplot(aes(x = "", y = TotalDelitos, color = Seg_Mun, fill = Seg_Mun)) +
  geom_bar(stat = "identity", color = "White") +
  coord_polar(theta = "y")

# Gráfica por Mas_Pat_Vil = Mas Patrullas Vigilando
CData_CDMX2 %>% group_by(Mas_Pat_Vil) %>% summarise(TotalDelitos = sum(Vic_Rob_As))%>%
  ggplot(aes(x = "", y = TotalDelitos, color = Mas_Pat_Vil, fill = Mas_Pat_Vil)) +
  geom_bar(stat = "identity", color = "White") +
  coord_polar(theta = "y")

# Gráfica Vehic
CData_CDMX2 %>% group_by(Vehic) %>% summarise(TotalDelitos = sum(Vic_Rob_As))%>%
  ggplot(aes(x = "", y = TotalDelitos, color = Vehic, fill = Vehic)) +
  geom_bar(stat = "identity", color = "White") +
  coord_polar(theta = "y")

# Gráfica Edad
CData_CDMX2 %>% group_by(Edad) %>% summarise(TotalDelitos = sum(Vic_Rob_As)) %>% 
  ggplot(aes(x = Edad, y = TotalDelitos, color = Edad, fill = Edad)) +
  geom_bar(stat = "identity")





#Scatter Plot todos los datos
CData_CDMX %>% ggplot() + geom_point(aes(x = 1:5496,y = Vic_Rob_As))

# Barplot número de delitos
CData_CDMX %>%
  ggplot() +
  geom_bar(aes(x = Vic_Rob_As))












# ggplot(CData_CDMX) + geom_bar(aes(x= Edad, y = Vic_Rob_As), stat = "identity")
# ggplot(CData_CDMX) + geom_bar(aes(x= Edad, y = Vic_Rob_As), stat = "identity")
# 
# 
# ggplot(CData_CDMX) + geom_bar(aes(x = Vic_Rob_As, y = TotalDelitos))
# 
# ggplot(CData_CDMX %>% filter(Nom_Mun == "Azcapotzalco")) + geom_bar(aes(x = Vic_Rob_As))
# 
# ggplot(CData_CDMX %>% filter(Nom_Mun =="Coyoacan")) + geom_bar(aes(x = Vic_Rob_As))
# 
# 
# ggplot(CData_CDMX) + geom_bar(aes(x = Sit_Lab_Act))
# 
# ggplot(CData_CDMX) + geom_bar(aes(x = Pos_OCup))
# 
# ggplot(CData_CDMX) + geom_bar(aes(x = Imp_Seg))
# 
# ggplot(CData_CDMX) + geom_bar(aes(x = ))




