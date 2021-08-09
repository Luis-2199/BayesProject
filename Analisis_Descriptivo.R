#Analisis Descriptivo

library(ggplot2)

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
  ggplot(aes(x = "", y = Tasas, fill = Region)) +
  geom_bar(stat = "identity", color = "White") +
  # scale_color_hue(l = 40, c = 35) +
  # scale_fill_hue(l = 40, c = 35) +
  coord_polar(theta = "y")  + 
  geom_text(aes(label = paste0(Tasas, "%")), position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL, fill = NULL) +
# theme(axis.text.x = element_text(size = 7, angle = 45))
  scale_fill_brewer(palette = "Blues") 

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

#Barplot sexo
CData_CDMX %>%
  ggplot() +
  geom_bar(aes(x = Sexo, y = ))







ggplot(CData_CDMX) + geom_bar(aes(x= Edad, y = Vic_Rob_As), stat = "identity")
ggplot(CData_CDMX) + geom_bar(aes(x= Edad, y = Vic_Rob_As), stat = "identity")


ggplot(CData_CDMX) + geom_bar(aes(x = Vic_Rob_As, y = TotalDelitos))

ggplot(CData_CDMX %>% filter(Nom_Mun == "Azcapotzalco")) + geom_bar(aes(x = Vic_Rob_As))

ggplot(CData_CDMX %>% filter(Nom_Mun =="Coyoacan")) + geom_bar(aes(x = Vic_Rob_As))


ggplot(CData_CDMX) + geom_bar(aes(x = Sit_Lab_Act))

ggplot(CData_CDMX) + geom_bar(aes(x = Pos_OCup))

ggplot(CData_CDMX) + geom_bar(aes(x = Imp_Seg))

ggplot(CData_CDMX) + geom_bar(aes(x = ))




library(maps)

mi_counties <- map_data("county", "mexico") %>% 
  select(lon = long, lat, group, id = subregion)
head(mi_counties)
#>     lon  lat group     id
#> 1 -83.9 44.9     1 alcona
#> 2 -83.4 44.9     1 alcona
#> 3 -83.4 44.9     1 alcona
#> 4 -83.3 44.8     1 alcona
#> 5 -83.3 44.8     1 alcona
#> 6 -83.3 44.8     1 alcona