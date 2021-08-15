library(tidyverse)
library(ggplot2)

###### Elaboracion de la serie de tiempo
manif_df <- tibble(year = seq(from = 2013, to = 2020),
                   num = c(2, 2, 3, 7, 13, 2, 7, 9))
manif_df

sum(manif_df$num) # Total

ts_manifestaciones <- ts(manif_df$num, start = c(2013), frequency = 1)

# Grafica
tibble(time = time(ts_manifestaciones),
                     serie = ts_manifestaciones,
                     str = c("2", "2", "3", "7", "13", "2", "7", "9")) %>%
  ggplot(aes(x = time, y = serie, label = str)) +
  geom_line(size = 2, color = "#6C4675") +
  geom_point(size = 4, shape = 21, stroke = 2, color = "#6C4675", fill = "white") +
  geom_text(nudge_x = c(0, 0, .2, .2, .2, .2, .2, .2),
            nudge_y = c(.8, .8, 0, 0, 0, 0, 0 ,0), size = 8) +
  labs(x = "Año", y = "Manifestaciones") +
  ggtitle("Número de Manifestaciones en CDMX", #REV
          subtitle = "2013-2020") +
  scale_x_continuous(breaks = seq(2013, 2020, 1)) +
  scale_y_continuous(breaks = seq(0, 15, 3)) +
  theme_light() +
  theme(plot.title = element_text(size = rel(3)),
        plot.subtitle = element_text(size = rel(2)),
        axis.line = element_line(colour = "black", linetype = "solid"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = rel(2)),
        axis.text.x = element_text(color = "black", size = rel(2)),
        axis.text.y = element_text(size = rel(2)),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())




###### Elaboracion de la grafica de barras

# Simbología
# Azul <- Justicia Patriarcal e Impunidad
# Violeta <- Alerta de Violencia de Género
# Rosa <- Feminicidio
# Rojo <- Acoso callejero
# Verde <- Aborto
# Gris <- Violencia Contra las Mujeres (junté este y el siguiente por problemas
# de definición)

clasif_table <- tribble( ~class,  ~num,
                         "Justicia\n Patriarcal\n e Impunidad", 16,
                         "Alerta de\n Violencia de\n Género", 3,
                         "Feminicidio", 9,
                         "Acoso\n Callejero", 5,
                         "Aborto", 3,
                         "Violencia Contra\n las Mujeres", 9
                         )
clasif_table

sum(clasif_table$num) # Total

# Grafica
ggplot(data = clasif_table, aes(x = class,
                                y = num, fill = class)) +
  geom_bar(stat = "identity") +
  labs(x = NULL, y = "Número de Manifestaciones") +
  ggtitle("Número de manifestaciones por reclamo", #REV
          subtitle = "2013-2020") +
  scale_y_continuous(breaks = seq(0, 15, 3)) +
  scale_fill_manual(values =  c("#57A639", "#D53032", "darkorchid3", "#DE4C8A", "dodgerblue", "gold1")) +
  theme_light() +
  theme(plot.title = element_text(size = rel(3)),
        plot.subtitle = element_text(size = rel(2)),
        axis.line = element_line(colour = "black", linetype = "solid"),
        axis.title.y = element_text(size = rel(2)),
        axis.text.x = element_text(color = "black", size = rel(2.8)),
        axis.text.y = element_text(size = rel(2)),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "none")

