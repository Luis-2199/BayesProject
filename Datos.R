library(tidyverse)
setwd("C:/Users/edson/Documents/BayesProject")

##Obtenemos nuestros CSV's
Data_raw_Sec3 <- read_csv("Sec3.csv")
Data_raw_Sec45 <- read_csv("sec45.csv")
Data_raw_Sec67 <- read_csv("sec67.csv")


##Juntamos las tablas
Data <- inner_join(Data_raw_Sec3, Data_raw_Sec45) %>%  
        inner_join(Data_raw_Sec67) %>% 
        select(-ID_PER, -CVE_ENT, -CVE_MUN, -AP6_1_2,-AP7_3_07,-AP7_3_08,-AP7_3_09,-AP7_3_10,-AP7_3_11,-AP7_3_12,-AP7_3_15,
               -AP7_4_07,-AP7_4_08,-AP7_4_09,-AP7_4_10,-AP7_4_11,-AP7_4_12,-AP7_4_15) %>% 
        filter(AP7_3_05 != 9,AP7_3_06 != 9,AP7_3_13 != 9,AP7_3_14 != 9) %>% 
        mutate(AP7_4_05 = replace_na( AP7_4_05, 0), AP7_4_06 = replace_na(AP7_4_06, 0),AP7_4_13 = replace_na( AP7_4_13, 0),
               AP7_4_14 = replace_na( AP7_4_14, 0)) %>% 
        select(-AP7_3_05,-AP7_3_06,-AP7_3_13,-AP7_3_14) %>%
        mutate(Vic_Rob_As = AP7_4_06 + AP7_4_05,Vic_Sex = AP7_4_13 + AP7_4_14) %>% 
        select(-AP7_4_13, -AP7_4_14,-AP7_4_05,-AP7_4_06)

##Reemplazamos NA's por ceros
Data_CDMX <- Data %>% 
  filter(Nom_Ent == "Ciudad de Mexico") %>%
  mutate(Sit_Lab_Act =AP3_8, Pos_OCup =replace_na( AP3_10, 0), Imp_Seg = AP4_2_05, Seg_Loc=AP4_3_1, Seg_Mun =AP4_3_2, Alum =AP4_8_1,
         Agua=AP4_8_2,Pandill=AP4_8_4,Robos=AP4_8_5,Del_Esc=AP4_8_6,Mas_Op_Del=AP5_1_08,Mas_Pat_Vil=AP5_1_10, Vehic=AP6_1_1) %>%
  select(-AP3_8,-AP3_10,-AP4_2_05,-AP4_3_1,-AP4_3_2,-AP4_8_1,-AP4_8_2,-AP4_8_4,-AP4_8_5,-AP4_8_6,-AP5_1_08,-AP5_1_10,-AP6_1_1)

save(Data_CDMX,file="BD_CDMX_Vic.Rda")

