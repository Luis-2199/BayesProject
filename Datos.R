library(tidyverse)

Data_raw_Sec3 <- read_csv("Sec3.csv"#, col_types = list(col_character(), col_factor(), col_integer(),
                                                       #  col_integer(), col_integer(), col_character(),
                                                        # col_integer(), col_character(), col_integer(),
                                                         ) #col_integer()))

Data_raw_Sec45 <- read_csv("sec45.csv")

Data_raw_Sec67 <- read_csv("sec67.csv")


glimpse(Data_raw_Sec67)

Data <- inner_join(Data_raw_Sec3, Data_raw_Sec45) %>%  
        inner_join(Data_raw_Sec67) %>% 
        select(-ID_PER, -CVE_ENT, -CVE_MUN, -AP6_1_2) %>% 
        filter(AP7_3_05 != 9,AP7_3_06 != 9,AP7_3_07 != 9,AP7_3_08 != 9,AP7_3_09 != 9,
               AP7_3_10 != 9,AP7_3_11 != 9,AP7_3_12 != 9,AP7_3_13 != 9,AP7_3_14 != 9,AP7_3_15 != 9) %>% 
        mutate(AP7_4_05 = replace_na( AP7_4_05, 0), AP7_4_06 = replace_na(AP7_4_06, 0),AP7_4_07 = replace_na( AP7_4_07, 0),
               AP7_4_08 = replace_na( AP7_4_08, 0),AP7_4_09 = replace_na( AP7_4_09, 0),AP7_4_10 = replace_na( AP7_4_10, 0),
               AP7_4_11 = replace_na( AP7_4_11, 0),AP7_4_12 = replace_na( AP7_4_12, 0),AP7_4_13 = replace_na( AP7_4_13, 0),
               AP7_4_14 = replace_na( AP7_4_14, 0),AP7_4_15 = replace_na( AP7_4_15, 0)) %>% 
        select(-AP7_3_05,-AP7_3_06,-AP7_3_07,-AP7_3_08,-AP7_3_09,-AP7_3_10,-AP7_3_11,-AP7_3_12,-AP7_3_13,-AP7_3_14,-AP7_3_15) 
        #mutate(Num_Inc = AP7_4_13 + AP7_4_14) %>% 
        #select(-AP7_4_13, -AP7_4_14)

Data_per_est <- Data %>% 
  filter(NOM_ENT == "Ciudad de Mexico") %>%
  group_by(NOM_MUN) %>%
  summarise(Robo_Asalto = sum(AP7_4_05),Otro_Robo = sum(AP7_4_06),Fraude_b = sum(AP7_4_07),Fraude_con = sum(AP7_4_08),
            Extorsion = sum(AP7_4_09),Amenaza = sum(AP7_4_10),Lesion = sum(AP7_4_11),Secuestro = sum(AP7_4_12),Host = sum(AP7_4_13),
            Violacion = sum(AP7_4_14),Otros = sum(AP7_4_15))

Data_per_est <- Data %>% 
  filter(NOM_ENT == "Ciudad de Mexico") %>%
  group_by(NOM_MUN) %>%
  summarise(Robo_Asalto = mean(AP7_4_05),Otro_Robo = mean(AP7_4_06),Fraude_b = mean(AP7_4_07),Fraude_con = mean(AP7_4_08),
            Extorsion = mean(AP7_4_09),Amenaza = mean(AP7_4_10),Lesion = mean(AP7_4_11),Secuestro = mean(AP7_4_12),Host = mean(AP7_4_13),
            Violacion = mean(AP7_4_14),Otros = mean(AP7_4_15))

Data_per_est_top_n <- Data_per_est %>% top_n(n = 10, wt = Sum_Num_Inc)

Data_cdmx <- Data %>% 
              filter(NOM_ENT == "Ciudad de Mexico") 
              

Data_Izt <- Data_cdmx %>% 
            filter(NOM_MUN == "Iztapalapa")


Data_per_mun <-  Data_cdmx %>% 
                  group_by(NOM_MUN) %>% 
                  summarise(Sum_Num = sum(Num_Inc))






