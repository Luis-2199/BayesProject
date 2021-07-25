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
        filter(AP7_3_13 != 9, AP7_3_14 != 9) %>% 
        mutate(AP7_4_13 = replace_na( AP7_4_13, 0), AP7_4_14 = replace_na(AP7_4_14, 0)) %>% 
        select(-AP7_3_13, -AP7_3_14) %>%  
        mutate(Num_Inc = AP7_4_13 + AP7_4_14) %>% 
        select(-AP7_4_13, -AP7_4_14)

Data_per_est <- Data %>% 
                  group_by(NOM_ENT) %>% 
                  summarise(Sum_Num_Inc = sum(Num_Inc))

Data_per_est_top_n <- Data_per_est %>% top_n(n = 10, wt = Sum_Num_Inc)

Data_cdmx <- Data %>% 
              filter(NOM_ENT == "Ciudad de Mexico") 
              

Data_Izt <- Data_cdmx %>% 
            filter(NOM_MUN == "Iztapalapa")


Data_per_mun <-  Data_cdmx %>% 
                  group_by(NOM_MUN) %>% 
                  summarise(Sum_Num = sum(Num_Inc))






