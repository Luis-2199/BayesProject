library(tidyverse)

Data_raw_Sec3 <- read_csv("Sec3.csv"#, col_types = list(col_character(), col_factor(), col_integer(),
                                                       #  col_integer(), col_integer(), col_character(),
                                                        # col_integer(), col_character(), col_integer(),
                                                         ) #col_integer()))

Data_raw_Sec45 <- read_csv("sec45.csv")

Data_raw_Sec67 <- read_csv("sec67.csv")


glimpse(Data_raw_Sec67)

Data_join <- inner_join(Data_raw_Sec3, Data_raw_Sec45) %>%  inner_join(Data_raw_Sec67)


