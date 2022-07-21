library(readxl)
library(tidyverse)


Suicide_male <- read_excel("dataset.xlsx" , sheet = 1)
Suicide_male <- rename(Suicide_male,c("2016.Suicide_male"="2016", "2017.Suicide_male"="2017", "2018.Suicide_male"="2018", "2019.Suicide_male"="2019"))

Suicide_female <- read_excel("dataset.xlsx" , sheet = 2)
Suicide_female <- rename(Suicide_female,c("2016.Suicide_female"="2016", "2017.Suicide_female"="2017", "2018.Suicide_female"="2018", "2019.Suicide_female"="2019"))


Suicide_all <- read_excel("dataset.xlsx" , sheet = 3)
Suicide_all <- rename(Suicide_all,c("2016.Suicide_all"="2016", "2017.Suicide_all"="2017", "2018.Suicide_all"="2018", "2019.Suicide_all"="2019"))


UP_male <- read_excel("dataset.xlsx" , sheet = 4)
UP_male <- rename(UP_male,c("2016.UP_male"="2016", "2017.UP_male"="2017", "2018.UP_male"="2018", "2019.UP_male"="2019"))

UP_female <- read_excel("dataset.xlsx" , sheet = 5)
UP_female <- rename(UP_female,c("2016.UP_female"="2016", "2017.UP_female"="2017", "2018.UP_female"="2018", "2019.UP_female"="2019"))

UP_all <- read_excel("dataset.xlsx" , sheet = 6)
UP_all <- rename(UP_all,c("2016.UP_all"="2016", "2017.UP_all"="2017", "2018.UP_all"="2018", "2019.UP_all"="2019", "2020.UP_all"="2020"))


literacy_male <- read_excel("dataset.xlsx" , sheet = 7)
literacy_male <- rename(literacy_male,c("2016.literacy_male"="2016", "2017.literacy_male"="2017", "2018.literacy_male"="2018", "2019.literacy_male"="2019", "2020.literacy_male"="2020"))

literacy_female <- read_excel("dataset.xlsx" , sheet = 8)
literacy_female <- rename(literacy_female,c("2016.literacy_female"="2016", "2017.literacy_female"="2017", "2018.literacy_female"="2018", "2019.literacy_female"="2019", "2020.literacy_female"="2020"))

literacy_all <- read_excel("dataset.xlsx" , sheet = 9)
literacy_all <- rename(literacy_all,c("2016.literacy_all"="2016", "2017.literacy_all"="2017", "2018.literacy_all"="2018", "2019.literacy_all"="2019", "2020.literacy_all"="2020"))

GDP_growth <- read_excel("dataset.xlsx" , sheet = 10)
GDP_growth <- rename(GDP_growth,c("2016.growth"="2016", "2017.growth"="2017", "2018.growth"="2018", "2019.growth"="2019", "2020.growth"="2020"))

GDP <- read_excel("dataset.xlsx" , sheet = 11)
GDP <- rename(GDP,c("2016.GDP"="2016", "2017.GDP"="2017", "2018.GDP"="2018", "2019.GDP"="2019"))

happiness <- read_excel("dataset.xlsx" , sheet = 12)

df_list <- list(Suicide_male, Suicide_female, Suicide_all, UP_male, UP_female, UP_all, literacy_male, literacy_female, literacy_all, GDP_growth, GDP, happiness)

merged <- df_list %>% 
  reduce(full_join, by='Country')

write.csv(merged, "H:\\7th semester\\BA\\A6\\merged.csv")

