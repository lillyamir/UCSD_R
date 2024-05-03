# Program: create dataframe to aggregate naturalizations by country over 2016 to 2020
library(tidyverse)
library(readxl)
# read in all years (naturalization by country)
by_country_2020 <- (read_excel("data/natz_by_country.xlsx", skip = 3, sheet = "2020")[-c(1:10),] %>%
  mutate(across(!"Region and country of birth", as.numeric)))[1:219,]
colnames(by_country_2020) <- toupper(colnames(by_country_2020))

by_country_2019 <- (read_excel("data/natz_by_country.xlsx", skip = 3, sheet = "2019")[-c(1:10),] %>%
  mutate(across(!"Region and country of birth", as.numeric)))[1:222,]
colnames(by_country_2019) <- toupper(colnames(by_country_2019))

by_country_2018<- (read_excel("data/natz_by_country.xlsx", skip = 3, sheet = "2018")[-c(1:10),] %>%
  mutate(across(!"Region and country of birth", as.numeric)))[1:223,]
colnames(by_country_2018) <- toupper(colnames(by_country_2018))

by_country_2017 <- (read_excel("data/natz_by_country.xlsx", skip = 3, sheet = "2017")[-c(1:10),]  %>%
  mutate(across(!"Region and country of birth", as.numeric)))[1:214,]
colnames(by_country_2017) <- toupper(colnames(by_country_2017))

by_country_2016 <- (read_excel("data/natz_by_country.xlsx", skip = 3, sheet = "2016")[-c(1:10),] %>%
  mutate(across(!"Region and country of birth", as.numeric)))[1:220,]
colnames(by_country_2016) <- toupper(colnames(by_country_2016))

# sum up over all countries
by_country <- by_country_2020 %>% full_join(by_country_2019,  by = "REGION AND COUNTRY OF BIRTH") %>%
  full_join(by_country_2018, by = "REGION AND COUNTRY OF BIRTH") %>%
  full_join(by_country_2017,  by = "REGION AND COUNTRY OF BIRTH") %>%
  full_join(by_country_2016,  by = "REGION AND COUNTRY OF BIRTH") 

by_country <- rbind(by_country_2020, by_country_2019, by_country_2018, by_country_2017, by_country_2016 ) %>% 
       rename(country_birth = "REGION AND COUNTRY OF BIRTH") %>%
  group_by(country_birth) %>%
  summarise_if(is.numeric, sum, na.rm = T)

write.csv(by_country, "out/natz_by_country_2016_2020.csv", row.names = F)  
  

