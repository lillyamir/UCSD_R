setwd("~/Desktop/USIPC/naturalizations")
# TABLE OF CONTENTS
# 1) Set up
# 2) Analysis
#     (a) new york population counts
#     (b) Naturalizations by World Regions (Of birth)
#     (c) Naturalizations in each county by World Region (of birth)
#     (d) Calculate occupation prevalence of immigrants in Ohio

# =======
# SET UP
# =======
library(tidyverse)
#install.packages('tidyr')
library(tidyr)
#install.packages('stringr')
library(stringr)
#install.packages('dplyer')
library(dplyr)
# DATASETS from by_state_natz.R
temp <- read.csv('out/Natz_CountryBirth_by_State.csv')
by_regions <- read.csv("out/natz_by_state_regions.csv")

##### USCIS DATA #####
# Read in USCIS data
newyork <- temp %>% select("Region.Name", "country_birth", "NEW.YORK") %>% 
  subset(!is.na(Region.Name))

###### ACS DATA ######
# load acs data
if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")
#install.packages('ipumsr')
library(ipumsr)
ddi <- read_ipums_ddi("data/usa_00012.xml")
data <- read_ipums_micro(ddi)

# add in occupation code names
occ_codes <- read.csv("data/occ-codes.csv")[1:9,]
category = c()
codes = c()
# add in occupation category names
for (i in 1:(length(occ_codes$Category))){
  start = occ_codes$Start[i]
  stop = occ_codes$Stop[i]
  reps = stop-start + 1 
  category <- append(category, rep(occ_codes$Category[i], reps))
  codes <- append(codes, start:stop)
}
occ_codes_clean <- data.frame(category, codes)
data <- data %>% left_join(occ_codes_clean, by = c("OCC" = "codes"))

# Subsets: ACS Data - Ohio,  ACS Data - Immigrants, ACS Data - Naturlized, 
#           ACS Data - Newly Naturalized
acs_NY20 <- data %>% subset(YEAR == 2020 & STATEFIP == 36)
acs_NY20_imm <- acs_NY20 %>% subset(BPL > 120)
acs_NY20_natz <- acs_NY20 %>% subset(CITIZEN == 2) 
acs_NY20_newNatz <- acs_NY20_natz %>% subset(YRNATUR >=2016)

###### ANALYSIS ###### 
#============================================================================
# new york POPULATION COUNTS
#============================================================================
###### NYLCULATE NUMBER OF IMMIGRANTS, TOTAL NATURALIZED IN new york ######
sum(acs_NY20$PERWT) # new york population
sum(acs_NY20_imm$PERWT) # number of immigrants in new york
sum(acs_NY20_natz$PERWT) # number of naturalized citizens in new york
sum(acs_NY20_newNatz$PERWT)  # number of newly naturalized citizens in new york

####### NYLCULATE NUMBER ELIGIBLE FOR CITIZENSHIP ####################
eligible_citizen <- acs_NY20_imm %>% subset(CITIZEN %in% c(3, 4) & YRSUSA1 > 5)
sum(eligible_citizen$PERWT)

####### PRE-TAX WAGE CONTRIBUTIONS ####################
# get rid of 99999/99998 values representing N/A or Missing incomes
incwage <- (acs_NY20_imm %>% subset(INCWAGE < 999998))
sum(incwage$INCWAGE * incwage$PERWT)

#============================================================================
# NATURLIZATIONS BY WORLD REGION
#============================================================================
# Regional Breakdowns (source: USCIS)
n_newnatz = 402904
natz_by_region <- as.data.frame(newyork %>% group_by(Region.Name) %>% summarise(Freq = sum(NEW.YORK)) %>%
                                  mutate(Percentage = Freq/n_newnatz))
View(natz_by_region)

# top ten country of origins
View(newyork %>% arrange(desc(newyork)))

# look for ohio's position relative to other state's naturalizations in each region
View(by_regions %>% select(c("state", "Africa")))
View(by_regions %>% select(c("state", "Americas")))
View(by_regions %>% select(c("state", "Asia")))
View(by_regions %>% select(c("state", "Europe")))
View(by_regions %>% select(c("state", "Oceania")))

#============================================================================
# NATURLIZATIONS IN EACH COUNTY ACROSS REGIONS OF BIRTH
#============================================================================
View(
  as.data.frame(
    acs_NY20_natz %>% group_by(COUNTYFIP) %>% summarise(count = sum(PERWT)) %>% 
      subset(count > 1000)
  ))

# Merge in corresponding region of birth (Source: UN)
# getting corresponding labels (for numeric codes)
country_labels <- attributes(acs_NY20_natz$BPLD)$labels
country_names <- names(country_labels)
bpl_labels <- data.frame(country_labels, country_names)

acs_NY20_natz <- acs_NY20_natz %>% left_join(bpl_labels, by = c("BPLD" = "country_labels"))
regions <- read.csv("data/UNSD_Methodology.csv") %>% select(Country.or.Area, Region.Name )
temp <-(acs_NY20_natz %>% left_join(regions, by =c("country_names" = "Country.or.Area")))

# manually recode regions that are not named the same
View(temp %>% subset(is.na(Region.Name)) %>% count(country_names))
temp <- (temp %>% mutate(
  Region.Name = case_when(
    country_names == "Africa, ns/nec" ~ "Africa",
    country_names == "Antigua-Barbuda" ~ "Americas",
    country_names == "Asia, nec/ns" ~ "Asia",
    country_names == "Belize/British Honduras" ~ "Americas",
    country_names == "Bosnia" ~ "Europe",
    country_names == "Burma (Myanmar)"~ "Asia",
    country_names == "Byelorussia" ~ "Europe",
    country_names == "Cambodia (Kampuchea)" ~ "Asia",
    country_names == "Caribbean, ns" ~ "Americas",
    country_names == "Czechoslovakia" ~ "Europe",
    country_names == "Eastern Africa, nec/ns" ~ "Africa",
    country_names == "Egypt/United Arab Rep." ~ "Africa",
    country_names == "England" ~ "Europe",
    country_names == "Europe, ns." ~ "Europe",
    country_names == "Guyana/British Guiana" ~ "Americas",
    country_names == "Hong Kong" ~ "Asia",
    country_names == "Iran" ~ "Asia",
    country_names == "Israel/Palestine" ~ "Asia",
    country_names == "Ivory Coast" ~ "Africa",
    country_names == "Korea" ~ "Asia",
    country_names == "Laos" ~ "Asia",
    country_names == "Macedonia" ~ "Europe",
    country_names == "Micronesia" ~ "Oceania",
    country_names == "Moldavia" ~ "Europe",
    country_names == "Other USSR/Russia" ~ "Europe",
    country_names == "Scotland" ~ "Europe",
    country_names == "South Africa (Union of)" ~ "Africa",
    country_names == "South America, ns" ~ "Americas",
    country_names == "Sri Lanka (Ceylon)" ~ "Asia",
    country_names == "Syria" ~ "Asia",
    country_names == "Taiwan" ~ "Asia",
    country_names == "Tanzania" ~ "Africa",
    country_names == "United Kingdom, ns" ~ "Europe",
    country_names == "USSR, ns" ~ "Europe",
    country_names == "Venezuela" ~ "Americas",
    country_names == "Vietnam" ~ "Asia",
    country_names == "West Indies, ns" ~ "Americas",
    country_names == "Western Africa, ns" ~ "Africa",
    country_names == "Western Africa, ns" ~ "Africa",
    country_names == "Yugoslavia" ~ "Europe",
    country_names == "Zaire" ~ "Africa",
    TRUE ~ Region.Name
  )
))

# group by county
county_region <- (
  as.data.frame(
    temp %>% group_by(COUNTYFIP, Region.Name) %>% summarise(count = sum(PERWT))
  )
)

###METROPOLITAN###
fips <- read.csv("data/state_and_county_fips_master.csv")
natz <- data %>% subset(YEAR == 2020 & CITIZEN == 2)

# Merge in corresponding region of birth (Source: UN)
# getting corresponding labels (for numeric codes)
country_labels <- attributes(natz$BPLD)$labels
country_names <- names(country_labels)
bpl_labels <- data.frame(country_labels, country_names)

natz <- natz %>% left_join(bpl_labels, by = c("BPLD" = "country_labels"))
regions <- read.csv("data/UNSD_Methodology.csv") %>% select(Country.or.Area, Region.Name )
temp <-(natz%>% left_join(regions, by =c("country_names" = "Country.or.Area")))

# merge in county names
temp <- temp %>% mutate(
  fip = str_c(str_pad(STATEFIP, 2, side = "left", pad = "0"), str_pad(COUNTYFIP, 3, side = "left", pad = "0")),
  fip = as.numeric(fip)
)
temp <- temp %>% left_join(fips %>% select(fips, name), by = c("fip" = "fips"))

# group by metropolitan area 2013
met_region <- (
  as.data.frame(
    temp %>% group_by(MET2013, Region.Name) %>% summarise(count = sum(PERWT))
  )
)

# wide to long reshape to get all the regional breakdowns
met_region <- spread(met_region, key = Region.Name, value = count)
met_region[is.na(met_region)] <- 0
met_region$TOTAL <-rowSums(met_region[,2:7])

# label the areas
met_labels <- attributes(natz$MET2013)$labels
met_names <- names(met_labels)
met_labels <- data.frame(met_labels, MET2013_NAME = met_names)

# rearrange datagrame
met_region <- met_region %>% left_join(met_labels, by = c("MET2013" = "met_labels")) %>% 
  relocate(MET2013_NAME, .after = MET2013)
met_region <- met_region %>% relocate(TOTAL, .after = MET2013_NAME)  %>% 
  arrange(desc(TOTAL))
met_region <- met_region %>% select(-c(MET2013))

# NY
ny <- met_region %>% filter(grepl("NY", MET2013_NAME))
View(as.data.frame(ny))

# wide to long reshape to get all the regional breakdowns
county_region <- spread(county_region, key = Region.Name, value = count)
county_region[is.na(county_region)] <- 0
county_region$TOTAL <-rowSums(county_region)
View(county_region %>% subset(TOTAL > 1000))

#============================================================================
####### NYLCULATE OCCUPATION PREVALENCE OF IMMIGRANTS IN new york ###########
#============================================================================
# workforce populaiton in new york
n_newyork = sum(acs_NY20$PERWT[acs_NY20$OCC != 0])
occ_general <- (
  acs_NY20 %>% group_by(category)  %>%
    summarise(n = sum(PERWT),
              Freq = sum(PERWT)/n_newyork)
)

# immigrant workforce population
n_imm = sum(acs_NY20_imm$PERWT[acs_NY20_imm$OCC != 0])
occ_imm <- (
  acs_NY20_imm %>% group_by(category)  %>%
    summarise(n = sum(PERWT),
              Freq = sum(PERWT)/n_imm)
)

occ <- data.frame(category = occ_imm$category, n_gen = occ_general$n, freq_gen = occ_general$Freq,
                  n_imm = occ_imm$n, freq_imm = occ_imm$Freq)

occ %>% subset(freq_imm > freq_gen)

# =========================================
# export csvs
write.csv(newyork, "Country_Region_Birth_OHIO.csv", row.names = F)
write.csv(county_region, "Natz_by_County_OHIO.csv", row.names = F)
write.csv(natz_by_region, "Natz_by_Region_OHIO.csv", row.names = F)
write.csv(acs_NY20, "out/acs_NY20.csv", row.names = F)
write.csv(acs_NY20_imm, "out/acs_NY20_imm.csv", row.names = F)
write.csv(acs_NY20_natz, "out/acs_NY20_natz.csv", row.names = F)
write.csv(acs_NY20_newNatz, "out/acs_NY20_newNatz.csv", row.names = F)
write.csv(by_state, "out/Natz_by_State.csv", row.names = F)


cols= temp$country_birth
temp2 <- data.frame(t(temp))[-c(1:2),]
colnames(temp2) <- cols
state <- rownames(temp2)
temp2$state <- rownames(temp2)
by_state <- by_regions %>% left_join(temp2, by = "state")






####QUARTERLY DATA#####
library(tidyverse)
library(forecast)

# ====================
#read in quarterly data
dat <- read.csv("data/quarterly_natz.csv")
yearly_natz <- read.csv("data/yearly_natz.csv")
yearly_natz$x2016_2022 <- rowSums(yearly_natz[,7:13], na.rm = T)
(yearly_natz %>% select(x2016_2022))[1,]
