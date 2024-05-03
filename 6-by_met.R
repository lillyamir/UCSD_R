# table 10
if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")
ddi <- read_ipums_ddi("data/usa_00012.xml")
data <- read_ipums_micro(ddi)

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
    country_names == "Americas, ns" ~ "Americas",
    country_names == "Cape Verde" ~ "Africa",
    country_names == "Czech Republic" ~ "Europe",
    country_names == "Kirghizia" ~ "Asia",
    country_names == "Kosovo" ~ "Europe",
    country_names == "North Africa, ns" ~ "Africa",
    country_names == "Northern Ireland" ~ "Europe",
    country_names == "Republic of Georgia" ~ "Asia",
    country_names == "St. Kitts-Nevis" ~ "Americas",
    country_names == "St. Lucia" ~ "Americas",
    country_names == "St. Vincent" ~ "Americas",
    country_names == "Yemen Arab Republic (North)" ~ "Asia",
    country_names == "Azores" ~ "Europe",
    country_names == "Bolivia" ~ "Americas",
    country_names == "Samoa, 1940-1950" ~ "Oceania",
    TRUE ~ Region.Name
  )
))

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

# export
write.csv(met_region, "out/Table10.csv", row.names = F)


met_region <- read.csv("out/final tables/appendix-table-10.csv")
# OHIO
ohio <- met_region %>% filter(grepl("OH", MET2013_NAME))
View(data.frame(ohio))
           
# GA
ga <- met_region %>% filter(grepl("GA", MET2013_NAME))
View(as.data.frame(ga))

# AZ
az <- met_region %>% filter(grepl("AZ", MET2013_NAME))
View(as.data.frame(az))

# NV
nv <- met_region %>% filter(grepl("NV", MET2013_NAME))
View(as.data.frame(nv))

# PA
pa <- met_region %>% filter(grepl("PA", MET2013_NAME))
View(as.data.frame(pa))

# FL
fl <- met_region %>% filter(grepl("FL", MET2013_NAME))
View(as.data.frame(fl))

# NC
nC <- met_region %>% filter(grepl("NC", MET2013_NAME))
View(as.data.frame(nC))

# WI
wi <- met_region %>% filter(grepl("WI", MET2013_NAME))
View(as.data.frame(wi))

# TX
tx <- met_region %>% filter(grepl("TX", MET2013_NAME))
View(as.data.frame(tx))

# MI
mi <- met_region %>% filter(grepl("MI", MET2013_NAME))
View(as.data.frame(mi))

# VA
va <- met_region %>% filter(grepl("VA", MET2013_NAME))
View(as.data.frame(va))



           