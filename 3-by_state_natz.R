# program: Create dataframe that shows naturalization based on region of birth and state of residence. 

library(readxl)
library(tidyverse)

# =========================
# READING AND CLEANING DATA
# =========================
# read in dataframe: country of birth and state of residence
dat <- read.csv("out/natz_by_country_2016_2020.csv")

# merge in regions (source: UN)
regions <- read.csv("data/UNSD_Methodology.csv") %>% select(c("Country.or.Area", "Region.Name"))
temp <- dat %>% left_join(regions, by = c("country_birth" = "Country.or.Area")) 

# not all country names are aligned, manual cleaning
# taiwan, hong kong, macau, kosovo, czechoslovakia (former), macedonia, Netherlands Antilles (former), Serbia and Montenegro (former)
# Soviet Union (former)
regions <- regions %>% mutate(
  recoded_country = recode(Country.or.Area,
                   "Bolivia (Plurinational State of)" = "Bolivia",
                   "Brunei Darussalam" = "Brunei",
                   "Myanmar" = "Burma",
                   "China, People's Republic" = "China",
                   "Democratic Republic of the Congo"  = "Congo, Democratic Republic",
                   "Congo" = "Congo, Republic",
                   "Côte d’Ivoire" = "Cote d'Ivoire",
                   "Curaçao" = "Curacao",
                   "Iran (Islamic Republic of)" = "Iran",
                   "Democratic People's Republic of Korea" = "Korea, North",
                   "Republic of Korea"="Korea, South",
                   "Lao People's Democratic Republic" ="Laos",
                   "Micronesia (Federated States of)" ="Micronesia, Federated States",
                   "Republic of Moldova"= "Moldova",
                   "Russian Federation"= "Russia",
                   "Sint Maarten (Dutch part)" ="Sint Maarten",
                   "Syrian Arab Republic" = "Syria",
                   "United Republic of Tanzania" = "Tanzania",
                   "United Kingdom of Great Britain and Northern Ireland" = "United Kingdom",
                   "United States of America" = "United States",
                   "Venezuela (Bolivarian Republic of)" = "Venezuela",
                   "Viet Nam" = "Vietnam",
                   "British Virgin Islands" = "Virgin Islands, British",
                   "United States Virgin Islands" = "Virgin Islands, U.S.",
                   .default = Country.or.Area
                  ) 
)

# merge in regions to main data frame
temp <- dat %>% left_join(regions, by = c("country_birth" = "recoded_country")) 
temp <- temp %>% mutate(
  Region.Name = case_when(
    country_birth == "China, People's Republic" ~ "Asia",
    country_birth == "Czechoslovakia (former)" ~ "Europe",
    country_birth == "Hong Kong" ~ "Asia",
    country_birth == "Kosovo" ~ "Europe",
    country_birth == "Macau" ~ "Asia",
    country_birth == "Macedonia" ~ "Europe",
    country_birth == "Netherlands Antilles (former)" ~ "Europe",
    country_birth == "Soviet Union (former)" ~ "Europe",
    country_birth == "Serbia and Montenegro (former)" ~ "Europe",
    country_birth == "Taiwan" ~ "Asia",
    country_birth == "Swaziland" ~ "Africa",
    country_birth == "Eswatini (formerly Swaziland)" ~ "Africa",
    country_birth == "Falkland Islands" ~ "Americas",
    country_birth == "North Macedonia (formerly Macedonia)" ~ "Europe",
    country_birth == "Pitcairn Islands" ~ "Europe",
    country_birth == "Republic of Nauru" ~ "Oceania",
    country_birth == "Total" ~ "Global",
    TRUE ~ Region.Name)
)

View(temp %>% subset(is.na(Region.Name)))

# ============================
# WHAT STATES ARE SWAYABLE?
# ============================
pres_2020_summary <- read.csv("out/pres_margins.csv") %>% select(c(state, margin_2020, margincount_2020))
senate_margins <- read.csv("out/senate_margins.csv")

# PRESIDENTIAL ELECTION
# get states that are swayable (natz count is higher than margin of victory)
# make sure to run the file: "2-presidential_margins.R" first 
voting_states <- temp %>% select(-c("U.S..ARMED.SERVICES.POSTS","U.S..TERRITORIES1", "UNKNOWN","TOTAL" , "GUAM", "PUERTO.RICO", "Country.or.Area", "Region.Name"))
swayable <- rep(NA, 51)
for (i in 1:length(colnames(voting_states)[-c(1)])){
  col <- colnames(voting_states)[-c(1)][i]
  margin <- pres_2020_summary[pres_2020_summary$state == col,]$margincount_2020
  origins <- sum(voting_states[,col] > margin, na.rm = T)
  swayable[i] <- origins
}
names(swayable) <- colnames(voting_states)[-c(1)]
swayable %>% subset(swayable > 0)

swayable_states <- names(swayable[swayable>=1])
View(voting_states %>% select(c("country_birth", swayable_states)))


# define almost swayable as the newly naturalized population is
# within 95 percent of the margin
almost_swayable <- rep(NA, 51)
for (i in 1:length(colnames(voting_states)[-c(1)])){
  col <- colnames(voting_states)[-c(1)][i]
  margin <- pres_2020_summary[pres_2020_summary$state == col,]$margincount_2020
  margin_adj <- pres_2020_summary[pres_2020_summary$state == col,]$margincount_2020 * 0.95
  origins <- sum((voting_states[,col] > margin_adj & voting_states[,col] < margin), na.rm = T)
  almost_swayable[i] <- origins
}

names(almost_swayable) <- colnames(voting_states)[-c(1)]
almost_swayable %>% subset(almost_swayable > 0)

# SENATE ELECTION
newNatz <-voting_states %>% subset(country_birth == "Total")
names(newNatz) <- gsub("\\.", " ", names(newNatz))

states = names(newNatz)[-c(1)]
newNatz = as.data.frame(t(newNatz[, -c(1)])) %>% rename(newNatz = "210")  %>% 
  mutate(state = states)

senate_swayable <- senate_margins %>% left_join(newNatz, by = "state")
election_2022 <- c("ARIZONA", "COLORADO", "FLORIDA", "GEORGIA", 
                   "MONTANA", "NORTH CAROLINA", "NEW HAMPSHIRE", 'NEVADA',
                   "OHIO", "PENNSYLVANIA", 'WASHINGTON', 'WISCONSIN',
                   'ARKANSAS', 'ALABAMA', 'CALIFORNIA', 'CONNEETICUT',
                   'HAWAII','INDIANA', 'IDAHO', 'ILLINOIS', 'KANSAS',
                   'KENTUCKY','LOUISIANA', 'MARYLAND', 'NORTH DAKOTA',
                   'NEW YORK', 'OKLAHOMA', 'OREGON', 'SOUTH CAROLINA',
                   'SOUTH DAKOTA', 'UTAH', 'VERMONT')

not_safe <- c("ARIZONA", "COLORADO", "FLORIDA", "GEORGIA", 
          "MONTANA", "NORTH CAROLINA", "NEW HAMPSHIRE", 'NEVADA',
          "OHIO", "PENNSYLVANIA", 'WASHINGTON', 'WISCONSIN')

safe <- c('ARKANSAS', 'ALABAMA', 'CALIFORNIA', 'CONNEETICUT',
'HAWAII','INDIANA', 'IDAHO', 'ILLINOIS', 'KANSAS',
'KENTUCKY','LOUISIANA', 'MARYLAND', 'NORTH DAKOTA',
'NEW YORK', 'OKLAHOMA', 'OREGON', 'SOUTH CAROLINA',
'SOUTH DAKOTA', 'UTAH', 'VERMONT')

senate_swayable <- senate_swayable %>% mutate(
  election_2022 = case_when(state %in% election_2022 ~ 1,
                            TRUE ~ 0 ),
  safe = case_when(state %in% not_safe ~ 0,
                   state %in% safe ~ 1,
                   TRUE ~ as.numeric(NA))
  
)

# =============================================================
# IN EACH STATE, WHERE ARE THE NEWLY NATURALIZED CITIZENS FROM?
# =============================================================
# aggregate by region (currently aggregated by country)
by_regions <- temp %>% group_by(Region.Name) %>%  summarise_if(is.numeric, sum, na.rm = TRUE)
by_regions <- by_regions %>% mutate(
  TOTAL = rowSums(.[3:58])) 
rownames = unlist(data.frame(t(by_regions))[1,])
by_regions = data.frame(as.data.frame(t(by_regions))[-1,]) %>%
  mutate_all(as.numeric)
by_regions <- by_regions %>% mutate(state = rownames(by_regions))
rownames(by_regions) <- 1:57
colnames(by_regions) = c(rownames[1:6],"Unknown", "state")
by_regions <- by_regions %>% select(c(state, Africa, Americas, Asia, Europe, Oceania, Unknown, Global)) %>%
  mutate(
    state = gsub( "\\.", " ", state),
    state = str_to_title(state)
  )

sub(pattern = "\\.", replacement = " ", "hi.there")
?gsub
# see top states for each region/vice versa
View(by_regions)

# summary
regions_summary <- by_regions[1,]
colnames(regions_summary) <- c("state", "Africa" ,  "Americas", "Asia" ,    "Europe"   ,"Oceania" , "Unknown", "Global") 
regions_summary = regions_summary %>% select(-c("Global", "state"))
View(prop.table(regions_summary))

# summary transposed
states <- (by_regions$state)
by_regions_t <- (t(by_regions)[-c(1),])
colnames(by_regions_t) <- states

# =============================================================
# EXPORT
# =============================================================
# export the temp dataframe (countries of birth by state (and includes regional info))
write.csv(temp, "out/Natz_CountryBirth_by_State.csv", row.names = F)
write.csv(by_regions, "out/natz_by_state_regions.csv", row.names = F)

