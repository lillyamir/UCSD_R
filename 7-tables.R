library(xlsx)
# load acs data
if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")
ddi <- read_ipums_ddi("data/usa_00012.xml")
data <- read_ipums_micro(ddi)

# table 4
yearly_natz <- read.csv("data/yearly_natz.csv")
yearly_natz$x2016_2022 <- rowSums(yearly_natz[,7:13], na.rm = T)

state_pop <- data %>% group_by(STATEFIP) %>% 
  mutate(naturalized = (CITIZEN == 2) * PERWT) %>%
  summarise(total_population = sum(PERWT),
            total_naturalized = sum(naturalized)
  )

# merge in labels
state_labels <- as.data.frame(attributes(data$STATEFIP)$labels) 
state_labels$state <- rownames(state_labels) 
state_labels <- state_labels %>% rename(STATEFIP = "attributes(data$STATEFIP)$labels")
state_pop <- state_pop%>% left_join(state_labels, by = "STATEFIP") %>% relocate(state, .after = STATEFIP)

# join natz into acs data
state_pop <- state_pop %>% left_join(yearly_natz %>% select(state, x2016_2022), by = "state") %>% arrange(desc(x2016_2022))
write.csv(state_pop %>% select(state, total_population, x2016_2022, total_naturalized), "out/table4.csv", row.names = F)

# table 6

by_regions <- by_regions %>% arrange(desc(Global)) %>% mutate(
  state = recode(state, "District Of Columbia" = "District of Columbia"))
top15 <- by_regions %>%
  left_join(yearly_natz %>% select(state, x2016_2022), by = "state") %>% head(16)

write.csv(top15, "out/table5.csv",row.names = F)

# table 6
for (state in colnames(temp)[3:57]){
  top10 <- temp %>% select(c(country_birth, !!as.name(state))) %>% 
    arrange(desc(!!as.name(state))) %>% subset(country_birth != "Total") %>% head(10)
  
  write.xlsx(top10, file = "out/Table6.xlsx", sheetName = str_to_title(state), 
             col.names = TRUE, row.names = FALSE, append = TRUE)
}

# table 7
by_regions <- read.csv("out/natz_by_state_regions.csv")
projected_natz <- read.csv("out/final tables/appendix-table-4.csv")
pis <- c("Georgia", "Arizona", "Nevada", "Pennsylvania", "Florida", "North Carolina", 
         "Wisconsin", "Texas", "Michigan", "Virginia", "Total")

natz_pis <- by_regions %>% subset(state %in% pis & state !="Total") %>% 
  mutate(state = factor(state, levels = pis)) %>%
  arrange(state)

natz_pis <- natz_pis %>% rename(Total_2014_2018 = Global) %>% 
  left_join(projected_natz %>% 
              select(c(State, Naturalized.2016.2022)), by = c("state" = "State")) %>%
  rename(Total_2016_2022 = Naturalized.2016.2022) %>%
  mutate(Total_2016_2022 = as.numeric(Total_2016_2022))


natz_pis <- rbind(natz_pis, c("Total", colSums(natz_pis[,2:9])))

write.csv(natz_pis, "out/Table7.csv", row.names = F)

# table 8


# table 9
for (region in colnames(by_regions)[2:6]){
  top15 <- by_regions %>% select(c(state, !!as.name(region))) %>% 
    arrange(desc(!!as.name(region))) %>% subset(state != "Total") %>% head(15)
  
  write.xlsx(top15, file = "out/Table9.xlsx", sheetName = str_to_title(region), 
             col.names = TRUE, row.names = FALSE, append = TRUE)
  }

# table 11
colnames(temp) <- str_to_title(gsub("\\.", " ", colnames(temp)))
write.csv(temp, "out/Table11.csv", row.names = F)

# table 12-15
for (region in colnames(by_regions)[2:6]){
  sorted <- by_regions %>% arrange(desc(!!as.name(region)))

  write.xlsx(sorted, file = "out/Table12_16.xlsx", sheetName = str_to_title(region), 
           col.names = TRUE, row.names = FALSE, append = TRUE)
}

# table 
