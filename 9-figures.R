library(readxl)
library(tidyverse)
install.packages("ggrepel")
library(ggrepel)
library(urbnmapr)
library(sf)

natz_gender_age <- read.csv("out/final tables/appendix-table-3.csv")
# ==============
# naturalizations by gender
data <- data.frame(gender = c("Female", "Male"), 
           natz  = c((natz_gender_age %>% pull(Female))[1], (natz_gender_age %>% pull(Male))[1]),
           natz_p = c((natz_gender_age %>% pull(`F....`))[1], (natz_gender_age %>% pull(`M...`))[1])) %>%
  mutate(
    natz = as.numeric(natz),
    natz_p = as.numeric(natz_p)
  )

data <- data %>% 
  mutate(pos = cumsum(data$natz) - (data$natz)/2)
        

ggplot(data, aes(x = "", y = natz, fill = gender)) + 
  geom_bar(stat = "identity", width = 1, color = "white") +
  scale_fill_manual(values = c("#a0bfdb", "#b6d6d2"),
                    name = element_blank(),
                    labels = c("Female: 55%", "Male: 44%")) +
  coord_polar("y", start = 4.9) + theme_void()  + 
  theme(legend.text = element_text(size = 12)) + 
  geom_text_repel(
                   aes(y = pos, label = c("1,643,552 \n 44%", "2,050,026\n 55%")),
                   size = 5, nudge_x = 0.3, show.legend = FALSE,
                   segment.colour = NA,
                   ) +
  ggtitle("NATURALIZATION BY GENDER") +
  theme(plot.title = element_text(face = "bold", hjust = 1, size = 20))
# ==============
# natz by age
data <- data.frame(age = c("18-24", "35-49", "50-64", "65+"), 
                   natz = c(as.numeric(natz_gender_age %>% slice(17) %>% pull(Total)),
                            as.numeric(natz_gender_age %>% slice(18) %>% pull(Total)),
                            as.numeric(natz_gender_age %>% slice(19) %>% pull(Total)),
                            as.numeric(natz_gender_age %>% slice(20) %>% pull(Total))
                   )
)

# get positions for labels
data <- data %>% 
  #mutate(pos = cumsum(data$natz) - (data$natz)/2)

 mutate(csum = rev(cumsum(rev(natz))), 
       pos = natz/2 + lead(csum, 1),
       pos = if_else(is.na(pos), natz/2, pos))

# plot data
ggplot(data, aes(x = "", y = natz, fill = age)) + 
  geom_bar(stat = "identity", width = 1, color = "white") +
  scale_fill_manual(values = c(  "#a0bfdb","#b6d6d2", "#4d9e8f", "#b7dae8"),
                    name = element_blank(),
                    labels = c("18-34: 31.7%", "35-49: 40.0%", "50-64: 21.6%", "65+: 9.7%"  )) +
  coord_polar("y", start = 1) + theme_void()  + 
  theme(legend.text = element_text(size = 12)) + 
  geom_text_repel(
    aes(y = pos, label = c("1,169,769", "1,365,006", "799,202", "360,090" )),
    size = 7, , nudge_y = 0, nudge_x = 0.5, show.legend = FALSE,
    segment.colour = NA,
  ) +
  ggtitle("NATURALIZATION BY AGE") +
  theme(plot.title = element_text(face = "bold", hjust = 1, size = 20))


# ==============
# HELPER FUNCTION
map_state <- function(by_state, relevant_states, high_color, title){
  # only include the 50 states
  states <- by_state %>% subset(! (state %in% 
                                     c("Total", "U S  Territories1", "U S  Armed Services Posts", "Guam", "Puerto Rico", "Unknown")) )%>% 
    pull(state)
  
  # create a dataframe of all the states
  state_names = data.frame(state = states)
  
  # create a temp data frame with lat/long measurements for each state
  test <- state_names %>%
    left_join(urbnmapr::states, by = c("state" = "state_name")) %>%
    # create new value column indicating if each state should be highlights (0,1)
    mutate(
      relevant = case_when( state %in% relevant_states ~ 1,
                           TRUE ~ 0)
    )
  
  # get positions for labels
  state_labels <- get_urbn_labels(map = "states")

  # plot map
  out <- ggplot(test, mapping = aes(long, lat, group = group, fill = relevant)) +
    geom_polygon(color = "#ffFFff", size = .25) + # line color and width
    geom_text(data = state_labels, aes(long, lat, label = state_abbv), inherit.aes=FALSE, size = 3) +
    scale_fill_gradient(
      low = "#cbd2d4",
      high = high_color,
      guide = guide_colorbar(title.position = "top")) +
    coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
    theme(legend.title = element_text(),
          legend.key.width = unit(.5, "in"),
          legend.position = "none",
          rect = element_blank(),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks = element_blank()) +
    ggtitle(title) + 
    theme(plot.title = element_text(face ="bold", hjust = 0.5))
  
  # return!
  return(out)
}


# only include the 50 states
states <- by_state %>% subset(! (state %in% 
                                   c("Total", "U S  Territories1", "U S  Armed Services Posts", "Guam", "Puerto Rico", "Unknown")) )%>% 
  pull(state)

# create a dataframe of all the states
state_names = data.frame(state = states)

# create a temp data frame with lat/long measurements for each state
test <- state_names %>%
  left_join(urbnmapr::states, by = c("state" = "state_name")) %>%
  left_join(by_state %>% select(state, Global), by = "state" ) %>%
  # create new value column indicating if each state should be highlights (0,1)
  mutate(
    relevant = case_when( state %in% top_ten ~ 0,
                          TRUE ~ as.numeric(Global))
  )

heatmap_state <- function(by_state, relevant_states, high_color, title){
  # only include the 50 states
  states <- by_state %>% subset(! (state %in% 
                                     c("Total", "U S  Territories1", "U S  Armed Services Posts", "Guam", "Puerto Rico", "Unknown")) )%>% 
    pull(state)
  
  # create a dataframe of all the states
  state_names = data.frame(state = states)
  
  # create a temp data frame with lat/long measurements for each state
  test <- state_names %>%
    left_join(urbnmapr::states, by = c("state" = "state_name")) %>%
    left_join(by_state %>% select(state, Global), by = "state" ) %>%
    # create new value column indicating if each state should be highlights (0,1)
    mutate(
      relevant = case_when( state %in% relevant_states ~ as.numeric(Global),
                            TRUE ~ 0)
    )
  
  # get positions for labels
  state_labels <- get_urbn_labels(map = "states")
  
  # plot map
  out <- ggplot(test, mapping = aes(long, lat, group = group, fill = relevant)) +
    geom_polygon(color = "#ffFFff", size = .25) + # line color and width
    geom_text(data = state_labels, aes(long, lat, label = state_abbv), inherit.aes=FALSE, size = 3) +
    scale_fill_gradient(
      low = "#cbd2d4",
      high = high_color,
      guide = guide_colorbar(title.position = "top")) +
    coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
    theme(legend.title = element_text(),
          legend.key.width = unit(.5, "in"),
          legend.position = "none",
          rect = element_blank(),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks = element_blank()) +
    ggtitle(title) + 
    theme(plot.title = element_text(face ="bold", hjust = 0.5))
  
  # return!
  return(out)
}
  
# highest number of newly natz
by_state <- read.csv("out/final tables/appendix-table-1.csv")
top_ten <- by_state %>% arrange(by = desc(Global)) %>% slice(2:11) %>% pull(state)
map_state(by_state, top_ten, "#3f93a6", "HIGHEST NUMBER OF NEWLY NATURALIZED BY STATE")
heatmap_state(by_state, top_ten, "#e34a42", "")

by_state$Global
# STATES OF IMPACT BASED ON 2020 PRES ELECTIONS
pres_elections <- c("Georgia", "Arizona", "Nevada", "Pennsylvania", "Florida")
map_state(by_state, pres_elections, "#3f93a6", "")

# STATES OF IMPACT BASED ON SENATE ELECTIONS
senate_elections <- c("Georgia")
map_state(by_state, senate_elections, "#e34a42", "")

# politically impt states
pis <- c("Georgia", "Arizona", "Nevada", "Pennsylvania", "Florida", "North Carolina", 
         "Wisconsin", "Texas", "Michigan", "Virginia")
map_state(by_state, pis, "#3f93a6", "" )


# ============
# world map
library(choroplethr)
library(choroplethrMaps)
un_regions <- read.csv("data/UNSD_Methodology.csv")
data(country.map, package = "choroplethrMaps")

# create dataframe to plot
plot_data <-country.map %>% select(c(lat, long, group, region_un, region, formal_en))

# plot world map, colored by region
ggplot(plot_data, mapping = aes(long, lat, group = group, fill = region_un)) +
  geom_polygon( size = 0) +  # line color and width 
  theme(legend.position = "none",
        rect = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank()) +
  theme(plot.title = element_text(face ="bold", hjust = 0.5)) +
  scale_fill_manual(values = c(
    "#c73c65",
    "#234aa6",
    "#ffffff",
    "#252078",
    "#8bc8e0",
    "#46898c",
    "#ffffff"
  ))

# get number of natz for each region
by_region = data.frame(t(by_state[1,])) %>% rownames_to_column("region") %>% 
  slice(2:6) %>% rename(natz = X1)

