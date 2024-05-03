setwd("/Users/lillyamirjavadi/Desktop/P178_W1")

require(tidyverse)
require(gapminder)


gapminder <- gapminder

head(gapminder)

#TASK 1: SUMMARIZE

summary(gapminder)

#Temporal Coverage: The temporal coverage spans from 1952 to 2007.

#Continents: The dataset covers the Americas, Oceania, Europe, Asia, and Africa.

#Population/Life Expectancy Dispersion: The minimum value for population is 60011 people, the mean is 29,600,000
    #people, and the maximum is 1,319,000,000. The values for population do span over all years. The Life
    # Expectancy distribution has a minimum value of 23.60 years, a mean of 59.47, and a maximum value of 82.60.
    #The standard deviation for population was 106157897, and the standard deviation for life expectancy is 12.91711.

sd(gapminder$lifeExp)
sd(gapminder$pop)


#TASK 2: SELECT

#a. only the lifeExp and population columns

select(gapminder, lifeExp, pop)

#b. only the columns where the header contains the letter “c”

select(gapminder, contains("c"))

#c. all columns except lifeExp.

select(gapminder, -lifeExp)


#TASK 3: FILTER

afr_gapminder <- gapminder %>% filter(continent=="Africa" & year>1990)
afr_gapminder


#TASK 4: Arranging/Slicing


entry_obs <- group_by(gapminder, country) %>% slice(1)
entry_obs


sum(entry_obs$pop)
  #The sum is 2406957150

#SLICE WILL get 1st year

#TASK 5: HOW MUCH HAS LIFEXP CHANGED OVER TIME?



mutate(gapminder, 
       prior = lag(pop, k=5),
       change = pop - prior)



#TASK 6: LONG DATA

long_gap <- gapminder

long_gap %>% pivot_longer(cols = c("pop", "year", "lifeExp"),
                          names_to = "values", values_to = "count")




