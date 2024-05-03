getwd()
setwd("/Users/lillyamirjavadi/Desktop/P178_W1")
getwd()

rm(list=ls(all=TRUE))

### In class, I discussed some packages to install:

## Make sure we now load them. 
require(readr)
require(readxl)
require(haven)


############
#### Remeber to set your working directory
###################


# Here is a dataframe of military expendatures and population for 6 countries:
country <- rep(c('Afghanistan', 'Brazil', 'China', 'Australia', 'France', 'Fiji'), each=2)
year <- rep(1999:2000, 6)
mil_exp <-  c(678, 756, 7563, 4389, 1003456, 1306481, 4563,5431, NA, 9342, NA, 134)
population <- c(19987071, 20595360, 172006362, 174504898, 1272915272, 1280428583, 21435654,21012345, 109034567,100203456, 4542, 44534)

dat <- data.frame(country, year, mil_exp, population)




######################
### Task 1: ifelse
##################
# Here is a list of democracies:
dems <-  c("Brazil", "Australia", "France", "Fiji")
dems

# Using the ifelse function create a new variable (democ) equal to 1 if the country is a democracy and 0 otherwise. 

democ <- ifelse(dat$country%in%dems, 1, 0)
democ

############## 
### Task 2: Loops
################

# 1.  We want to normalize the results based on the population average over 2 years. 

# We are going to do it using a loop. (This is not the only way to do it, but we need to practice)
# Write a loop that:
# computes the average population over two years for each country. 
# saves the results.
# Build a dataframe with country  and average population. 

pop2_yr <- c()
for (i in country){
  
  avg_pop<- mean(population[country==i])
  pop2_yr[i] <- print(avg_pop)
  
  }
df <- data.frame(pop2_yr)

df



## 2. Do the same thing for military experience. 
# WITH A TWIST. 
# For each iteration, record whether you found missing data. 
# If you do find missing data, record the result that includes only the data you find. 
# DO NOT use the option na.rm = T. Instead, use an ifelse command. 

mil_exp2 <- c()

for (i in country){
  avg_mil <- mean(mil_exp[country==i])
  mil_exp2[i] <- print(avg_mil)
  
}

df <- data.frame(mil_exp2, pop2_yr)
df$mil_exp2 <- ifelse(is.na(mil_exp2), na.omit(mil_exp2), print(mil_exp2))
df

##found missing data (last 4)

###################
#######   Task 4: Save and load your package. 
#######################

# 4.1 Save your dataframe to your computer as a .csv file. 
# Remember! You want to save it in the directory for data you developed. 

write.csv(df, "/Users/lillyamirjavadi/Desktop/P178_W1/df", row.names = FALSE)

# Run this command to remove all objects/packages from your directory: 

rm(list=ls(all=TRUE))

#4.2 Re-load your packages that you need to read that datafile. 

require(readr)
require(readxl)
require(haven)

# 4.3 Read your dataframe back into R. 

df <- read.csv("/Users/lillyamirjavadi/Desktop/P178_W1/df")

# Make sure it looks right. 

    #Looks good!

df



###########################
# Task 3:  Install the following packages 
# dbplyr
# tidyverse
#############################

library(dbplyr)
library(tidyverse)

