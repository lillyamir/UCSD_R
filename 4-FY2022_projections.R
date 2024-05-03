library(tidyverse)
library(forecast)

# ====================
#read in quarterly data
dat <- read.csv("data/quarterly_natz.csv")
yearly_natz <- read.csv("data/yearly_natz.csv")
yearly_natz$x2016_2022 <- rowSums(yearly_natz[,7:13], na.rm = T)

# estimated from 2016-2022
(yearly_natz %>% select(x2016_2022))[1,]

# naturalized since trump's elections
(yearly_natz %>% select(x2016_2022))[1,] - sum((newly_natz %>% subset(Year == 2016 & (Quarter < 4)))$Applications.Approved)

# =======================
#read in quarterly data
quarterly_natz_ohio <- read.csv("data/quarterly_natz_ohio.csv")

# ======================

