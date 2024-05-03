##Per the syllabus instructions, I would like this homework to be graded and count towards
  #my class score.



install.packages("rvest")
require(rvest)

#TASK 1

#Build a function that extracts the (1) title, (2) date, (3) text (4) URL
#of all of the documents that relate to Mexico. Scrape this information and store it in a Tibble.

url <-  "https://history.state.gov/historicaldocuments/frus1862/ch12"
website <- read_html(url)  
website 


headline = site %>% html_node('head')
headline
class(headline)

library(tidyverse)


url <-  "https://history.state.gov/historicaldocuments/frus1862/d597"
site <- read_html(url)  



link <- website %>% html_nodes("a") %>% html_attr("href")
link = link[41:69]  #Indexing the links to the actual documents.
link

title = site %>% html_nodes("tei-hi1.font-italic") %>% html_text(.)
title = title[1]

date = site %>% html_nodes(".tei-date") %>% html_text(.)



story = site %>% html_nodes(".tei-p2") %>% html_text(.) %>%
  paste0(.,collapse = " ")
story 


##Building the function:

scraper <- function(url){ 
  #  website   
  raw = read_html(url)
  # headline
  headline = raw %>% 
    html_node(".tei-hi1.font-italic") %>% 
    html_text(.)
  headline = headline[1]
  
  # date
  date = raw %>% 
    html_node('.tei-date') %>% 
    html_text(.)
  # Story
  story = raw %>% 
    html_nodes('.tei-p2') %>%  html_text(.) %>%
    paste0(.,collapse = " ")  
  data.out = tibble(headline,date,story, url)
  return(data.out)
}

x <- scraper(url)
x


output <- c()

for (i in 1:length(link)){
  address <- paste("https://history.state.gov", link[i], sep="")
  object <- scraper(address)
  output <- bind_rows(output, object)
  
}

output

#TASK 2

install.packages("rtweet")
require("rtweet")
library(ggplot2)
library(dplyr)

install.packages("tidytext")
require("tidytext")




appname <- "lillys_r_project"


key <- "PEMFNIFN6YR64Ud90otfIrWlc"


secret <- " P5MnAzSrqLsVnleC09qJkya8kotzWaTZ25UZHSBBBVd7z7Y1LW"

access_token <- "1460351889308930053-2V3BAWa2imyOAITtjawqbae8WXXaA7"
access_secret <- "lIXnS8HQbmEHVzMrJCHZT5JrvXVtIyn5FtCmjlNR2Ksxz"

#Interface key:
twitter_token <- create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secret,
  access_token = access_token,
  access_secret = access_secret)


