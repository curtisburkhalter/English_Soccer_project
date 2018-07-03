#read in required libraries
suppressMessages(library(tidyverse))
suppressMessages(library(here))
suppressMessages(library(googlesheets))
suppressMessages(library(rvest))

#specify the url to retrieve information
url <- "http://www.footstats.co.uk/index.cfm?task=league_goals"

#set up a page session
pgsession <- html_session(url)

#set the page form in 'pgform' because there are a series of dropdowns; returns empty list
pgform <-html_form(pgsession)

filled_form <-set_values(pgform,
                         'league' = "English Premier",
                         'venue' = "All",
                         'season' = "2017/2018" 
)


d <- submit_form(session=pgsession, form=filled_form)

y <- d %>%
  html_nodes("strong") %>%
  html_text()

web <- read_html(url)
rankings <- html_nodes(web,'td')
rank_data <- html_text(rankings)
rank_data
