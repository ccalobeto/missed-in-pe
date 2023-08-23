library(RSelenium)
library(netstat)
library(tidyverse)
library(rvest)
library(RCurl)
library(stringr)
library(httr)

source(scripts/coordinates_scraper.R)
## prepare dataset
# import data
df0 <- read.csv("../data/scraped_missed.csv")
df <- read.csv("../data/scraped_appeared.csv")
  
duplicates <- raw_citizens %>%
  group_by(name) %>%
  summarise(n = n()) %>%
  filter(n > 1)
raw_citizens$event_place <- toupper(raw_citizens$event_place)

#order make fake data
str(raw_citizens)

# labeling wrong rows
citizens <- raw_citizens[order(raw_citizens$name,
                               desc(raw_citizens$missing_state),
                               desc(raw_citizens$event_date)), ]
citizens <- citizens %>%
  group_by(name, missing_state) %>%
  mutate(numbering = row_number())
citizens[citizens$numbering > 1, "error_label"] <- "duplicated name"
citizens[citizens$name == "" | citizens$event_place == "",
         "error_label"] <- "no data"

# separate two dataframes
cleaned <- citizens %>% filter(is.na(error_label))
cleaned <- cleaned[, c("name", "event_date", "event_place", "missing_state")]
cleaned_0 <- cleaned[cleaned$missing_state == 0, ]
cleaned_1 <- cleaned[cleaned$missing_state == 1, ]
output <- merge(x = cleaned_1, y = cleaned_0, by = "name", all.x = TRUE)
