library(tidyverse)
source("scripts/coordinates_scraper.R")

# import scraped datasets
df_ <- read.csv("data/out/scraped_missed.csv")
df <- read.csv("data/out/scraped_appeared.csv")
df <- rbind(df_, df)
  

df$event_date <- as.Date(df$event_date, "%d/%m/%Y %H:%M:%S")
df$event_place <- toupper(df$event_place)

duplicates <- df %>%
  group_by(name) %>%
  summarise(n = n()) %>%
  filter(n > 1)


# clean data
# order make fake data
str(df)

# labeling wrong rows
df <- df[order(df$name,
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

# get coordinates 
coordinates <- get_google_coordinates(df[1:20, 5])
