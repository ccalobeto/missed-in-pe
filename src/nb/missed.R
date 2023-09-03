library(tidyverse)
source("scripts/coordinates_scraper.R")
lintr::lint_dir()

# import scraped datasets
df1 <- read.csv("data/out/scraped_missed.csv")
df0 <- read.csv("data/out/scraped_appeared.csv")
df_ <- rbind(df1, df0)

# set category label: no name, no place, duplicated entry and inconsistent.
label_duplicated_entry <- function(df, columns) {
  df %>%
    group_by(pick({{ columns }})) %>%
    mutate(numbering = row_number()) %>%
    mutate(error_label = ifelse(numbering > 1, "duplicated_entry", error_label))
}

calculate_missed_averages <- function(df, columns, avg_period) {
  df %>%
    filter(missing_state == 1) %>%
    count(pick({{ columns }})) %>%
    select(year, n) %>%
    group_by(year) %>%
    summarise("{{ avg_period }}" = mean(n))
}

df <- df_[order(df_$name,
                df_$event_date,
                desc(df_$missing_state)), ] %>%
  group_by(name) %>%
  mutate(cum_sum = cumsum(missing_state))
# label missing data
df[df$name == "", "error_label"] <- "no name"
# label duplicated entries
df <- df %>%
  label_duplicated_entry(c(name, cum_sum, missing_state))
# separate mistakes
mistakes <- df %>%
  ungroup() %>%
  filter(!is.na(error_label)) %>%
  select(name, event_date, event_place, missing_state, error_label)
# keep clean data
df <- df %>%
  ungroup() %>%
  filter(is.na(error_label)) %>%
  select(name, event_date, event_place, missing_state)
# detecting another duplicated entries: shifting the missing_state
# and calculate the difference
df <- df %>%
  group_by(name) %>%
  mutate(diff = missing_state - lag(missing_state, default = NULL))
df <- df %>% mutate(error_label = ifelse(diff == 0, "duplicated_entry", NA))
more_mistakes <- df %>%
  ungroup() %>%
  filter(!is.na(error_label)) %>%
  select(name, event_date, event_place, missing_state, error_label)
df <- df %>%
  ungroup() %>%
  filter(is.na(error_label)) %>%
  select(name, event_date, event_place, missing_state)
mistakes <- rbind(mistakes, more_mistakes)
# detect addresses with "", only numbers and "."
df$event_place_clean <- df$event_place
df[df$event_place_clean %in% c("", NA), "event_place_clean"] <- NA
df[grepl("^\\d+$", df$event_place_clean), "event_place_clean"] <- NA
df[grepl("^\\.$", df$event_place_clean), "event_place_clean"] <- NA
# calculating period variables
df$event_date <- as.Date(df$event_date)
df$year <- year(df$event_date)
df$month <- month(df$event_date)
df$day <- day(df$event_date)
# adding identifier per name
df <- df %>%
  group_by(name) %>%
  mutate(ID = cur_group_id()) %>%
  ungroup()
# metrics
avg_day <- df %>% calculate_missed_averages(c(year, month, day), avg_mpd)
avg_month <- df %>% calculate_missed_averages(c(year, month), avg_mpm)
sum_year <- df %>%
  filter(missing_state == 1) %>%
  group_by(year) %>%
  count(missing_state) %>%
  ungroup() %>%
  rename(mpy = n) %>%
  select(year, mpy)
list_df <- list(avg_day, avg_month, sum_year)
metrics <- list_df %>% reduce(inner_join, by = "year")

# convert long to pivot data
df0 <- df[df$missing_state == 0, ]
df1 <- df[df$missing_state == 1, ]
output <- merge(x = df1, y = df0, by = "name", all.x = TRUE)
# test
write.csv(df, "data/out/output.csv", row.names = FALSE)
write.csv(mistakes, "data/out/mistakes.csv", row.names = FALSE)

# testing
duplicates <- df_ %>%
  group_by(name) %>%
  summarise(n = n()) %>%
  filter(n > 1)

lapply(df_ %>% filter(name == "SORIA LOPEZ DAVID DEYSON"), unlist)

df %>% filter(name == "VILLEGAS TUIRO ROEL ALEJANDRO")

# get coordinates
coordinates <- get_google_coordinates(df[1:20, 5])
