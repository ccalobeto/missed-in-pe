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
df <- df_[order(df_$name,
                df_$event_date,
                desc(df_$missing_state)), ] %>%
  group_by(name) %>%
  mutate(cum_sum = cumsum(missing_state))
# label missing data
df[df$name == "", "error_label"] <- "no name"
df[df$event_place == "", "error_label"] <- "no place"
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
# convert long to pivot data
df0 <- df[df$missing_state == 0, ]
df1 <- df[df$missing_state == 1, ]
output <- merge(x = df1, y = df0, by = "name", all.x = TRUE)
# test
write.csv(df, "output.csv", row.names = FALSE)


# testing
duplicates <- df_ %>%
  group_by(name) %>%
  summarise(n = n()) %>%
  filter(n > 1)

# get coordinates
coordinates <- get_google_coordinates(df[1:20, 5])
