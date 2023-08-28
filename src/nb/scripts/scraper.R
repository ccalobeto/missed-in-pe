library(RSelenium)
library(netstat)
library(tidyverse)
library(rvest)

## crawling pages
# set the number of sections you want to scrape. every section has 10 pages
# and 15 citizens
start <- Sys.time()
n <- 2
url_missed <- "https://desaparecidos.policia.gob.pe/Registrogeneral.aspx#"
url_appeared <- "https://desaparecidos.policia.gob.pe/Registroaparecidos.aspx#"

scrape_citizens <- function(url, n, state) {
  rd <- rsDriver(browser = "firefox",
                 chromever = NULL,
                 verbose = FALSE,
                 port = free_port())

  remdr <- rd[["client"]]
  remdr$navigate(url)

  # initialize variables
  tbl <- data.frame()

  sections <- seq(1:n)

  page_has_loaded <- function(remote_driver, old_html_xml, page, index) {
    # returns an xml document when it has already downloaded
    # index: to select which "..." page choose since section 2
    # page: the page you are scraping either "1", "2" or some number or "..."
    old_page <- old_html_xml %>%
      html_nodes("table.mGrid") %>%
      html_nodes("table") %>%
      html_nodes("span") %>%
      html_text()
    remote_driver$findElements(using = "link text",
                               page)[[index]]$clickElement()
    next_page <- old_page
    Sys.sleep(0.1)
    # wait until the next page in the form is already downloaded
    while (next_page == old_page) {
      html_xml <- remote_driver$getPageSource()[[1]] %>% read_html()
      next_page <- html_xml %>%
        html_nodes("table.mGrid") %>%
        html_nodes("table") %>%
        html_nodes("span") %>%
        html_text()
    }
    return(html_xml)
  }

  for (section in sections) {
    # scrape the page numbers and remove "..."
    if (section == 1) {
      html_xml <- remdr$getPageSource()[[1]] %>% read_html()
    } else {
      if (section == 2) {
        html_xml <- page_has_loaded(remdr, html_xml, "...", 1)
      } else if (section > 2) {
        html_xml <- page_has_loaded(remdr, html_xml, "...", 2)
      }
    }
    pages <- html_xml %>%
      html_nodes("table.mGrid") %>%
      html_nodes("table") %>%
      html_nodes("a, span") %>%
      html_text()
    pages <- pages[!pages == "..."]
    # extract info by page
    for (page in pages) {
      # pages that end in 1 are skipped. like 1, 11, 21 etc
      if (!grepl("1$", page)) {
        html_xml <- page_has_loaded(remdr, html_xml, page, 1)
      }
      temp <- html_xml %>%
        html_nodes("table.mGrid") %>%
        html_table() %>%
        .[[1]]
      temp <- temp[1:15, 1:5]
      temp$page <- page
      tbl <- rbind(tbl, temp)
    }
  }
  rd$server$stop()
  tbl$state <- state
  return(tbl)
}

export_dataframe <- function(df, file) {
  library(dplyr)
  police_unit <- name <- event_date <- event_place <- missing_state <- NULL
  columns <- c("police_unit", "name", "event_date", "event_place",
               "report_date", "page", "missing_state")
  names(df) <- columns
  # initial preparation
  df$event_date <- as.Date(df$event_date, "%d/%m/%Y %H:%M:%S")
  df$event_place <- toupper(df$event_place)
  df <- df %>%
    select(police_unit, name, event_date, event_place, missing_state, page)
  write.csv(df, file, row.names = FALSE)
}

citizens_missed <- scrape_citizens(url_missed, n, 1)
export_dataframe(citizens_missed, "data/out/scraped_missed_.csv")

print(Sys.time() - start)

start <- Sys.time()
citizens_appeared <- scrape_citizens(url_appeared, n, 0)
export_dataframe(citizens_appeared, "data/out/scraped_appeared_.csv")

print(Sys.time() - start)
