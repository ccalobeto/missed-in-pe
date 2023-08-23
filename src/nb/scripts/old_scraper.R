library(RSelenium)
library(netstat)
library(tidyverse)
library(rvest)
library(RCurl)
library(stringr)
library(httr)


get_missed_urls <- function(url, sections) {
  # scrape a list of urls
  rd <- rsDriver(browser = "firefox",
                 chromever = NULL,
                 verbose = FALSE,
                 port = free_port())

  remdr <- rd[["client"]]
  urls <- c()
  for (section in sections){
    new_url <- paste0(url, section)
    remdr$navigate(new_url)
    # open the page
    html <- remdr$getPageSource()[[1]]
    # list of suffix. They are every 5 elements
    suffix <- read_html(html) %>%
      html_nodes(".card-body a") %>%
      html_attr("href")
    suffix <- suffix[-1]
    suffix <- suffix[seq(1, length(suffix), 5)]
    temp <- paste0(url, suffix)
    urls <- c(temp, urls)
  }
  Sys.sleep(2)
  rd$server$stop()
  return(urls)
}

sections <- c("/Desaparecidos/mujer_desaparecido",
              "/Desaparecidos/menor_desaparecido")
url <- "https://desaparecidosenperu.policia.gob.pe"

# gather the urls
missed_urls <- get_missed_urls(url, sections)

scrape_citizens_details <- function(urls) {

  rd <- rsDriver(browser = "firefox",
                 chromever = NULL,
                 verbose = FALSE,
                 port = free_port())

  remdr <- rd[["client"]]
  citizens <- list()
  for (i in seq(1, length(urls))){
    # open the page
    remdr$navigate(urls[i])
    html <- remdr$getPageSource()[[1]]
    raw <- read_html(html) %>%
      html_nodes(".detalle-desaparecidos-p1 b") %>%
      html_text()
    # keep a slice of data
    raw <- raw[1:20]
    # make the list of lists
    citizens[[i]] <- raw[seq(2, length(raw), 2)]
    Sys.sleep(0.1)
  }
  # capture the name of columns
  columns <- raw[seq(1, length(raw), 2)]
  columns <- str_squish(gsub(":", "", columns))
  # convert to dataframe
  citizens <- as.data.frame(do.call(rbind, citizens))
  names(citizens) <- columns
  Sys.sleep(2)
  rd$server$stop()
  return(citizens)
}

# gather a dataframe of citizens
citizens_details <- scrape_citizens_details(missed_urls)

# write json file from dataframe
# ** write(toJSON(citizens), "citizens_sample.json")