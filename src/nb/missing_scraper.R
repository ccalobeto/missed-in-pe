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

  remDr <- rd[["client"]] # nolint
  urls <- c()
  for (section in sections){
    new_url <- paste0(url, section)
    remDr$navigate(new_url)
    # open the page
    html <- remDr$getPageSource()[[1]]
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

scrape_citizens <- function(urls) {

  rd <- rsDriver(browser = "firefox",
                 chromever = NULL,
                 verbose = FALSE,
                 port = free_port())

  remDr <- rd[["client"]] # nolint
  citizens <- list()
  for (i in seq(1, length(urls))){
    # open the page
    remDr$navigate(urls[i])
    html <- remDr$getPageSource()[[1]]
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
citizens <- scrape_citizens(missed_urls)

# write json file from dataframe
# ** write(toJSON(citizens), "citizens.json")

get_coordinates <- function(addresses) {
  # convert input into a list
  search_query_url <- sapply(addresses, as.list)
  # percent-encode search request
  search_query_url <- sapply(search_query_url, URLencode)
  url_api <- "https://www.google.com/maps/search/"
  # construct search request for geocode
  urls <- paste0(url_api, search_query_url)
  n <- length(addresses)
  coordinates <- list()
  # to avoid block
  user_a <- user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 12_0_1) AppleWebKit/537.36 (KHTML, # nolint
                        like Gecko) Chrome/95.0.4638.69 Safari/537.36")
  proxy <- use_proxy(url = "185.199.229.156",
                     port = 7492,
                     username = "ambrosio",
                     password = "gikpyh0vnhoj")
  for (i in seq(1:n)) {
    #* web_session <- session(urls[i], user_a)
    #* html_page <- getURL(web_session$url)
    #* content <- read_html(html_page) %>% html_nodes("meta") %>% html_attr("content") # nolint
    response <- GET(urls[i], user_a, proxy)
    xml_doc <- response %>% content(as = "text") %>% read_html
    vector_char <- xml_doc %>% html_nodes("meta") %>% html_attr("content")
    link <- vector_char[str_detect(vector_char, "^https")][1]
    temp <- str_match(link,
                      'center=(-?\\d+\\.\\d+)%2C(-?\\d+\\.\\d+)')[, -1, drop = FALSE] # nolint
    coordinates[[i]] <- temp[1, ]
    Sys.sleep(.5)
  }
  print(response$request)
  # convert list of lists to dataframe
  coordinates <- as.data.frame(do.call(rbind, coordinates))
  names(coordinates) <- c("latitude", "longitude")
  return(coordinates)
}

coordinates <- get_coordinates(citizens[1:20, 10])


## test crawling pages
url <- "https://desaparecidos.policia.gob.pe/Registrogeneral.aspx#"
rd <- rsDriver(browser = "firefox",
               chromever = NULL,
               verbose = FALSE,
               port = free_port())

remDr <- rd[["client"]] # nolint: object_name_linter.
remDr$navigate(url)

# initialize variables
tbl <- data.frame()
section <- 1

# wait until the page is already downloaded and then grab the pages per section
if (section == 2) {
  remDr$findElements(using = "link text", "...")[[1]]$clickElement()
} else if (section > 2) {
  remDr$findElements(using = "link text", "...")[[2]]$clickElement()
}
if (section > 1) {
  old_page <- html_xml %>%
    html_nodes("table.mGrid") %>%
    html_nodes("table") %>%
    html_nodes("span") %>%
    html_text()
  next_page <- old_page
  while (next_page == old_page) {
    html_xml <- remDr$getPageSource()[[1]] %>% read_html()
    next_page <- html_xml %>%
      html_nodes("table.mGrid") %>%
      html_nodes("table") %>%
      html_nodes("span") %>%
      html_text()
  }
} else if (section == 1) {
  html_doc <- remDr$getPageSource()[[1]]
  html_xml <- read_html(html_doc)
}

# scrape the page numbers and remove "..."
pages <- html_xml %>%
  html_nodes("table.mGrid") %>%
  html_nodes("table") %>%
  html_nodes("a, span") %>%
  html_text()
pages <- pages[!pages == "..."]

# extract info
for (page in pages) {
  if (!grepl("1$", page)) {
    # wait until the next page in the form is already downloaded
    old_page <- html_xml %>%
      html_nodes("table.mGrid") %>%
      html_nodes("table") %>%
      html_nodes("span") %>%
      html_text()
    remDr$findElements(using = "link text", page)[[1]]$clickElement()
    next_page <- old_page
    Sys.sleep(0.1)
    while (next_page == old_page) {
      html_xml <- remDr$getPageSource()[[1]] %>% read_html()
      next_page <- html_xml %>%
        html_nodes("table.mGrid") %>%
        html_nodes("table") %>%
        html_nodes("span") %>%
        html_text()
    }
  }
  temp <- html_xml %>% html_nodes("table.mGrid") %>% html_table() %>% .[[1]]
  temp <- temp[1:15, 1:5]
  # temp$page <- page
  tbl <- rbind(tbl, temp)
}
section <- section + 1