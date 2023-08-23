library(RSelenium)
library(netstat)
library(tidyverse)
library(rvest)
library(RCurl)
library(stringr)
library(httr)

get_google_coordinates <- function(addresses) {
  ## return coordinates(lat, lon) given an input list of addresses
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

coordinates <- get_google_coordinates(citizens_details[1:20, 10])