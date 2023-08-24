library(rvest)
library(httr)
library(jsonlite)
library(stringr)

get_google_coordinates <- function(addresses) {
  ## return coordinates(lat, lon) given an input list of addresses
  # import data
  state <- NULL
  agents <- read.csv("../nb/data/support/agents.csv")
  ips <- read.csv("../nb/data/support/ips.csv")
  ips <- ips %>%
    filter(state == 1)
  secrets <- read_json("../nb/data/support/secrets.json")
  # chunk
  chunk <- 5
  # convert input into a list
  search_query_url <- sapply(addresses, as.list)
  # percent-encode search request
  search_query_url <- sapply(search_query_url, URLencode)
  url_api <- "https://www.google.com/maps/search/"
  # construct search request for geocode
  urls <- paste0(url_api, search_query_url)
  n <- length(addresses)
  regex_pattern <- "center=(-?\\d+\\.\\d+)%2C(-?\\d+\\.\\d+)"
  coordinates <- list()
  # select the agent
  user_a <- user_agent(agents$agent[10])
  for (i in seq(1:n)) {
    # iterate proxies every chunk
    if (i == 1 || i %% chunk == 0) {
      random_row <- sample(length(ips$ip), 1)
      pick_ip <- ips$ip[random_row]
      pick_port <- ips$port[random_row]
      proxy <- use_proxy(url = pick_ip,
                         port = pick_port,
                         username = secrets$user,
                         password = secrets$password)
    }
    response <- GET(urls[i], user_a, proxy)
    xml_doc <- response %>% content(as = "text") %>% read_html
    vector_char <- xml_doc %>% html_nodes("meta") %>% html_attr("content")
    link <- vector_char[str_detect(vector_char, "^https")][1]
    temp <- str_match(link, regex_pattern)[, -1, drop = FALSE]
    coordinates[[i]] <- temp[1, ]
    Sys.sleep(.5)
  }
  # convert list of lists to dataframe
  coordinates <- as.data.frame(do.call(rbind, coordinates))
  names(coordinates) <- c("latitude", "longitude")
  return(coordinates)
}
