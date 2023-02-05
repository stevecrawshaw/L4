p <- c("httr2", "jsonlite", "tidyverse", "config")
library("pacman")
p_load(char = p)

pat <- get(config = "teltonika")$pat

url <- "https://rms.teltonika-networks.com/api/devices?limit=10&status=online&model=RUT950"
shorturl <- "https://rms.teltonika-networks.com/api/devices"

req <- request(url)

response <- req %>% 
  req_headers(Accept = "application/json") %>% 
  req_auth_bearer_token(pat) %>% 
  req_perform()

content <- response %>% 
  resp_body_json() %>% 
  pluck("data")

notags <- map(content, ~chuck(.x, "tags"))

library(rlist)
l <- list.cbind(content) %>%
  as.data.frame() %>% 
rownames_to_column()

l
