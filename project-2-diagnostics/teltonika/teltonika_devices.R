libs <- c("httr2", "jsonlite", "tidyverse")
library("xfun")
pkg_attach2(libs)
pat <- "eyJ0eXAiOiJKV1QiLCJhbGciOiJSUzI1NiJ9.eyJqdGkiOiI4MmNhNjk5ZTI5ZTM0ZDU2NGM4OGQzZjkzMjBmNThhZjMxNGNlMmRjODkzYzQxOTJjN2JlZmI3ZjU0MWFjY2UzMjM2NjJkOGU0NGIzMjM2ZSIsImlzcyI6Imh0dHBzOlwvXC9ybXMudGVsdG9uaWthLW5ldHdvcmtzLmNvbVwvYWNjb3VudCIsImlhdCI6MTY0MDI3ODc0NiwibmJmIjoxNjQwMjc4NzQ2LCJzdWIiOiI0ODIzMSIsImNsaWVudF9pZCI6IjkxMjNlYTY2LTJmMWQtNDM5Yy1iMWMyLTMxMWFjMDEwYWFhZCIsImZpcnN0X3BhcnR5IjpmYWxzZX0.HO44S5joUevYFvCrRBcMHtZmUMdJ9Jx1D8DgRwO13vq2vWU_aWDJjl4TJx1-KXI0OcrsOKI1Qx1Oq0KXTc7ujtPnfdqZ9Zw-bEpPGHx-Qqo-mfFvx58WRHvLPXf_7zjE3LVP2L9tzrKhky8ZcaDGqZNA0bSHYxHUnbbODYtsYfJX3N9eeQZUdZwMrFU9eUpSh49nHvRhFRt1ejXTd5uxF6w6Ads9OAz9eQTMZwLaviSc6e2JoRrNswg0py8pqmfvjPia_rloNig58PItsz8Q6na3kgum1G_bCwjXGR93Cp8oJ-4X0DQjbSUbgJtrin_ZG-jn_c-3nv3Y5BaFgfGTjg"

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
