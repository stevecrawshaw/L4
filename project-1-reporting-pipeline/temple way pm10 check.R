library(openair)
library(tidyverse)

tw <- importAURN(site = c("BR11", "BRS8"), year = 2018:2022, pollutant = "pm10")

tw %>% 
    select(-code) %>% 
    # group_by(year = year(date), site) %>% 
    timeAverage(avg.time = "day", type = "site", data.thresh = 75) %>% 
    group_by(year = year(date), site) %>% 
    summarise(exc = sum(pm10 > 50, na.rm = TRUE),
              perc = quantile(pm10, 0.904, na.rm = TRUE))
