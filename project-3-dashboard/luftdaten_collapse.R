library(tidyverse)
library(collapse)
library(vroom)
library(here)


colspec <- cols(
  date_time = col_datetime(format = ""),
  nox = col_double(),
  no2 = col_double(),
  no = col_double(),
  siteid = col_double(),
  pm10 = col_double(),
  nvpm10 = col_double(),
  vpm10 = col_double(),
  nvpm25 = col_double(),
  pm25 = col_double(),
  vpm25 = col_double(),
  co = col_double(),
  o3 = col_double(),
  so2 = col_double(),
  temp = col_double(),
  rh = col_double(),
  press = col_double(),
  location = col_character(),
  geo_point_2d = col_character(),
  datestart = col_datetime(format = ""),
  dateend = col_logical(),
  current = col_logical(),
  instrumenttype = col_character()
)

aq_raw <- read_delim("https://opendata.bristol.gov.uk/explore/dataset/air-quality-data-continuous/download/?format=csv&disjunctive.location=true&refine.date_time=2021&timezone=GMT&lang=en&use_labels_for_header=false&csv_separator=%3B", delim = ";", col_types = colspec)

names(aq_raw)

keepvars <- c("siteid", "location", "date_time", "no2", "pm10", "pm25")
pollvars <- keepvars[-3:-1]
pmvars <- c("pm10", "pm25") 

daily_mean <- aq_raw %>% 
    get_vars(keepvars) %>% 
    fgroup_by(date = as.Date(date_time), siteid, location) %>% 
    collapg()

perc_904 <- function(x){
    quantile(x, 0.904, na.rm = TRUE)
}

dc <- function(x){
return(sum(!is.na(x)) /length(x) * 100)
}

pm_perc_904 <- daily_mean %>% 
    fgroup_by(siteid, location) %>% 
    get_vars(pmvars) %>% 
    collapg(FUN = list("pc904" = perc_904, "datacap" = dc)) %>% 
    filter(if_any(.cols = starts_with("pc"), .fns = ~!is.na(.)))
    
    
