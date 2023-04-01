wants <- c("tidyverse", "data.table", "here", "openair", "stringr", "lubridate")
has   <- wants %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has])
lapply(wants, library, character.only=T)

ods_url <- "https://opendata.bristol.gov.uk/explore/dataset/luftdaten_pm_bristol/download/?format=csv&refine.date=2019&timezone=Europe/London&lang=en&use_labels_for_header=false&csv_separator=%3B"

ldDT <- fread(ods_url)[, `:=`(date = as_datetime(date))]

central_locs_url <- "https://opendata.bristol.gov.uk/explore/dataset/luftdaten_locations_bristol/download/?format=csv&geofilter.polygon=(51.46095939981568,-2.6010990142822266),(51.48720886639676,-2.6010990142822266),(51.48720886639676,-2.5599002838134766),(51.46095939981568,-2.5599002838134766),(51.46095939981568,-2.6010990142822266)&timezone=Europe/London&lang=en&use_labels_for_header=false&csv_separator=%3B"

central_sites <- fread(central_locs_url)[, sensor_id]
    

sites <- c(7685, 11068)

ldDT %>% 
    add_count(sensor_id) %>% 
    filter(sensor_id %in% central_sites) %>% 
    ggplot(aes(x = date, y = pm10)) + 
    geom_line() + 
    facet_wrap(~ sensor_id, scales = "free_y")


ldDT %>% 
    cutData(type = "season") %>% 
    filter(sensor_id %in% 11068) %>%
    timeVariation(pollutant = "pm10", type = "season", ci = F)




ldDT %>% 
    cutData(type = "season")
    
    
    ggplot(aes(x = date, y = pm10)) + 
    geom_smooth() + 
    facet_wrap(~ sensor_id, scales = "free_y")
filter(dense_rank(desc(n)) <= 10) %>%
    select(-n) %>% 


summarise(mean(pm10, na.rm = T)) %>% View()
    
    frankv(ldDT, cols = "sensor_id")

timeVariation(ldDT, type = "sensor_id", )