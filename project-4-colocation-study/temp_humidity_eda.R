pacman::p_load(tidyverse, openair, janitor, glue, lubridate)

brs8 <- importAURN(site = "brs8", pollutant = "all", meta = TRUE, year = 2022)

brs8 %>% 
    head()
br11 <- importAURN(site = "br11", pollutant = "all", meta = TRUE, year = 2022)

br11

temp_diff_tbl <- brs8 %>% 
    inner_join(br11, by = join_by(date == date)) %>% 
    select(date, code.x, code.y, air_temp.x, air_temp.y) %>% 
    mutate(t_diff = (air_temp.x - air_temp.y)) %>% 
    filter(t_diff != 0)
# looks like air temp is from a model for AURN data 


get.temp.rh.raw.tbl <- function(start_date, end_date, sensor_id = "71553"){
    
col_spec = cols(
  sensor_id = col_double(),
  sensor_type = col_character(),
  location = col_double(),
  lat = col_double(),
  lon = col_double(),
  timestamp = col_datetime(format = ""),
  temperature = col_double(),
  humidity = col_double()
)

dates <- seq.Date(from = as.Date(start_date), 
                  to = as.Date(end_date), 
                  by = "day") %>% 
    as.character()

filenames = glue("https://archive.sensor.community/{dates}/{dates}_dht22_sensor_{sensor_id}.csv")

rh_temp_tbl_raw <- map_dfr(filenames, \(x) read_delim(x, 
                                                      delim = ";",
                                                      col_types = col_spec))
return(rh_temp_tbl_raw)
    
}

make_rh_temp_tbl <- function(rh_temp_tbl_raw){

rh_temp_tbl <- rh_temp_tbl_raw %>% 
    group_by(date = lubridate::ceiling_date(timestamp, unit = "hour")) %>% 
    summarise(temperature = mean(temperature, na.rm = TRUE),
              humidity = mean(humidity, na.rm = TRUE))

return(rh_temp_tbl)
}

