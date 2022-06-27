# ___________ ----
# 1.0 Libraries ----
p <- c("tidyverse", "lubridate", "httr2", "jsonlite", "glue", "janitor", "fs")
library(xfun)
pkg_attach2(p)
rm(p)

source("../airquality_GIT/importODS.R")
#____________----
# 2.0 Variables ----
data_root_url <- "https://api-rrd.madavi.de/data_csv/"

# url <-  "https://api-rrd.madavi.de/csvfiles.php?sensor=esp8266-6496445"
# base_url <- "https://api-rrd.madavi.de/csvfiles.php"
sensor <- "esp8266-6496445"
start_date <- "2022-04-23"
# ___________ ----
# 3.0 Functions ----

# 3.1 Get Sensor Data ----

# 3.1.1 Get data from the sensor that is not registered on the 
# sensor.community. This is available through the madavi.de API
# as csv and zip files
get_zip_file_urls <- function(data_root_url, sensor, start_date){
    previous_month <- (Sys.Date() - dmonths(1)) %>% as.Date()
    
    seq_dates <- seq.Date(from = start_date %>% as.Date(),
                          to = previous_month, by = "month")
    
    zip_months <- strftime(seq_dates, "%m")
    zip_years <- strftime(seq_dates, "%Y")
    
    zip_file_urls <- glue("{data_root_url}{zip_years}/{zip_months}/data-{sensor}-{zip_years}-{zip_months}.zip")
    
    return(zip_file_urls)
}

get_daily_csv_urls <- function(data_root_url, sensor){
    first_of_month <- strftime(Sys.Date(), "%Y-%m-01") %>% as.Date()
    
    seq_dates_curr_month <- seq.Date(from = first_of_month,
                                     to = Sys.Date(), by = "day")
    
    glue("{data_root_url}csv-files/{seq_dates_curr_month}/data-{sensor}-{seq_dates_curr_month}.csv")
    
}

read_sds_csv <- function(filename){
    
    #fname <- glue("../air quality analysis/data/{filename}")
    colspec <- cols_only(
        Time = col_datetime(format = "%Y/%m/%d %H:%M:%S"),
        SDS_P1 = col_double(),
        SDS_P2 = col_double(),
        Samples = col_integer(),
        Min_cycle = col_integer(),
        Max_cycle = col_integer(),
        Signal = col_integer())
    
    read_delim(filename, delim = ";", col_types = colspec) %>% 
        return()
    
}

read_sds_zip <- function(zip_url){
    
    tempdir <- "../air quality analysis/data/sds_csv/"
    destfile <-  "../air quality analysis/data/sds_csv/temp.zip"
    download.file(zip_url, destfile)
    
    unzip(destfile, exdir = tempdir)
    
    csv_from_zip <- fs::dir_ls(tempdir, regexp = "[.]csv$")
    
    zip_file_tbl <- csv_from_zip %>% 
        map_df(~read_sds_csv(.x))
    
    fs::file_delete(dir_ls(tempdir))
    
    return(zip_file_tbl)
    
}

# 3.1.2 Get sensor data from the sensor at Parson Street which is registered on sensor.community and also on the open data portal

sensor_id <- "71552"
dataset_id <- "luftdaten_pm_bristol"

ps_raw_tbl <- getODSExport(select_str = "sensor_id, date, pm2_5",
                              where_str = "sensor_id = 71552",
                              date_col = "date",
                              dateon = start_date,
                              dateoff = Sys.Date(),
                              dataset = dataset_id,
                              order_by = "date",
                              refine = NULL,
                              apikey = NULL
                              )

parson_st_sds_tbl <- ps_raw_tbl %>% 
    select(date, pm2.5 = pm2_5) 
    
start_ref_date <- parson_st_sds_tbl$date %>% min() %>% as.Date()

# 3.2 Get Reference Data ----

raw_ref_tbl <- getODSExport(select_str = "siteid, date_time, pm25, pm10",
             where_str = "(siteid = 215 OR siteid = 500) AND date_time IN [date'2022'..date'2022']",
             date_col = NULL,
             dateon = NULL,
             dateoff = NULL,
             dataset = "air-quality-data-continuous",
             order_by = "siteid, date_time",
             refine = NULL,
             apikey = NULL
)

ref_tbl <- raw_ref_tbl %>% 
    filter(date_time >= start_ref_date) %>% 
    rename(date = date_time,
           pm2.5 = pm25)
# %>% 
#     pivot_wider(id_cols = date, names_from = siteid, values_from = c(pm10, pm2.5)) %>% 
#     mutate(across(where(~is.na(.x) %>% all()), ~NULL))



day_files_tbl <- get_daily_csv_urls(data_root_url, sensor) %>% 
    map_df(~read_sds_csv(.x))

zip_sds_tbl <- get_zip_file_urls(data_root_url, sensor, start_date) %>% 
    map_df(~read_sds_zip(.x))

combined_tbl <- day_files_tbl %>% 
    bind_rows(zip_sds_tbl)
write_rds(combined_tbl, glue("../air quality analysis/data/{sensor}_{Sys.Date()}_raw.rds"))

temple_way_sds_raw_tbl <- read_rds(choose.files())

temple_way_sds_tbl <- 
    
    temple_way_sds_raw_tbl %>% 
    # select(Time,
           # pm10 = SDS_P1,
           # pm2.5 = SDS_P2) %>% 
        group_by(date = ceiling_date(Time, unit = "hour")) %>% 
        summarise(pm10_sds = mean(SDS_P1, na.rm = TRUE)) 

temple_way_joined_tbl <- temple_way_sds_tbl %>% 
    inner_join(ref_tbl %>%
                   filter(siteid == 500) %>% 
                   select(date, pm10_btw = pm10),
               by = "date")

temple_way_joined_tbl %>% 
    filter(pm10_btw < 100,
           !is.na(pm10_btw),
           !is.na(pm10_sds)) %>% 
    ggplot(aes(x = pm10_btw, y = pm10_sds)) +
    geom_point() +
geom_smooth(method = "lm")
    
# check the madavi data against grafana ----

graf_raw_tbl <- read_csv("../air quality analysis/data/PM 10 - last 24 hours-data-2022-06-27 10_45_40.csv")

graf_hr_tbl <- graf_raw_tbl %>% 
    group_by(date = ceiling_date(Time, unit = "hour")) %>% 
    summarise(pm10_sds = mean(`SDS011 PM10`, na.rm = TRUE))

scatter <- function(joined_hour_tbl){
    joined_hour_tbl %>% 
        ggplot(aes(x = pm10_sds_grafana, y = pm10_sds_madavi)) +
        geom_point() +
        geom_smooth(method = "lm")
}

joined_hour_tbl <- graf_hr_tbl %>% 
    inner_join(temple_way_sds_tbl, by = "date", suffix = c("_grafana", "_madavi")) 

joined_hour_tbl %>%
    pivot_longer(cols = starts_with("pm10")) %>% 
    ggplot(aes(x = date, y = value, colour = name)) +
    geom_line()
    




parson_st_joined_tbl <- ref_tbl %>% 
    filter(siteid == 215,
           pm2.5 < 800) %>% 
    select(date, pm2.5) %>% 
    inner_join(parson_st_sds_tbl,
               by = "date",
               suffix = c("_215", "_sds")) 

parson_st_joined_tbl %>% 
    ggplot(aes(x = pm2.5_215, y = pm2.5_sds)) +
    geom_point() +
    geom_smooth(method = "lm")


#parson_st_joined_tbl %>% #
    cor(x = parson_st_joined_tbl$pm2.5_215, y = parson_st_joined_tbl$pm2.5_sds, use = "complete.obs", )
