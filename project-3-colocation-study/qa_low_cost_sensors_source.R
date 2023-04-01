# 1.0 Libraries ----
p <-
    c("tidyverse",
      "lubridate",
      "httr2",
      "jsonlite",
      "glue",
      "janitor",
      "fs",
      "padr")
library(pacman)
p_load(char = p)
rm(p)

source("../../airquality_GIT/ods-import-httr2.R")
#____________----
# 2.0 Variables ----
data_root_url <- "https://api-rrd.madavi.de/data_csv/"

# url <-  "https://api-rrd.madavi.de/csvfiles.php?sensor=esp8266-6496445"
# base_url <- "https://api-rrd.madavi.de/csvfiles.php"
sensor <-
    "esp8266-6496445" # Temple Way - not registered on sensor.community
start_date <- "2022-05-01" # BTW started operating
# ___________ ----
# 3.0 Functions ----

# 3.1 Get Sensor Data ----

# 3.1.1 Get data from the sensor that is not registered on the
# sensor.community. This is available through the madavi.de API
# as csv and zip files
get_zip_file_urls <- function(data_root_url, sensor, start_date) {
    previous_month <- (Sys.Date() - months(1)) %>% as.Date()
    
    seq_dates <- seq.Date(from = start_date %>% as.Date(),
                          to = previous_month,
                          by = "month")
    
    zip_months <- strftime(seq_dates, "%m")
    zip_years <- strftime(seq_dates, "%Y")
    
    zip_file_urls <-
        glue(
            "{data_root_url}{zip_years}/{zip_months}/data-{sensor}-{zip_years}-{zip_months}.zip"
        )
    
    return(zip_file_urls)
}

get_daily_csv_urls <- function(data_root_url, sensor) {
    first_of_month <- strftime(Sys.Date(), "%Y-%m-01") %>% as.Date()
    
    seq_dates_curr_month <- seq.Date(from = first_of_month,
                                     to = Sys.Date(),
                                     by = "day")
    
    glue(
        "{data_root_url}csv-files/{seq_dates_curr_month}/data-{sensor}-{seq_dates_curr_month}.csv"
    )
    
}
# read the daily csv files
read_sds_csv <- function(filename) {
    #fname <- glue("../air quality analysis/data/{filename}")
    colspec <- cols_only(
        Time = col_datetime(format = "%Y/%m/%d %H:%M:%S"),
        SDS_P1 = col_double(),
        SDS_P2 = col_double(),
        Samples = col_integer(),
        Min_cycle = col_integer(),
        Max_cycle = col_integer(),
        Signal = col_integer()
    )
    
    read_delim(filename, delim = ";", col_types = colspec) %>%
        return()
    
}
# read the zip files for each month
read_sds_zip <- function(zip_url) {
    tempdir <- "../air quality analysis/data/sds_csv/"
    destfile <-  "../air quality analysis/data/sds_csv/temp.zip"
    download.file(zip_url, destfile)
    
    unzip(destfile, exdir = tempdir)
    
    csv_from_zip <- dir_ls(tempdir, regexp = "[.]csv$")
    
    zip_file_tbl <- csv_from_zip %>%
        map_df(~ read_sds_csv(.x))
    
    file_delete(dir_ls(tempdir))
    
    return(zip_file_tbl)
    
}
# read the zip and csv files and combine

get_madavi_combined <- function(data_root_url, sensor, start_date) {
    day_files_tbl <- get_daily_csv_urls(data_root_url, sensor) %>%
        map_df(~ read_sds_csv(.x))
    
    zip_sds_tbl <-
        get_zip_file_urls(data_root_url, sensor, start_date) %>%
        map_df(~ read_sds_zip(.x))
    
    combined_tbl <- day_files_tbl %>%
        bind_rows(zip_sds_tbl) %>%
        group_by(date = ceiling_date(Time, unit = "hour")) %>%
        summarise(pm10 = mean(SDS_P1, na.rm = TRUE),
                  .groups = "drop") %>%
        filter(date >= as.POSIXct(start_date))
    
    return(combined_tbl)
}


# 3.1.2 Get sensor data from the sensor at Parson Street which is registered on sensor.community and also on the open data portal

get_parson_st_data <- function(start_date) {
    ps_raw_tbl <- import_ods(
        select = "sensor_id, date, pm2_5",
        where = "sensor_id = 71552",
        date_col = "date",
        dateon = start_date,
        dateoff = Sys.Date(),
        dataset = "luftdaten_pm_bristol",
        order_by = "date"
    )
    ps_raw_tbl %>%
        select(date, pm2.5 = pm2_5) %>%
        filter(date > start_date %>% as.POSIXct()) %>%
        return()
}

# 3.2 Get Reference Data ----

get_ref_data <- function(start_date) {
    raw_ref_tbl <-
        import_ods(
            "air-quality-data-continuous",
            select = "siteid, date_time, pm25, pm10",
            where = "(siteid = 215 OR siteid = 500) AND date_time IN [date'2022'..date'2022']",
            order_by = "siteid, date_time"
        )
    
    raw_ref_tbl %>%
        filter(date_time >= start_date) %>%
        rename(date = date_time,
               pm2.5 = pm25) %>%
        return()
}

# retrieve the Temple way low cost sensor data
temple_way_hr_tbl <-
    get_madavi_combined(data_root_url, sensor, start_date)

#
write_rds(
    temple_way_hr_tbl,
    glue("data/{sensor}_{Sys.Date()}_raw.rds")
)

# temple_way_hr_tbl <-
#     read_rds("../air quality analysis/data/esp8266-6496445_2022-08-08_raw.rds")

parson_st_hr_tbl <-
    get_parson_st_data(start_date) # parson st low cost
ref_tbl <- get_ref_data(start_date) # BAM data from both sites

# wrangling to combine reference and low cost sensors in long format
# removing data due to faults and assigning siteids manually
combined_long_tbl <- ref_tbl %>%
    mutate(type = "reference") %>%
    mutate(pm2.5 = if_else(pm2.5 > 200, NA_real_, pm2.5)) %>%
    bind_rows(temple_way_hr_tbl %>%
                  mutate(
                      siteid = 500L,
                      type = "low_cost",
                      pm2.5 = NA
                  )) %>%
    bind_rows(parson_st_hr_tbl %>%
                  mutate(
                      siteid = 215L,
                      type = "low_cost",
                      pm10 = NA
                  ))

# nesting data for modelling

model_data_tbl <- combined_long_tbl %>%
    group_by(siteid) %>%
    nest() %>%
    mutate(
        md = map(data, ~ pluck(.x) %>% # just the relevant pollutants included
                     mutate(across(
                         where( ~ is.na(.x) %>%
                                    all()), ~ NULL
                     ))),
        md_wide = map(
            # daily data
            md,
            ~ pluck(.x) %>%
                pivot_wider(
                    id_cols = date,
                    names_from = type,
                    values_from = starts_with("pm")
                ) 
        )
    )




#
# # lobstr::obj_size(model_data_tbl)
model_data_tbl %>%
    write_rds("data/model_data_tbl.rds")
#
#
# temple_way_hr_tbl %>%
#     openair::timePlot(pollutant = "pm10")
#
# ref_tbl    %>%
#     mutate(across(starts_with("pm"),
#                   ~if_else(.x > 500, NA_real_, .x))) %>%
#     filter(siteid == 215) %>%
#     openair::timePlot(pollutant = "pm2.5")
