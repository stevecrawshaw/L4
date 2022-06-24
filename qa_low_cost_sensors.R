# ___________ ----
# 1.0 Libraries ----
p <- c("tidyverse", "lubridate", "httr2", "jsonlite", "glue", "janitor", "fs")
library(xfun)
pkg_attach2(p)
rm(p)

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
# 3.2 Get Reference Data ----

source("../airquality_GIT/importODS.R")


day_files_tbl <- get_daily_csv_urls(data_root_url, sensor) %>% 
    map_df(~read_sds_csv(.x))

zip_sds_tbl <- get_zip_file_urls(data_root_url, sensor, start_date) %>% 
    map_df(~read_sds_zip(.x))

combined_tbl <- day_files_tbl %>% 
    bind_rows(zip_sds_tbl)
write_rds(combined_tbl, glue("../air quality analysis/data/{sensor}_{Sys.Date()}_raw.rds"))
