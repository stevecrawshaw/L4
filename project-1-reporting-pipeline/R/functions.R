# Libraries - move to _targets.R set_options when done testing ----

library(pacman)
p <-
    c("odbc",
      "here",
      "dplyr",
      "DBI",
      "config",
      "tidyverse",
      "openair",
      "fastverse",
      "janitor",
      "timetk",
      "glue",
      "rvest",
      "lubridate",
      "readxl",
      "openxlsx2",
      "scales",
      "ggtext",
      "sf"
      )
p_load(char = p)

# Variables ----

startDate <- "2022-01-01"
endDate <- "2022-12-31"


# Functions ----

# Functions to scrape the DT calendar dates ----

get.cal.content <- function(){
    url <- "https://laqm.defra.gov.uk/air-quality/air-quality-assessment/diffusion-tube-monitoring-calendar/"
    
    return(read_html(url))
}

get.html.tables <- function(content){
    # return a named list of the html tables holding the DT calendar dates
    get.cal.tbl <- function(tbl){
        if(dim(tbl)[1] == 12){
            return(tbl)
        } else {
            return(NULL)
        }   
    }
    
    tables <- content %>% 
        html_table(fill = TRUE)
    
    table_tbls <- map(tables, .f = ~get.cal.tbl(.x))
    table_list <- table_tbls[!sapply(table_tbls, is.null)]
    
    table_years_names <- html_elements(content, "caption") %>% 
        html_text() %>% 
        str_extract("[0-9]{1,4}") %>% 
        paste0("tbl_", .)
    
    names(table_list) <- table_years_names
    
    return(table_list)
    
}

get.calendar.dates <- function(table_list, year, start_end = "start"){
    # parse the dates in the calendar table for the year
    # return a named vector of start and end dates for the year
    
    table_name <- paste0("tbl_", year)
    
    p.date <- function(string, year, end = FALSE){
        string <- glue("{string} {year}")
        as.Date(string, format = "%e %B %Y") %>% 
            return()
    }
    
    dates_table <- table_list[names(table_list) == table_name][[1]] %>% 
        clean_names() %>% 
        mutate(sd = p.date(start_date, {{year}}),
               ed = if_else(month(as.Date(end_date, format = "%e %B")) == 1,
                            p.date(end_date, ({{year}} + 1)),
                            p.date(end_date, {{year}})),
               mth = month(sd + ((ed - sd) / 2)))
    
    cal_start_date <- dates_table$sd[dates_table$mth == 1]
    cal_end_date <- dates_table$ed[dates_table$mth == 12]
    
    calendar_dates <- c(cal_start_date, cal_end_date) 
    names(calendar_dates) <- c("cal_start", "cal_end")
    
    if (start_end == "start"){
        return(calendar_dates[1])
    } else if (start_end == "end") {
        return(calendar_dates[2])
    }
    
    
}

# Utility functions ---- 

# ggtheme

theme_web_bw <- function() {
    theme_bw() + # note ggplot2 theme is used as a basis
        theme(plot.title = element_text(size = 16, face = "bold",
                                        hjust = 0,
                                        margin = margin(t = 5, b = 25)),
              plot.caption = element_text(size = 12, hjust = 0, 
                                          margin = margin(t = 15)),
              panel.grid.major = element_line(colour = "grey88"),
              panel.grid.minor = element_blank(),
              legend.title = element_text(size = 14, face = "bold"),
              legend.text = element_text(size = 14),
              strip.text = element_text(size = 14, face = "bold"),
              axis.text = element_text(size = 14),
              axis.title.x = element_text(margin = margin(t = 10), size = 15),
              axis.title.y = element_text(margin = margin(r = 10), size = 15))
}

toMidday <- function(date) {
    with_tz(as.POSIXct(paste0(date, " 12:00:00")), 'UTC')
}

toEleven <- function(date) {
    with_tz(as.POSIXct(paste0(date, " 11:00:00")), 'UTC')
}

renameTube <- function(x) {
    paste0("Tube", str_sub(x, -1, -1))
}

hrs.in.year <- function(startDate){
    if(leap_year(startDate)){
        8784
    } else {
        8760
    }
}

# Database connections ----

connect.envista <- function() {
    con_params <- get(file = "config.yml", config = "envista") # credentials
    con_params$driver
    #make connection to Envista using details in the config file
    dbConnect(
        odbc(),
        Driver = con_params$driver,
        Server = con_params$server,
        Database = con_params$database,
        UID = con_params$uid,
        PWD = con_params$pwd,
        Port = con_params$port
    ) %>%
        return()
    
}

connect.access <- function(){
    con <- dbConnect(odbc::odbc(), .connection_string = "Driver={Microsoft Access Driver (*.mdb, *.accdb)}; Dbq=S:\\SUSTAIN\\Sustain-Common\\SCCCS\\write_gis_r\\tube_database\\access_2010_versions\\no2_data.accdb;")
    return(con)
    
}

# Load data ----
get.final.tbl <- function() {
    #read the table which holds contin meta data for the envista database
    read_delim(file = "S:/SUSTAIN/Sustain-Common/SCCCS/write_gis_r/R Projects/air_quality_data_management/data/finalContinTablerhtemp.csv",
               delim = ",",
               col_types = "ciiciccc") %>%
        return()
}

get.aq.data.db <-
    function(con,
             final_tbl,
             startDate = "2020-01-01",
             endDate = "2020-01-05",
             siteid = c(215, 270, 463, 203, 501, 672),
             timebase = 60) {
        #variable to use in padding function
        padby <- case_when(timebase == 15 ~ "15 min",
                           timebase == 60 ~ "hour",
                           timebase == 1440 ~ "day",
                           TRUE ~ "hour")
        
        if (any(siteid %in% "all"))
            siteid = unique(final_tbl$siteid)
        
        #this tbl filters for the sites and timebase supplied to the function
        filtered_tbl <- final_tbl %>%
            filter(siteid %in% {
                {
                    siteid
                }
            },
            minutes == timebase)
        
        # this tbl adds the start and end date
        arg_tbl <- filtered_tbl %>%
            select(siteid, value, status, table) %>%
            mutate(
                dateStartPOS = as.POSIXct(startDate),
                dateEndPOS = as.POSIXct(glue("{endDate} 24:00")),
                table_site = glue("{siteid}_{table}")
            )
        #we convert to a list with tibles in each element
        # to iterate over and supply the siteids when mapped to a df
        site_list <- arg_tbl %>%
            split(arg_tbl$table_site) %>%
            map(select, -c(siteid, table_site)) #remove siteid and table_site as it is the list element name
        
        #function to extract data for each line of the tibble =
        #combination of pollutant and timebase for each site
        getData <- function(value,
                            status,
                            table,
                            dateStartPOS,
                            dateEndPOS) {
            # dbplyr to get each table
            tbl(con, as.character(table)) %>%
                filter(between(Date_Time, dateStartPOS, dateEndPOS)) %>%
                select(Date_Time, all_of(value), all_of(status)) %>%
                collect()
        }
        
        #this is the magic
        #map the getData function to each line of the tibble, using the column names as arguments (pmap) and return a dataframe (dfr)
        
        #then map the mapped function to the list, appending the siteid list name as the .id argument
        output <-
            map_dfr(site_list, ~ pmap_dfr(.l = all_of(.x), .f = getData), .id = "table_site")
        
        
        
        con %>% dbDisconnect()
        
        # output
        
        long <- output %>%
            separate(
                table_site,
                into = c("siteid", "table"),
                sep = "_",
                remove = T
            ) %>%
            pivot_longer(cols = -(siteid:Date_Time), names_to = "metric") %>%
            inner_join(
                filtered_tbl %>%
                    mutate(siteid = as.character(siteid)) %>%
                    pivot_longer(cols = value:status) %>%
                    select(siteid, pollutant, metric = value, table),
                by = c(
                    "siteid" = "siteid",
                    "metric" = "metric",
                    "table" = "table"
                )
            ) %>%
            filter(!is.na(value)) %>%
            mutate(field = str_sub(metric, 1, 1)) %>%
            select(-metric) %>%
            pivot_wider(
                id_cols = c(siteid, table, Date_Time, pollutant),
                names_from = field,
                values_from = value
            ) %>%
            mutate(V = replace(V, S != 1, NA)) %>%
            pivot_wider(
                id_cols = c(siteid, Date_Time),
                names_from = pollutant,
                values_from = V
            ) %>%
            group_by(siteid) %>%
            pad_by_time(.date_var = Date_Time, #throws a non fatal dplyr error here
                        .by = padby) %>%
            # padr::pad(interval = padby) %>%
            rename(date = Date_Time) %>%
            mutate(siteid = as.integer(siteid)) %>% 
            ungroup()
        
        long %>% return()
        
    }

get.aq.data.aurn <- function(sites = c("BRS8", "BR11"), startDate, endDate){

    test.import.aurn <- function() {
    #returns TRUE if import AURN OK
    !is.null(importMeta(source = "aurn", all = FALSE)) %>% 
        return()
    }

get.hourly.aurn.csv.DT.year <- function(site, year) {
    base_url <- "https://uk-air.defra.gov.uk/data_files/site_data/"
    SITE <- toupper(site)
    dl_url <- str_glue("{base_url}{SITE}_{year}.csv?v=1")
    
    tbl_names <-
        fread(dl_url,
              skip = 4,
              nrows = 1,
              fill = TRUE) %>%
        names() %>%
        make_clean_names()
    
    tbl <- tbl <- fread(dl_url, skip = 7)
    
    re_name <- function(x) {
        renamed <- case_when(
            str_detect(x, "volatile") ~ "volatile",
            str_detect(x, "pm_sub_10") ~ "pm10",
            str_detect(x, "pm_sub_2_5") ~ "pm2.5",
            str_detect(x, "nitric_oxide") ~ "no",
            str_detect(x, "nitrogen_oxides") ~ "nox",
            str_detect(x, "nitrogen_dioxide") ~ "no2",
            str_detect(x, "pm_sub_10") ~ "pm10",
            str_detect(x, "ozone") ~ "o3",
            TRUE ~ x
        )
    }
    
    re_names <- map_chr(tbl_names, ~ re_name(.x))
    # strip out meta columns and the volatile data which was discontinued in 2019
    valid <- re_names[!str_detect(re_names, c("unit")) &
                          !str_detect(re_names, c("status")) &
                          !str_detect(re_names, c("volatile"))]
    
    setnames(tbl, new = re_names)
    # select valid columns
    output_DT <- tbl[, ..valid]
    # make nice date and site cols
    output_DT[, date := as.POSIXct(paste0(date, " ", time),
                                   format = "%d-%m-%Y %H:%M")][, time := NULL][, site := ..site]
    #reorder col names
    setcolorder(output_DT, c("date", "site"))
    return(output_DT)
    
}

multi.site.year.import <- function(sites, years) {
    # make all possible combo's of site and year
    comb_df <- expand.grid(sites = sites, years = years)
    # map the function to both arguments and return stacked data frame
    map2_dfr(
        .x = comb_df$sites,
        .y = comb_df$years,
        .f = ~ get.hourly.aurn.csv.DT.year(.x, .y)
    ) %>%
        return()
}

import.aurn.uk.air <- function(sites = c("BRS8", "BR11"), startDate, endDate){

    years <- unique(year(c(as.Date(startDate), as.Date(endDate))))

aurndata <- multi.site.year.import(sites, years) %>%
    filter(date %>%
               between_time(start_date = startDate,
                            end_date = endDate)) %>%
    mutate(siteid = if_else(site == "BRS8", 452L, 500L)) %>%
    select(date, siteid,
           nox, no2, no, pm10, pm2.5, o3, -site) %>% 
    as_tibble()
}

get.aurn.openair <- function(sites = c("BRS8", "BR11"),
                     startDate,
                     endDate) {
    dateStartPOS = as.POSIXct(glue("{startDate} 00:00"), tz = 'UTC')
    dateEndPOS = as.POSIXct(glue("{endDate} 23:59"), tz = 'UTC')
    
    stopifnot(dateStartPOS <= dateEndPOS)
    
    
    yearFrom = year(dateStartPOS)
    yearTo = year(dateEndPOS)
    
    importAURN(sites, year = yearFrom:yearTo) %>%
        with_tz(tzone = 'UTC') %>%
        filter(date %>% between(dateStartPOS, dateEndPOS)) %>%
        mutate(siteid = if_else(code == "BRS8", 452L, 500L)) %>%
        select(date,
               siteid,
               nox,
               no2,
               no,
               pm10,
               pm2.5,
               o3,
               temp = air_temp,
               -c(site, ws, wd)) %>%
        return()
    
}

if(test.import.aurn()) {
    aurn_tbl <- get.aurn.openair(startDate = startDate,
                                 endDate = endDate)    
} else {
    aurn_tbl <- import.aurn.uk.air(startDate = startDate,
                                   endDate = endDate)
}
return(aurn_tbl)
}

get.aq.data.all <- function(startDate = startDate,
                            endDate = endDate){

bcc_data_tbl <- get.aq.data.db(con = connect.envista(),
                               final_tbl = get.final.tbl(),
                               startDate = startDate,
                               endDate = endDate)
aurn_data_tbl <- get.aq.data.aurn(startDate = startDate,
                             endDate = endDate)

all_data_tbl <- bind_rows(bcc_data_tbl, aurn_data_tbl)
return(all_data_tbl)
}

get.no2.data.db <- function(con, startDate, endDate){
    # get the raw no2 data for the year
    safe_start <- as.Date(startDate) - weeks(6)
    safe_end <- as.Date(endDate) + weeks(6)
    year <- year(as.Date(startDate))
    df <- tbl(con, "data") %>% 
        filter(dateOn > safe_start,
               dateOff < safe_end) %>% 
        collect()
    
    out <- df %>% 
        mutate(mid_date = ((dateOff - dateOn) / 2) + dateOn) %>% 
        filter(year(mid_date)  == year) %>% 
        select(dateon = dateOn,
               mid_date,
               dateoff = dateOff,
               siteid = LocID,
               concentration,
               id = ID,
               use_for_annual = UseForAnnual)
    return(out)
    
}

get.background.data <- function(no2_data){
# return the hourly NO2 data in wide format from 4 background sites
    year <- year(no2_data$mid_date %>% median(na.rm = TRUE))
    mindate <-  no2_data$dateon %>% min(na.rm = TRUE)
    maxdate <-  no2_data$dateoff %>% max(na.rm = TRUE)
    
contin_back_sites <- importMeta(all = T)  %>% 
    filter(between(latitude, 50.70, 52.24), 
           between(longitude, -3.79, -1.42),
           site_type %in% c("Urban Background"),
           Parameter_name == "Nitrogen dioxide",
           end_date == "ongoing")

years_vec <- seq(from = as.integer(year) - 1, to = as.integer(year) + 1, by = 1)
#get the data
back_sites <- contin_back_sites$code
    # c("BRS8", "BORN", "NPT3", "SWHO") # MANUAL

back_site_names <- contin_back_sites$site
    # contin_back_sites %>% 
    # filter(code %in% back_sites) %>% 
    # select(site)

annual_back_data <- importAURN(site = back_sites,
                               pollutant = "no2",
                               year = years_vec) %>%
    with_tz(tzone = 'UTC') %>%
    filter(date %>%
               between(with_tz(as.POSIXct(mindate), 'UTC'),
                       with_tz(as.POSIXct(maxdate + 1), 'UTC'))) %>% 
    select(-code) %>% 
    pivot_wider(id_cols = date, names_from = site, values_from  = no2) %>% 
    arrange(date) %>% 
    relocate(date, `Bristol St Paul's`, Bournemouth, Newport, `Swindon Walcot`, everything())

return(annual_back_data)

}

get.aqms <- function(con){
    instrument_tbl <- tibble::tribble(
        ~instrumenttype, ~instrument_id,
        "Diffusion Tube", 1L,
        "Continuous (Reference)", 2L,
        "Volume Sampler", 3L,
        "Frisbee", 4L,
        "Anemometer", 5L,
        "Thermometer", 6L,
        "Rain Gauge", 7L,
        "Hygrometer", 8L,
        "Radiometer", 9L,
        "Continuous (Indicative)", 10L
    )
        #    get air quality monitoring sites from access database
    aqms_tbl <- tbl(con, 'locations') %>%
        filter(!is.na(Easting), !is.na(Northing)) %>% 
        filter(!SiteID == 573L) %>% 
        collect() %>% 
        clean_names() %>% 
        left_join(instrument_tbl, by = "instrument_id") %>% 
        rename(sample_height = tube_height,
               siteid = site_id) %>% 
        select(-site_no, -instrument_id, route_id)
    
    return(aqms_tbl)
}

get.lastyears.sites <- function(con, startDate){
    lastyear = year(startDate) - 1
    
    tbl(con, "tbl_final_ba_annual") %>% 
        filter(dYear == lastyear) %>% 
        select(siteid = LocID) %>% 
        collect() %>% 
        mutate(exist_last_year = TRUE) %>% 
        return()
}
# Wrangle data ----

pivot.tubes.monthly <- function(no2_data){
    #construct the wide format concentration data grouped by siteid and month
    no2_data %>% 
        filter(use_for_annual == 1L) %>%
        # THIS REMOVES TUBES WITH NON COMPLIANT EXPOSURE DATES
        mutate(month_num = lubridate::month(mid_date) %>% as.integer(),
               month = month.abb[month_num] %>%
                   as_factor(),
               siteid = as.integer(siteid)) %>%
        group_by(month, siteid)  %>% 
        mutate(row = row_number()) %>%
        ungroup() %>% 
        arrange(month_num) %>% 
        pivot_wider(id_cols = c(siteid, row),
                    names_from = month,
                    values_from = concentration) %>%
        arrange(siteid, row) %>% 
        select(-row) %>%
        return()
}

make.step.2.table <- function(aqms_tbl,
                              pivoted_tubes_tbl,
                              last_years_sites_tbl){
    
    step_2_table <- aqms_tbl %>% 
        filter(instrumenttype == "Diffusion Tube") %>% 
        transmute(siteid,
                  location,
                  dup_trip = if_else(!is.na(duplicate_triplicate),
                                     glue("{siteid}_{duplicate_triplicate}"),
                                     NA_character_),
                  easting, northing, laqm_locationclass,
                  dist_exposure = if_else(exposure,
                                          rec_kerb - tube_kerb,
                                          0),
                  # if there's no exposure make distances 0 so that the
                  # spreadsheet doesn't run a fall off distance calc
                  tube_kerb_distance = tube_kerb) %>% 
        right_join(pivoted_tubes_tbl,
                   by = c("siteid" = "siteid"), multiple = 'all') %>% 
        left_join(last_years_sites_tbl, by = c("siteid" = "siteid"), multiple = 'all') %>% 
        mutate(new_existing = if_else(is.na(exist_last_year), "New", "Existing")) %>%
        group_by(siteid, dup_trip) %>%
        mutate(row_id = row_number()) %>%
        ungroup() %>%
        mutate(newsiteid = if_else(!is.na(dup_trip),
                                   glue("{as.character(siteid)}_{row_id}"),
                                   glue("{siteid}")),
               .before = siteid) %>%
        arrange(siteid) %>% 
        select(newsiteid:dup_trip, new_existing,
               everything(),
               -exist_last_year,
               -siteid,
               -row_id)
    return(step_2_table)
}

make.bias.site.list <- function(aqms_tbl, no2_data){
#subset monitor data to colocated sites
    
    renameTube <- function(x) {
        paste0("Tube", str_sub(x, -1, -1))
    }

join_table <- aqms_tbl %>%
    filter(!is.na(colocated)) %>%
    select(siteid, contin_siteid = colocated) %>%
    inner_join(aqms_tbl, by = c("contin_siteid" = "siteid")) %>%
    select(site = location, contin_siteid, siteid)

# just colocated tube data

colocated_tbl <- no2_data %>%
    inner_join(join_table, by ="siteid") %>%
    arrange(siteid, mid_date)
year <- year(median(no2_data$mid_date, na.rm = TRUE))
# 
# # colocated_tbl %>% 
# #     write_csv(glue("data/{year}_colocated_tube_data.csv"))
# #make the table
# tubes_report <- 
#     no2_data %>% 
#     filter(siteid %in% join_table$siteid) %>%
#     inner_join(join_table, by = "siteid") %>% 
#     transmute(site, month = month(mid_date), concentration) %>%
#     group_by(site, month) %>% 
#     nest() %>% 
#     unnest_wider(data) %>%
#     unnest_wider(concentration) %>% 
#     rename_with(renameTube, starts_with("..")) %>%
#     arrange(site, month) %>% 
#     ungroup() %>% 
#     nest_by(site) %>% 
#     mutate(path = glue("{here::here('data')}/{site}_tubes.csv")) %>% 
#     ungroup() %>% 
#     select(x = data, path) 
bias_site_list <- no2_data %>% 
        filter(siteid %in% join_table$siteid) %>%
        inner_join(join_table, by = "siteid") %>% 
        transmute(site, month = month(mid_date),
                  concentration) %>%
        group_by(site, month) %>% 
        nest() %>% 
        unnest_wider(data) %>%
        unnest_wider(concentration, names_sep = "_") %>% 
        # rename_with(renameTube, starts_with("..")) %>%
        arrange(site, month) %>% 
        ungroup() %>% 
        split(as_factor(.$site))

# use pwalk to iteratively save the csvs

return(bias_site_list)
}

get.contin_data <- function(con, no2_data, final_tbl){
    mindate <- min(no2_data$dateon)
    maxdate <- max(no2_data$dateoff)
    
    get.aq.data.db(con = con, final_tbl = get.final.tbl(),
                   startDate = mindate,
                   endDate = maxdate, timebase = 60)
}

make.contin.no2.nested <- function(contin_data, aqms_tbl){

    year <- year(median(contin_data$date))
    
contin_data %>% 
    inner_join(aqms_tbl %>% 
                   select(siteid, location),
               by = c("siteid" = "siteid")) %>% 
    select(location, date, no2) %>% 
    arrange(date) %>% 
    nest_by(location) %>% 
    mutate(file = glue("{here::here('data')}/{location}_{year}_continuous.csv")) %>%
        ungroup() %>% 
        select(x = data, file) %>% 
        return()
# write with pwalk write_csv
#pwalk(fwrite, na = "", dateTimeAs = "write.csv"
}

get.gridconcs.da.tubes <- function(con, startDate, siteids){
    year <- year(as.Date(startDate))
    # tbl of sites that need adjusting with grid id's
    distance_adjust_sites_tbl <- aqms_tbl %>% 
        filter(current,
               exposure,
               rec_kerb > tube_kerb,
               siteid %in% siteids) %>% 
        select(siteid, grid_id)
    
    # the no2 backfground grid data
    grid_concs_tbl <- tbl(con, "tbl_background_grids_2018_base") %>% 
        select(id, starts_with("no2")) %>% 
        collect()
    # reference table linking grid ids
    grid_id_tbl <- tbl(con, "tbl_grid_id") %>% 
        rename(grid_id = gridid) %>% 
        collect()
    # join these two
    grid_joined_tbl <- grid_concs_tbl %>% 
        pivot_longer(cols = -id,
                     names_to = "year",
                     names_prefix = "no2_",
                     values_to = "back_conc") %>%
        mutate(year = as.integer(year)) %>% 
        filter(year == {{year}}) %>% 
        inner_join(grid_id_tbl, by = c("id" = "id"))
    #join the tubes tbl to return the final tbl of background concs for the tubes in sitids
    sites_gridconcs_tbl <- grid_joined_tbl %>% 
        inner_join(distance_adjust_sites_tbl,
                   by = c("grid_id" = "grid_id")) %>% 
        select(siteid, back_conc) 
    
    return(sites_gridconcs_tbl)
}

make.table.a2 <- function(aqms_tbl){
    aqms_tbl %>% 
        filter(current,
               instrumenttype == "Diffusion Tube") %>% 
        transmute(siteid, aqma = if_else(aqma, "Yes", "No"),
                  colocated = if_else(is.na(colocated), "No", "Yes"),
                  sample_height) %>% 
        arrange(siteid)
}

make.coloc.divisor.tbl <- function(aqms_tbl){
    aqms_tbl %>% 
        filter(current,
               instrumenttype == "Diffusion Tube") %>% 
        select(siteid, duplicate_triplicate) %>% 
        mutate(div = case_when(
            is.na(duplicate_triplicate) ~ 1L,
            duplicate_triplicate == "T" ~ 3L,
            TRUE ~ 2L
        )) %>% 
        return()
}

make.data.cap.period.tbl <- function(coloc_divisor_tbl, no2_data){
    
    no2_data %>%
        mutate(days_measuredin_month = dateoff - dateon) %>% #view()
        group_by(siteid) %>%
        summarise(
            starts = min(dateon),
            ends = max(dateoff),
            period = (ends - starts) %>% as.numeric(),
            sumdays = sum(days_measuredin_month) %>%
                as.numeric(),
            .groups = "keep"
        ) %>%
        inner_join(coloc_divisor_tbl,
                   by = c("siteid" = "siteid")) %>%
        mutate(dc_monitoring_period = ((sumdays / period) / div) * 100)
}

get.annual.tube.data.4yrs.tbl <- function(con, startDate){
    year <- year(as.Date(startDate))
    
    annual_tube_data_4years_tbl <-
        tbl(con, "tbl_final_ba_annual") %>% 
        filter(between(dYear,
                       {{year}} - 4,
                       {{year}} - 1)) %>% 
        select(siteid = LocID,
               year = dYear,
               conc = final_adjusted_conc) %>% 
        pivot_wider(id_cols = siteid,
                    names_from = year,
                    values_from = conc) %>%
        collect()

    return(annual_tube_data_4years_tbl)
}

make.table.a4 <- function(annual_tube_data_4years_tbl,
                          data_cap_period_tbl){
    
    annual_tube_data_4years_tbl %>% 
        right_join(data_cap_period_tbl %>%
                       select(siteid, dc_monitoring_period),
                   by = "siteid") %>% 
        mutate(across(where(is.numeric), ~round(.x, 1))) %>% 
        relocate(siteid, dc_monitoring_period)
}

get.count.tubes.tbl <- function(con){
    tbl(con, "data") %>% 
        mutate(mid_date = dateOn + ((dateOff - dateOn) / 2L)) %>% 
        filter(mid_date >= as.Date("2010-01-01")) %>% 
        select(mid_date, siteid = LocID) %>% 
        collect() %>% 
        group_by(year = year(mid_date), siteid) %>% 
        summarise(count = n(), .groups = "drop") %>% 
        return()
}

make.ods.upload.tube.tbl <- function(con,
                                     count_tubes_tbl,
                                     path,
                                     startDate){
    # the path 
    # references the a copy of the data calculated as
    # final concs in Table A4 as readxl won't open .xlsb. bah!
    
    year <- year(startDate)
    
    annual_tube_data_all_tbl <- tbl(con, "tbl_final_ba_annual") %>% 
        filter(dYear >= 2010) %>%
         #filter for current yr
        select(year = dYear,
               siteid = LocID,
               conc_ugm3 = final_adjusted_conc) %>%
        collect() %>% 
        filter(year != {{year}} %>% as.integer())
    
    re_siteid <- function(siteid_dirty){
        if(str_detect(siteid_dirty, "_")){
            return(str_sub(siteid_dirty, 1, 3))
        } else {
            return(siteid_dirty)
        }
    }
    
    from_a4_tbl <- readxl::read_xlsx(path = path,
                             sheet = "Sheet1",
                             range = "A1:B500") %>% 
        drop_na() %>% 
        filter(!conc_ugm3 == "-") %>% # filter out sites with low DC
        mutate(siteid = siteid %>%
                   map_chr( ~re_siteid(.x)) %>%
                   as.integer(),
               conc_ugm3 = as.double(conc_ugm3))
    
    ods_tube_upload_tbl <- from_a4_tbl %>%
        mutate(year = !!year %>% as.integer()) %>% 
                relocate(year, siteid, conc_ugm3) %>% 
        bind_rows(annual_tube_data_all_tbl) %>%
                      inner_join(count_tubes_tbl,
                                 by = c("siteid" = "siteid",
                                        "year" = "year")) %>% 
        arrange(desc(year), siteid)
    
    return(ods_tube_upload_tbl)
    
    
}

make.table.a1 <- function(aqms_tbl){

monitor_tech <- function(pollutants){
    # return the monitoring technique given a string of pollutants
    # this will need amending to account for the new CAV monitors
    pm <- str_detect(pollutants, "PM")
    nox <- str_detect(pollutants, "NOx|NOX")
    if (pm & nox){
        out = "Chemiluminescent (NOx) and Beta Attentuation (PM)"
    } else if( nox & !pm) {
        out = "Chemiluminescent"
    } else if (!nox & pm){
        out = "Beta Attentuation"
    }
    return(out)
}

table_a1 <- aqms_tbl %>% 
    filter(instrumenttype == "Continuous (Reference)", current) %>% 
    transmute(siteid,
              location,
              laqm_locationclass,
              easting, northing, pollutants,
              aqma = if_else(aqma, "Yes", "No"),
              monitoring_tech = map_chr(pollutants, monitor_tech),
              dist_exposure = ifelse(exposure,
                                     rec_kerb - tube_kerb,
                                     NA), 
              tube_kerb,
              sample_height)
return(table_a1)
}

make.datacap.tbl <-  function(contin_4yrs_tbl, startDate, pollutant){
    pollutant <- enquo(pollutant)
    year <- year(startDate)
    # pollutant <- "no2"
    data_cap_tbl <- contin_4yrs_tbl %>% 
    select(siteid, date, !!pollutant) %>% 
    filter(year(date) == {{year}}) %>% 
    group_by(siteid, year = year(date)) %>% 
    summarise(dcp = ((sum(!is.na(!!pollutant))) / hrs.in.year({{startDate}}) * 100) %>% round(1),
              dcy = ((sum(!is.na(!!pollutant)) / difftime(max(date), min(date), units = "hours") %>% as.integer()) * 100) %>% round(1),
              .groups = "drop") %>% 
    select(-year)
}

make.table.a3 <- function(contin_4yrs_tbl, startDate, aqms_tbl, no2_data_cap_tbl){
    
    contin_annual_no2_wide_tbl <- contin_4yrs_tbl %>% 
        select(siteid, date, no2) %>% 
        filter(year(date) <= year(startDate)) %>% 
        group_by(siteid, year = year(date)) %>% 
        summarise(ann_mean_no2 = mean(no2, na.rm = TRUE),
                  .groups = "drop") %>% 
        pivot_wider(id_cols = siteid,
                    names_from = year,
                    values_from = ann_mean_no2)
    
    table_a3_tbl <- aqms_tbl %>% 
        select(siteid, easting, northing, laqm_locationclass) %>% 
        inner_join(no2_data_cap_tbl, by = "siteid") %>% 
        inner_join(contin_annual_no2_wide_tbl, by = "siteid")
    
    return(table_a3_tbl)
    
}

make.table.a5 <- function(contin_4yrs_tbl,
                          startDate,
                          aqms_tbl,
                          no2_data_cap_tbl){
    
    perc_exc_no2_tbl <- contin_4yrs_tbl %>% 
        select(siteid, date, no2) %>% 
        filter(year(date) <= year(startDate)) %>% 
        group_by(siteid, year = year(date)) %>% 
        summarise(exc_no2 = sum(no2 > 200, na.rm = TRUE),
                  perc_no2 = quantile(no2,
                                      probs = 0.998,
                                      na.rm = TRUE) %>%
                      round(1),
                  .groups = "drop") %>% 
        left_join(no2_data_cap_tbl, by = "siteid") %>%
        rowwise() %>% 
        mutate(cell = if_else(dcp < 85,
                              glue("{exc_no2}({perc_no2})"),
                              glue("{exc_no2}"))) %>% 
        pivot_wider(id_cols = siteid,
                    names_from = year,
                    values_from = cell)
    
    a5_table_tbl <- aqms_tbl %>% 
        select(siteid, easting, northing, laqm_locationclass) %>% 
        inner_join(no2_data_cap_tbl, by = "siteid") %>% 
        inner_join(perc_exc_no2_tbl, by = "siteid")
    
    return(a5_table_tbl)
}

make.table.a6 <- function(contin_4yrs_tbl,
                          startDate,
                          aqms_tbl,
                          pm10_data_cap_tbl){
    pm10_mean_tbl <- contin_4yrs_tbl %>% 
        select(siteid, date, pm10) %>% 
        filter(year(date) <= year(startDate)) %>% 
        group_by(siteid, year = year(date)) %>% 
        summarise(mean_pm10 = mean(pm10, na.rm = TRUE) %>% round(1),
                  .groups = "drop") %>% 
        left_join(pm10_data_cap_tbl, by = "siteid") %>% 
        na.omit() %>% 
        pivot_wider(id_cols = siteid,
                    names_from = year,
                    values_from = mean_pm10) 
    
    table_a6_tbl <- aqms_tbl %>% 
        select(siteid, easting, northing, laqm_locationclass) %>% 
        inner_join(pm10_data_cap_tbl, by = "siteid") %>% 
        inner_join(pm10_mean_tbl, by = "siteid")
return(table_a6_tbl)    
    
}

make.table.a7 <- function(contin_4yrs_tbl,
                          startDate,
                          aqms_tbl,
                          pm10_data_cap_tbl){

pm10_exc_tbl <- contin_4yrs_tbl %>% 
    select(siteid, date, pm10) %>% 
    filter(year(date) <= year(startDate)) %>%
    mutate(siteid = as_factor(siteid)) %>% 
    timeAverage(avg.time = "day",
                data.thresh = 0.75,
                type = "siteid") %>% 
    mutate(siteid = as.integer(as.character(siteid))) %>% 
    group_by(siteid, year = year(date)) %>% 
    summarise(exc_pm10 = sum(pm10 > 50, na.rm = TRUE),
              perc_pm10 = quantile(pm10,
                                  probs = 0.904,
                                  na.rm = TRUE) %>% 
                  round(1),
              .groups = "drop") %>% 
    left_join(pm10_data_cap_tbl, by = "siteid") %>%
    rowwise() %>% 
    mutate(cell = if_else(dcp < 85,
                          glue("{exc_pm10}({perc_pm10})"),
                          glue("{exc_pm10}"))) %>% 
    na.omit(perc_pm10) %>% 
    pivot_wider(id_cols = siteid,
                names_from = year,
                values_from = cell)

table_a7_tbl <- aqms_tbl %>% 
    select(siteid, easting, northing, laqm_locationclass) %>% 
    inner_join(pm10_data_cap_tbl, by = "siteid") %>% 
    inner_join(pm10_exc_tbl, by = "siteid")
return(table_a7_tbl)    

}

make.table.a8 <- function(contin_4yrs_tbl,
         startDate,
         aqms_tbl,
         pm2.5_data_cap_tbl){
    pm2.5_mean_tbl <- contin_4yrs_tbl %>% 
        select(siteid, date, pm2.5) %>% 
        filter(year(date) <= year(startDate)) %>% 
        group_by(siteid, year = year(date)) %>% 
        summarise(mean_pm2.5 = mean(pm2.5, na.rm = TRUE) %>% round(1),
                  .groups = "drop") %>% 
        left_join(pm2.5_data_cap_tbl, by = "siteid") %>% 
        na.omit() %>% 
        arrange(year) %>% 
        pivot_wider(id_cols = siteid,
                    names_from = year,
                    values_from = mean_pm2.5) 
    
    table_a8_tbl <- aqms_tbl %>% 
        select(siteid, easting, northing, laqm_locationclass) %>% 
        inner_join(pm2.5_data_cap_tbl, by = "siteid") %>% 
        inner_join(pm2.5_mean_tbl, by = "siteid")
    
    return(table_a8_tbl)    
}

make.table.list <- function(...){
    # take dataframes and turn all contents to character
    # making empty strings for all NA's for nice spreadsheet
    # formatting
    tables <- lst(...)
    t <- map(tables,
             ~mutate(.x, across(everything(),
                                .f = ~as.character(.x) %>%
                                    replace_na(""))))
    return(t)
}

write.spreadsheets <- function(table_list,
                               bias_site_list,
                               ods_tubes_upload_tbl,
                               startDate){
    year <- year(startDate)
    asr_dtpt_file = glue("data/asr_tables_{year}.xlsx")
    bias_tube_file = glue("data/bias_input_tables_{year}.xlsx")
    ods_tubes_upload_tbl_file = glue("data/ods_tubes_upload_{year}.csv")
    
    write_xlsx(table_list, file = asr_dtpt_file)
    write_xlsx(bias_site_list, file = bias_tube_file)
    write.csv2(ods_tubes_upload_tbl, ods_tubes_upload_tbl_file)
    
    print(glue("  files are saved /n
               {bias_tube_file} /n
               {asr_dtpt_file} /n
               {ods_tubes_upload_tbl_file}")) %>% 
        return()
    
}

make.plotareas_tbl <- function(){
    
    central <- tribble(~siteid, 2, 5, 9, 11, 15, 113, 125, 147, 423, 318) %>% 
        mutate(area = "Central")
    glosrd <- tribble(~siteid, 21, 22, 157, 159, 161, 163) %>% 
        mutate(area = "Gloucester Road")
    bathrd <- tribble(~siteid, 10, 403, 478) %>% 
        mutate(area = "Bath Road")
    a37 <- tribble(~siteid, 4, 14, 413, 438) %>% 
        mutate(area = "A37")
    parson <- tribble(~siteid, 239, 242, 418, 419) %>% 
        mutate(area = "Parson Street")
    m32 <- tribble(~siteid, 20, 260, 261, 263, 373, 374, 441) %>% 
        mutate(area = "M32")
    
    plotareas_tbl <- bind_rows(central, glosrd, bathrd, a37, parson, m32)
    return(plotareas_tbl)
}

make.no2.trend.chart.tbl <- function(startDate,
                                     ods_tubes_upload_tbl,
                                     plotareas_tbl,
                                     aqms_tbl){
    
       year = year(startDate)
    xyear = year -8 #for location of the AQ objective annotation
    
    tube_chart_data <- 
        ods_tubes_upload_tbl %>%  
        filter(siteid %in% plotareas_tbl$siteid) %>% 
        inner_join(plotareas_tbl, by = "siteid") %>% 
        inner_join(aqms_tbl %>% 
                       select(siteid, location), by = "siteid") %>% 
        mutate(location = str_wrap(location, width = 25)) %>%
        group_by(area) %>%
        nest() %>% #creaet a ggplot object for each area and nest into this DF
        mutate(plot = map(data,
                          ~ggplot(., aes(year,
                                         conc_ugm3,
                                         colour = location)) +
                              geom_line(linewidth = 1) +
                              labs(title = quickText(
                                  paste0("NO2 trends at diffusion tube sites: ", area)
                              ),
                              y = quickText("NO2 ugm-3"),
                              x = "Year",
                              colour = "Location",
                              caption = "Bias adjusted and annualised, not distance adjusted") +
                              geom_hline(yintercept = 40,
                                         linewidth = 2,
                                         lty = 5,
                                         colour = "red") +
                              annotate("label",
                                       x = xyear,
                                       y = 42,
                                       label = "Air Quality Objective") +
                              scale_x_continuous(breaks = scales::breaks_pretty()) + 
                              theme_web_bw()),
               filename = glue("plots/{area}_{xyear}_to_{year}_no2_trend_.png"))
}

write.no2.trend.charts <- function(no2_trend_chart_tbl){
    #plot the outputs to file
    no2_trend_chart_tbl %>%
        ungroup() %>% 
        select(plot, filename) %>% 
        pwalk(ggsave, width = 10, height = 7)
}
# PM2.5 chart

make.pm25.trend.chart <- function(startDate){
    
    year_range <- seq.int(year(startDate) - 5, year(startDate))
    
    pm_25_tbl <- importAURN(year = year_range,
                            data_type = "annual") %>% 
        filter(code == "BRS8") %>% 
        select(date, pm2.5) %>% 
        mutate(year = year(date))
    
    pm25_chart <- pm_25_tbl %>%
        ggplot(aes(x = year, y = pm2.5)) +
        geom_line(linewidth = 1) +
        geom_text(aes(label = round(pm2.5, 1)),
                  vjust = 2) +
        geom_hline(yintercept = 5,
                   color = "red",
                   lty = 5,
                   linewidth = 1,
                   alpha = 0.5) +
        scale_y_continuous(breaks = scales::breaks_pretty()) +
        labs(x = "Year",
             y = quickText("ugm-3"),
             title = quickText("Trend in Annual Mean PM2.5 Bristol St. Pauls")) +
        annotate("label",
                 x = min(pm_25_tbl$year) + 0.5,
                 y = 5.6,
                 label = "WHO guideline value") +
        expand_limits(y = 0) + 
        theme_web_bw() +
        theme(panel.border = element_blank())
    
    return(pm25_chart)
}

write.pm25.trend.chart <- function(pm25_trend_chart){
    
    yearvec <- pm25_trend_chart$data$year
    yearmin <- min(yearvec)
    yearmax = max(yearvec)
    
    filename <- glue("plots/PM2.5_trend_{yearmin}_to_{yearmax}.png")
    
    ggsave(pm25_trend_chart, filename = filename)
    
    return(glue("chart image file saved to {filename}"))
}



