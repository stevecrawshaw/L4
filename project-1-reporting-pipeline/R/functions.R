# Libraries - move to _targets.R set_options when done testing ----

library(pacman)
p <-
    c("odbc",
      "dplyr",
      "DBI",
      "config",
      "tidyverse",
      "openair",
      "fastverse",
      "janitor",
      "timetk")
p_load(char = p)

# Functions ----

connect.envista <- function() {
    con_params <- get(config = "envista") # credentials
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
            mutate(siteid = as.integer(siteid))
        
        long %>% return()
        
    }

test.import.aurn <- function() {
    #returns TRUE if import AURN OK
    importAURN(site = "BRS8", year = 2020) %>%
        !is.null %>%
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

import.aurn.uk-air <- function(sites, startDate, endDate){

    years <- unique(year(c(as.Date(startDate), as.Date(endDate))))

aurndata <- multi.site.year.import(sites, years) %>%
    filter(date %>%
               between_time(start_date = startDate,
                            end_date = endDate)) %>%
    mutate(siteid = if_else(site == "BRS8", 452L, 500L)) %>%
    select(date, siteid,
           nox, no2, no, pm10, pm2.5, o3, -site)
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