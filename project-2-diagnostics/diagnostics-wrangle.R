# Load packages ----
packages <-
    c("tidyverse",
      "openair",
      "glue",
      "timetk",
      "lubridate")

pacman::p_load(char = packages)

source("diagnostics-retrieve.R")
# Variables ----
dateon <-  "2022-09-01"
dateoff <-  "2022-10-01"
month <- month(dateon,
               label = TRUE,
               abbr = FALSE) %>%
    as.character()
year <- year(dateon)

# Data ----
# diagnostics data
con <- connect.envista()

diag_tbl <- get.diag.tbl(con, dateon, dateoff)

con %>% dbDisconnect()

# site metadata linking station ID's with site ID's and pollutants
final_tbl <- read_delim(file = "S:/SUSTAIN/Sustain-Common/SCCCS/write_gis_r/R Projects/air_quality_data_management/data/finalContinTablerhtemp.csv", delim = ",", col_types = "ciiciccc")
# matching siteids to names
siteid <- c(215L, 270L, 203L, 463L, 501L, 672L)
site_name <- c("Parson St",
                "Wells Road",
                "Brislington",
                "Fishponds",
                "Colston Ave",
                "Marlborough St")
sites_tbl <- tibble(siteid, site_name)
# Functions ----

lengthen.diag <- function(diagnostics_tbl){
    # function to pivot longer the names, units and values from the 
    # diagnostics table and combine into one dataframe
    
    names_df <- diagnostics_tbl %>% 
        select(DIG_DateTime, DIG_Station, contains("Name")) %>% 
        pivot_longer(cols = contains("Name"),
                     names_to = "DIG_Name",
                     values_to = "parameter")
    
    units_df <- diagnostics_tbl %>% 
        select(DIG_DateTime, DIG_Station, contains("Unit")) %>% 
        pivot_longer(cols = contains("Unit"),
                     names_to = "DIG_Unit",
                     values_to = "unit")
    
    values_df <- diagnostics_tbl %>% 
        select(DIG_DateTime, DIG_Station, contains("Value")) %>% 
        pivot_longer(cols = contains("Value"),
                     names_to = "DIG_Value",
                     values_to = "value")
    
    combined_tbl <- cbind(names_df %>% 
                              select(-DIG_Name),
                          units_df %>%
                              select(unit),
                          values_df %>% 
                              select(value)) %>% 
        as_tibble()
    return(combined_tbl)
}

clean.plot <- function(data, site, month, year){
# takes the nested df, strips out conc data (not interesting)
    # and produces ggplot with facets
        p <- data %>% 
        filter(!str_detect(parameter, "Conc")) %>% 
        ggplot(aes(x = DIG_DateTime, y = value)) +
        geom_line() +
        facet_wrap(~parameter + unit, scales = "free_y") +
        labs(title = glue("Diagnostics plots for {site}: {month} {year}"),
             x = "Date") +
        scale_x_datetime(breaks = "1 weeks", date_labels = "%d") +
        theme_bw() 
    return(p)
}

# Wrangling ----

station_site_tbl <- final_tbl %>% 
    mutate(station = str_remove(tablename, "S") %>% as.integer()) %>% 
    distinct(station, siteid) %>% 
    inner_join(sites_tbl, by = "siteid")

combined_tbl <- lengthen.diag(diag_tbl)

# clean it
clean_combined_tbl <- combined_tbl %>% 
    na.omit() %>% 
    mutate(parameter = str_trim(parameter),
           unit = str_trim(unit)) %>% 
    inner_join(station_site_tbl, by = c("DIG_Station" = "station")) 

all_sites_plots_tbl <- clean_combined_tbl %>%
    select(-DIG_Station) %>% 
    nest_by(siteid, site_name) %>% 
    mutate(plot = list(clean.plot(data,
                                  site = site_name,
                                  {{month}},
                                  {{year}})))

#testing plotting
# clean.plot(bris_test_tbl, site ="bris", month, year)
# 
# plot_tbl <- all_sites_tbl %>% 

plot_tbl$plot[3]

# Next step ---

# create a range for each parameter in a tbl
# ensure all possible names are included.
# join tbl to the diag_tbl
# filter for sites \ parameters where out of bounds.
# display in formatted gt::table to identify excursions




