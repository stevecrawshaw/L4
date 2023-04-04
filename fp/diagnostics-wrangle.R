# Load packages ----
pacman::p_load(tidyverse,
               openair,
               glue,
               timetk,
               lubridate)

source("diagnostics-retrieve.R")
# Variables ----
dateon <-  "2022-12-01"
dateoff <-  "2022-12-31"

datecheck <- function(dateon, dateoff){
    
datediff <- as.Date(dateoff) - as.Date(dateon)

return(datediff > 27)

}
    datecheck(dateon = dateon, dateoff = dateoff)

# Data ----
# diagnostics data
con <- connect.envista()

diag_tbl <- get.diag.tbl(con, dateon, dateoff)

con %>% dbDisconnect()

# site metadata linking station ID's with site ID's and pollutants
final_tbl <- read_delim(file = "S:/SUSTAIN/Sustain-Common/SCCCS/write_gis_r/R Projects/air_quality_data_management/data/finalContinTablerhtemp.csv", delim = ",", col_types = "ciiciccc")

    # https://www.teledyne-api.com/prod/Downloads/06858F%20-%20MANUAL,%20OPERATORS,%20T200.pdf
    # table 12.1

limits_tbl <- tribble(
	~parameter, ~warning_low, ~warning_high, ~normal_low, ~normal_high,
	"Converter temp", 305, 325, 310, 320,
	"Internal box temp", 7, 48, 7, 40,
	"React cell temp", 45, 55, 49, 51,
	"Ozone flow rate", 50, 150, 80, 100,
	"PMT temp", 5, 12, 6, 8,
	"React cell pres", 4, 10, 5, 5.6,
	"React cell Pres", 4, 10, 5, 5.6,
	"Sample flow rate", 350, 600, 450, 550,
	"Sample pres", 28, 30, 15, 35,
	"Sample Pres", 28, 30, 15, 35
)

# 463 and 672 have diagnostics data collected on wrongly labelled channels

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

make.long.diag.tbl <- function(diag_tbl){
    # function to pivot longer the names, units and values from the 
    # diagnostics table and combine into one dataframe
    
    names_df <- diag_tbl %>% 
        select(DIG_DateTime, DIG_Station, contains("Name")) %>% 
        pivot_longer(cols = contains("Name"),
                     names_to = "DIG_Name",
                     values_to = "parameter")
    
    units_df <- diag_tbl %>% 
        select(DIG_DateTime, DIG_Station, contains("Unit")) %>% 
        pivot_longer(cols = contains("Unit"),
                     names_to = "DIG_Unit",
                     values_to = "unit")
    
    values_df <- diag_tbl %>% 
        select(DIG_DateTime, DIG_Station, contains("Value")) %>% 
        pivot_longer(cols = contains("Value"),
                     names_to = "DIG_Value",
                     values_to = "value")
    
    long_diag_tbl <- cbind(names_df %>% 
                              select(-DIG_Name),
                          units_df %>%
                              select(unit),
                          values_df %>% 
                              select(value)) %>% 
        as_tibble()
    return(long_diag_tbl)
}

make.clean.plot <- function(data, site, dateon, dateoff){
# takes the nested df, strips out conc data (not interesting)
    # and produces ggplot with facets
	if(var(month(c(dateon, dateoff))) == 0 & var(year(c(dateon, dateoff))) == 0){
	    
	    datelabel <- glue("{month(dateon,
               label = TRUE,
               abbr = FALSE) %>%
	    as.character()} {year(dateon)}")

	} else {
	    datelabel = glue("{format(as.Date(dateon), '%d/%m/%Y')} to {format(as.Date(dateoff), '%d/%m/%Y')}")
	}
    
        p <- data %>% 
        filter(!str_detect(parameter, "Conc")) %>% 
        ggplot(aes(x = DIG_DateTime, y = value)) +
        geom_line() +
        geom_line(aes(x = DIG_DateTime,
                      y = normal_low),
                  color = "blue",
                  lty = 5,
                  alpha = 0.6) +
        geom_line(aes(x = DIG_DateTime,
                      y = normal_high),
                  color = "blue",
                  lty = 5,
                  alpha = 0.6) +
        # geom_line(aes(x = DIG_DateTime,
        #               y = warning_low),
        #           color = "firebrick",
        #           lty = 5,
        #           alpha = 0.9) +
        # geom_line(aes(x = DIG_DateTime,
        #               y = warning_high),
        #           color = "firebrick",
        #           lty = 5,
        #           alpha = 0.9) +
        facet_wrap(~parameter + unit, scales = "free_y", ncol = 3) +
        labs(title = glue("Diagnostics plots for {site}: {datelabel}"),
             x = "Date") +
        scale_x_datetime(breaks = "1 weeks", date_labels = "%d") +
        theme_bw() 
    return(p)
}

# Wrangling ----
make.station.site.tbl <- function(final_tbl, sites_tbl){
station_site_tbl <- final_tbl %>% 
    mutate(station = str_remove(tablename, "S") %>% as.integer()) %>% 
    distinct(station, siteid) %>% 
    inner_join(sites_tbl, by = "siteid")

return(station_site_tbl)

}

station_site_tbl <-  make.station.site.tbl(final_tbl, sites_tbl)

long_diag_tbl <- make.long.diag.tbl(diag_tbl)

# clean it
make.clean.long.diag.tbl <- function(long_diag_tbl, limits_tbl, station_site_tbl){
clean_long_diag_tbl <- long_diag_tbl %>% 
    na.omit() %>% 
    mutate(parameter = str_trim(parameter),
           unit = str_trim(unit)) %>%
    filter(value != -9999) %>% 
    inner_join(station_site_tbl, by = c("DIG_Station" = "station")) %>% 
	left_join(limits_tbl, by = join_by(parameter == parameter))

return(clean_long_diag_tbl)
}

    clean_long_diag_tbl <- make.clean.long.diag.tbl(long_diag_tbl,
                                                    limits_tbl,
                                                    station_site_tbl)


make.all.sites.plots.tbl <- function(clean_long_diag_tbl, dateon, dateoff){
all_sites_plots_tbl <- clean_long_diag_tbl %>%
    select(-DIG_Station) %>% 
    nest_by(siteid, site_name) %>% 
    mutate(plot = list(make.clean.plot(data,
                                  site = site_name,
                                  {{dateon}},
                                  {{dateoff}})))
return(all_sites_plots_tbl)
}

all_sites_plots_tbl <- make.all.sites.plots.tbl(clean_long_diag_tbl, dateon, dateoff)

all_sites_plots_tbl$plot

#---------------------------

# all_sites_plots_tbl %>%
#     filter(siteid == 203) %>% 
#     ungroup() %>% 
#     select(data) %>% 
#     pluck(1, 1) %>% 
#     transmute(DIG_DateTime,
#               param_unit = glue("{parameter}_{unit}"),
#               value) %>% 
#     pivot_wider(id_cols = DIG_DateTime,
#                 names_from = param_unit,
#                 values_from = value)
#     
#     
#     write_csv(file = "wells_road_diagnostics.csv")
# 
    
# Next step ---
    

    
# ensure all possible names are included.
# join tbl to the diag_tbl
# filter for sites \ parameters where out of bounds.
# display in formatted gt::table to identify excursions

# develop function to extract and plot single site diagnostics for sending to ESU




