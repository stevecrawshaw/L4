libs <- c("tidyverse", "fastverse", "here", "lubridate", "openair", "glue", "janitor", "viridis", "ggExtra", "openairmaps", "fs", "tidyquant")
library(xfun)
pkg_attach2(libs)
# get the data with functions from this script
source("../airquality_GIT/importODS.R")
# install.packages("remotes")
# remotes::install_github("davidcarslaw/openairmaps")
#library("openairmaps")
# define variables
date_on <- "2021-11-10"
date_off <- "2022-03-07 23:59:59"
nicedate_fnc <- function(datestring){
    paste(str_sub(datestring, 9, 10),
    str_sub(datestring, 6, 7),
    str_sub(datestring, 1, 4),
    sep = "/")
}
sensor_id <- "66963"

my_format_fnc <- function(datestring){
format(datestring %>% as.Date(), "%b %Y")
}

# vector of STS sensors
sts_sensors_vec <- 
c("66963", 
"66966",
"66970",
"66972",
"66974",
"66979",
"66987",
"67568",
"67655",
"67665")
# function for ggplot theme

nicetheme <- function(){
  theme_bw() +
theme(legend.position = "bottom") +
  theme(plot.title = element_text(size = 12)) +
  theme(plot.subtitle = element_text(size = 10)) +
  theme(axis.text.y = element_text(size = 6)) +
  theme(strip.background = element_rect(colour = "white")) +
  theme(plot.title = element_text(hjust = 0)) +
  theme(axis.ticks = element_blank()) +
  theme(axis.text = element_text(size = 7)) + 
  theme(legend.title = element_text(size = 10), ) +
  theme(legend.text = element_text(size = 6)) +
  removeGrid()#ggExtra
  
}
# function to create search string for ODS box from vector
field_filter_str_fnc <- function(field_name = "siteid", values_vec = c("203", "215")){
# function to turn a vector of values into a search string for the ODS SQL API
    field_assign <- str_glue("{field_name} = ")
    field_collapse = str_glue(" OR {field_assign}")

pasted_str <- paste0(values_vec, collapse = field_collapse)
ods_search_str <- str_glue("{field_assign}{pasted_str}")
return(ods_search_str)

}
# paste into search box on portal
sts_sensors <- field_filter_str_fnc(field_name = "sensor_id",
                                    values_vec = sts_sensors_vec)
#get data from ALL sensors for the period

if(!file.exists("data/ld_all_raw_tbl.rds")){

ld_all_raw_tbl <- getODSExport(select_str = "sensor_id, date, pm10, pm2_5, geo_point_2d",
                               date_col = "date",
                               dateon = date_on,
                               dateoff = date_off,
                               where_str = NULL,
                               refine = NULL,
                               apikey = NULL,
                               dataset = "luftdaten_pm_bristol") %>% 
    rename(pm2.5 = pm2_5)

write_rds(ld_all_raw_tbl, file = "data/ld_all_raw_tbl.rds")

} else {

    ld_all_raw_tbl <- read_rds(file = "data/ld_all_raw_tbl.rds")
    
}

# put into format needed for openairmaps
ld_all_tbl <- ld_all_raw_tbl %>% 
    separate(geo_point_2d,
             into = c("latitude", "longitude"),
             sep = ",",
             convert = TRUE) %>% 
    mutate(sensor_id = as_factor(sensor_id)) %>% 
    select(date, pm2.5, pm10, sensor_id, latitude, longitude) 

# Get met data for specified period
if(!file.exists("data/met_raw.rds")){
met_raw <- getODSExport(select_str = "date_time, temp, ws, wd, rh",
                        where_str = "",
                        date_col = "date_time",
                        dateon = date_on,
                        dateoff = date_off,
                        refine = NULL,
                        apikey = NULL,
                        dataset = "met-data-bristol-lulsgate")

write_rds(met_raw, "data/met_raw.rds")

} else {
    met_raw <- read_rds(file = "data/met_raw.rds")
}

met_proc_tbl <- met_raw %>% 
    select(date = date_time, ws, wd, rh, temp) %>% 
    timeAverage(avg.time = "hour")

maen_fnc <- function(dttm){
    # function to assign period label to 
    # observation depending on date time
    hr <- hour(dttm)
    maen <- case_when(
        hr >= 18L ~ "Evening",
        hr <= 6L ~ "Night",
        hr > 6 & hr <= 12 ~ "Morning",
        TRUE ~ "Afternoon"
        )
    return(maen)
}

# join met data for single polar plot
joined_tbl <- ld_all_raw_tbl %>% 
    left_join(met_proc_tbl, by = "date") %>% 
    mutate(period = maen_fnc(date))

hours <- difftime(as_datetime(date_off), as_datetime(date_on), units = "hours") %>% 
  as.integer()

sts_dc_tbl <- joined_tbl %>% 
  group_by(sensor_id) %>% 
  summarise(dc = n() / hours, .groups = "drop") %>% 
  filter(dc >= 0.85) %>% 
  mutate(sts = if_else(
    sensor_id %in% sts_sensors_vec, "Slow the Smoke", "City"
  ))

plot_png_fnc <- function(data, sensor_id, date_day, filename){
gc()
    pp <- polarPlot(data,
            pollutant = "pm2.5",
            main = glue("{sensor_id} \n {date_day %>% nicedate_fnc()}"),
            k = 10,
            statistic = "max",
            # resolution = "fine",
            # upper = 15
            limits = c(0, 30)
            # type = "period"
            # normalise = TRUE
  ) 
  
  png(filename = filename)
  pp %>% 
    pluck("plot") %>% 
    plot() # must explicitly call plot
  dev.off() %>% 
    return()
}

fs::dir_create(path = "images", sts_sensors_vec)

    # mutate(ws = ws / max(ws, na.rm = TRUE)) %>% 

  nest_prep_fnc <- function(data){
    data %>% 
    nest_by(sensor_id, date_day = as.Date(date)) %>% 
    # head(5) %>% 
    filter(nrow(data) == 24,
           sensor_id %in% sts_sensors_vec) %>% 
    ungroup() %>%
    # slice_sample(n = 20) %>%
  arrange(sensor_id, date_day) %>% 
  group_by(sensor_id) %>% 
  mutate(filename = glue("images/{sensor_id}/pp_{row_number() + 1000}.png")) %>% 
    rowwise() 
  }

pp_tbl <-  joined_tbl %>% 
  select(-geo_point_2d, -rh, -temp) %>% 
  nest_prep_fnc()
  
  pp_tbl %>% 
  relocate(data, sensor_id, date_day, filename) %>% 
  pwalk(.f = plot_png_fnc)
  
  # Openair maps -----
  
  ld_all_met_tbl <- ld_all_tbl %>% 
    left_join(met_proc_tbl, by = "date") %>% 
    na_omit(cols = c("ws", "wd")) %>% 
    select(-c(rh, temp)) %>% 
    nest_prep_fnc()
  
  ld_all_met_tbl[1, "data"] %>% pluck(1, 1)
  
  polarMap(ld_all_met_tbl,
           dir_polar = "plots/polar",
           alpha = 0.5,
           pollutant = "pm10",
           x = "ws",
           k = 20,
           latitude = "latitude",
           longitude = "longitude",
           provider = "OpenStreetMap",
           type = "sensor_id",
           cols = cls)
  #-----------------------------------------

# now make the animated gif
path <- "images/orig/"

fullpath <- glue("{path}{sts_sensors_vec}")

make_gif <- function(fullpath){
  files <- fs::dir_ls(path = fullpath, glob = "*.png")
  outpath <- "images/"
  sensor_id <- stringr::str_sub(fullpath, -6, -1)
  sensor_img <- image_read(files)
  
  sensor_img %>% 
    image_animate(optimize = TRUE, delay = 50) %>% 
    image_write(path = glue("{outpath}{sensor_id}.gif"))
}

walk(fullpath, .f = make_gif)

# Plot mean pm2.5 showing sts sensors
colors <-  paletteer::palettes_d$jcolors$pal2[c(1, 3)]

colors %>% scales::show_col()

stats_tbl <- joined_tbl %>% 
  inner_join(sts_dc_tbl, by = "sensor_id")

exc_tbl <- stats_tbl %>%
  group_by(sensor_id, sts, date_day = as.Date(date)) %>% 
  summarise(PM2.5 = mean(pm2.5, na.rm = TRUE),
            exc15 = if_else(PM2.5 >= 15, TRUE, FALSE)) %>% 
  summarise(count_exc15 = sum(exc15))

mean_tbl <- stats_tbl %>% 
  group_by(sensor_id, sts) %>% 
  summarise(PM2.5 = mean(pm2.5, na.rm = TRUE), .groups = "keep")

dc_sts_p <- mean_tbl %>%
    ggplot(aes(x = reorder(x = sensor_id, PM2.5),
             y = PM2.5,
             fill = sts)) +
  geom_col(alpha = 0.7) + 
  coord_flip() +
  labs(title = quickText("Do the StS sensors show higher PM2.5 levels than other Bristol sensors?"),
       subtitle = "Data capture better than 85% (only 4 sensors met this)",
       fill = "Sensor:",
       x = "Sensor ID",
       y = quickText(glue("Average PM2.5 for period {my_format_fnc(date_on)} to {my_format_fnc(date_off)}"))) +
  # scale_fill_manual(values = colors) +
  scale_fill_tq() +
  nicetheme() 

dc_sts_p

exc_sts_p <- exc_tbl %>%
  ggplot(aes(x = reorder(x = sensor_id, count_exc15),
             y = count_exc15,
             fill = sts)) +
  geom_col(alpha = 0.7) + 
  coord_flip() +
  labs(title = quickText("Do the StS sensors show more exceedences than other Bristol sensors?"),
       subtitle = "Data capture better than 85% (only 4 sensors met this)",
       fill = "Sensor:",
       x = "Sensor ID",
       y = quickText(glue("Number of exceedences of WHO PM2.5 guideline for period {my_format_fnc(date_on)} to {my_format_fnc(date_off)}"))) +
  # geom_hline(yintercept = 4) +
  # annotate("text",
  #          x = 4,
  #          y = 8,
  #          label = "4 exceedences allowed per year") +
  # scale_fill_manual(values = colors) +
  scale_fill_tq() +
  nicetheme()

exc_sts_p

ggsave("plots/exceedences_STS_barplot.png",
       plot = exc_sts_p,
       scale = 1,
       # width = 700,
       # height = 400,
       # units = "px",
       dpi = 200)



ggsave("plots/datacapture_STS_barplot.png",
       plot = dc_sts_p,
       scale = 1,
       # width = 700,
       # height = 400,
       # units = "px",
       dpi = 200)

# other openair plots ----

scatterPlot(joined_tbl, x = "pm10", y = "pm2.5", method = "hexbin", col = "jet",
            border = "grey", xbin = 10)

scatterPlot(joined_tbl,
            x = "date",
            y = "pm2.5",
            cols = "firebrick",
            pch = 16,
            col = "red",
            alpha = 0.5,
            windflow = list(scale = 0.15, lwd = 2),
            key = TRUE,
            key.footer = "pm2.5\n (ugm-3)")

cls <- openColours(c( "darkgreen", "yellow", "red", "purple"), 10)  
# Trendlevel ----

ld_time_tbl <- ld_raw %>% 
    mutate(day = (as_date(date)),
           hour = (hour(date)),
           month = month(date, label = TRUE, abbr = FALSE),
           year = year(date))

ld_time_tbl %>% 
    trendLevel(pollutant = "pm2.5", x = "day", cols = cls)

# Heatmap ggplot2 ----

title <- expression(paste(expression("PM" [2.5] * " " * mu * "g m" ^-3 * "at sensor "), sensor_id))

p <- ld_time_tbl %>% 
    ggplot(aes(day, hour, fill = pm2.5))+
    geom_tile(color = "white", size = 0.1) + 
    # scale_fill_viridis(name = expression("PM" [2.5] * " " * mu * "g m" ^-3 * "  "),
    #                    option = "C") +
    scale_fill_gradientn(colours = cls) +
    facet_grid(year ~ month) +
    scale_y_continuous(trans = "reverse", breaks = unique(ld_time_tbl$hour)) +
    scale_x_continuous(breaks = unique(ld_time_tbl$day)) +
    theme_minimal(base_size = 8) +
    labs(title= glue("Heatmap for Sensor: {sensor_id}"),
         x = "Day",
         y = "Hour Commencing") +
    theme(legend.position = "bottom") +
    theme(plot.title = element_text(size = 14)) +
    theme(axis.text.y = element_text(size = 6)) +
    theme(strip.background = element_rect(colour = "white")) +
    theme(plot.title = element_text(hjust = 0)) +
    theme(axis.ticks = element_blank()) +
    theme(axis.text = element_text(size = 7)) + 
    theme(legend.title = element_text(size = 10), ) +
    theme(legend.text = element_text(size = 6)) +
    removeGrid()#ggExtra


p


# Polar plot - multiple ---- 
#            pm25_plot = list(polarPlot(., pollutant = "pm2.5", main = glue("PM2.5 Polar Plot: Sensor {sensor_id}"))))



# joined_tbl <- ld_raw %>% 
#     select(date, sensor_id, pm2.5 = pm2_5, everything()) %>% 
#     left_join(met_proc_tbl, by = "date") %>% 
#     filter(sensor_id %in% c(7675, 10491)) %>% # for testing
#     nest_by(sensor_id) %>% 
#     mutate(pm10_plot = list(polarPlot(data, pollutant = "pm10", main = glue("PM10 Polar Plot: Sensor {sensor_id}"))),
#            pm25_plot = list(polarPlot(data, pollutant = "pm2.5", main = glue("PM2.5 Polar Plot: Sensor {sensor_id}"))))

# joined_tbl$pm10_plot[[2]][[1]]


# polarPlot SIngle ----
single_tbl <- ld_raw %>% 
    # select(date, sensor_id, pm2.5, everything()) %>% 
    left_join(met_proc_tbl, by = "date")

pp <- polarPlot(single_tbl,
                pollutant = "pm10",
                cols = cls,
                main = glue("Polar plot for PM10 at sensor: {sensor_id}"),
                uncertainty = FALSE)

# Calendar Plot AQ Index
labels <- c("1 - Low", "2 - Low", "3 - Low", "4 - Moderate", "5 - Moderate",
            "6 - Moderate", "7 - High", "8 - High", "9 - High", "10 - Very High")
# o3.breaks <-c(0, 34, 66, 100, 121, 141, 160, 188, 214, 240, 500)
# no2.breaks <- c(0, 67, 134, 200, 268, 335, 400, 468, 535, 600, 1000)
pm10.breaks <- c(0, 17, 34, 50, 59, 67, 75, 84, 92, 100, 1000)
pm25.breaks <- c(0, 12, 24, 35, 42, 47, 53, 59, 65, 70, 1000)


days <- ld_time_tbl %>%
    select(day) %>% 
    n_distinct() 

if(days > 7){
    
}

pm25_calplot <- ld_time_tbl %>% 
    calendarPlot(pollutant = "pm2.5",
                 labels = labels,
                 statistic = "mean",
                 breaks = pm25.breaks,
                 annotate = "value")

pm10_calplot <- ld_time_tbl %>% 
    calendarPlot(pollutant = "pm10",
                 labels = labels,
                 statistic = "mean",
                 breaks = pm10.breaks,
                 annotate = "value",
                 cols = cls)


