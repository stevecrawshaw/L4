pacman::p_load(
        tidyverse,
        lubridate,
        openair,
        glue,
        janitor,
        viridis,
        ggExtra,
        openairmaps,
        htmlwidgets,
        tidyquant,
        fs
    )
# get the data with functions from this script
source("../../airquality_GIT/ods-import-httr2.R")
# define variables
date_on <- "2021-11-10"
date_off <- "2022-12-31 23:59:59"
nicedate_fnc <- function(datestring){
    strftime(datestring, format = "%d/%m/%Y") %>% 
        return()
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
  theme(plot.title = element_text(linewidth = 12)) +
  theme(plot.subtitle = element_text(linewidth = 10)) +
  theme(axis.text.y = element_text(linewidth = 6)) +
  theme(strip.background = element_rect(colour = "white")) +
  theme(plot.title = element_text(hjust = 0)) +
  theme(axis.ticks = element_blank()) +
  theme(axis.text = element_text(linewidth = 7)) + 
  theme(legend.title = element_text(linewidth = 10), ) +
  theme(legend.text = element_text(linewidth = 6)) +
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

# bcc reference sensors
bcc_ref_tbl <- 
    import_ods(dataset = "air-quality-data-continuous",
               endpoint = "exports",
               group_by = NULL,
               date_col = "date_time",
               dateon = date_on,
               dateoff = date_off,
               where = "siteid in (452, 215, 500)",
               format = "csv",
               select = "date_time, location, pm10, pm25, geo_point_2d") %>%
    rename(pm2.5 = pm25,
           sensor_id = location,
           date = date_time) 


if(!file.exists("data/ld_all_raw_tbl.rds")){

    ld_all_raw_tbl <- import_ods(dataset = "luftdaten_pm_bristol",
                              endpoint = "exports",
                              group_by = NULL,
                              date_col = date,
                              dateon = date_on,
                              dateoff = date_off,
                              format = "csv",
                              select = "date, sensor_id, pm10, pm2_5, geo_point_2d",
                              where = NULL) %>% 
    rename(pm2.5 = pm2_5)

write_rds(ld_all_raw_tbl, file = "data/ld_all_raw_tbl.rds")

} else {

    ld_all_raw_tbl <- read_rds(file = "data/ld_all_raw_tbl.rds")
    
}

# join bcc and ld raw sensors

ld_plus_bcc_tbl <- bind_rows(bcc_ref_tbl, ld_all_raw_tbl)

# put into format needed for openairmaps
ld_all_tbl <- ld_plus_bcc_tbl %>% 
    separate(geo_point_2d,
             into = c("latitude", "longitude"),
             sep = ",",
             convert = TRUE) %>% 
    mutate(sensor_id = as_factor(sensor_id)) %>% 
    select(date, pm2.5, pm10, sensor_id, latitude, longitude) 

# Get met data for specified period
if(!file.exists("data/met_raw.rds")){
met_raw <- import_ods(select = "date_time, temp, ws, wd, rh",
                        where = NULL,
                        date_col = "date_time",
                        dateon = date_on,
                        dateoff = date_off,
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
joined_tbl <- ld_all_tbl %>% 
    left_join(met_proc_tbl, by = "date") %>% 
    mutate(period = maen_fnc(date)) %>% 
    filter(between(date, as_datetime(date_on), as_datetime(date_off)))

hours <- difftime(as_datetime(date_off),
                  as_datetime(date_on), 
                  units = "hours") %>% 
    as.integer()


sts_dc_tbl <- joined_tbl %>% 
  group_by(sensor_id) %>% 
  summarise(dc = n() / hours, .groups = "drop") %>% 
  filter(dc >= 0.85) %>% 
  mutate(sts = if_else(
    sensor_id %in% sts_sensors_vec, "Slow the Smoke", "City"
  ),
  sensor_id = sensor_id %>% fct_drop())


plot_png_fnc <- function(data, sensor_id, date_day, filename){
    # this function plots a polar plot for a given site and date
    # and saves to file - used for making the polar plot individual files for the 
    # creation of an animated GIF
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

# fs::dir_create(path = "images", sts_sensors_vec)

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
  select( -rh, -temp) %>% 
  nest_prep_fnc()
  # this writes the plots to file below
  pp_tbl %>% 
  relocate(data, sensor_id, date_day, filename) %>% 
  pwalk(.f = plot_png_fnc)
  
  # Openair maps -----
  
high_dc_sensors <- sts_dc_tbl$sensor_id %>% as.character()
  
nest_ppmaps <- function(data) {
    data %>%
    nest_by(year_month = as.Date(date) %>% strftime("%Y%m")) %>%
    ungroup() %>%
    arrange(year_month) %>%
    mutate(filename = glue("images/ppmap/pmap_{year_month}.html")) %>%
    rowwise()
}

# joined_tbl %>% 
#     transmute(week_year = as.Date(date) %>% strftime("%W-%Y"))
  
  ld_all_met_tbl <- joined_tbl %>% 
    collapse::na_omit(cols = c("ws", "wd")) %>% 
    select(-c(rh, temp)) %>% 
      filter(sensor_id %in% high_dc_sensors) %>% 
  mutate(sensor_id = fct_drop(sensor_id)) %>% 
      nest_ppmaps()
  
   # test_tbl <- ld_all_met_tbl[1, "data"] %>% pluck(1, 1)
   # test_tbl
   # 
   # map_dfr(ld_all_met_tbl$data, ~pluck(.x))
cls <- openColours(c( "darkgreen", "yellow", "red", "purple"), 10) 
   
save.month.polarmaps <- function(year_month, data, filename){

pmap <-   polarMap(data,
           alpha = 0.5,
           limits = c(0, 25),
           draw.legend = TRUE,
           pollutant = "pm2.5",
           control = "period",
           x = "ws",
           k = 100,
           latitude = "latitude",
           longitude = "longitude",
           provider = "OpenStreetMap",
           popup = "sensor_id",
           label = "sensor_id",
           cols = cls)

saveWidget(pmap, file=filename)

}

# this will take long
# ld_all_met_tbl %>% 
#     pwalk(save.month.polarmaps)


# a single map showing season and daytime period
unnested_tbl <- ld_all_met_tbl %>% 
    unnest(cols = data) %>% 
    mutate(year_month = NULL,
           filename = NULL) %>% 
    cutData(type = "season") %>% 
    mutate(season_period = glue('{case_when(
        season == "autumn (SON)" ~ "4",
        season == "winter (DJF)" ~ "1",
        season == "spring (MAM)" ~ "2",
        season == "summer (JJA)" ~ "3"
        )} {toupper(str_sub(season, 1, 1))}{str_sub(season, 2)} {period}'
        )
        )

unnested_tbl %>% 
    saveRDS('data/unnested_tbl.rds')

unnested_tbl <- read_rds("data/unnested_tbl.rds")

plot.polarmap <- function(data, pollutant = c("pm10", "pm2.5")){

    if(pollutant == "pm10"){
        limits = c(0, 50)
        file = "season_period_pm10.html"
    } else if (pollutant == "pm2.5"){
        limits = c(1, 25)
        file = "season_period_pm25.html"
    } else {
        stop()
    }

pmap <-   polarMap(data,
                   alpha = 0.5,
                   limits = limits,
                   draw.legend = TRUE,
                   pollutant = pollutant,
                   control = "season_period",
                   x = "ws",
                   k = 100,
                   latitude = "latitude",
                   longitude = "longitude",
                   provider = "OpenStreetMap",
                   popup = "sensor_id",
                   label = "sensor_id",
                   cols = cls)

saveWidget(pmap,  file = file)
}

plot.polarmap(unnested_tbl, pollutant = "pm10")

plot.polarmap(unnested_tbl, pollutant = "pm2.5")



static_polar_pm25 <- unnested_tbl %>% 
    rename(lat = latitude,
           lon = longitude) %>% 
    mutate(sensor_id = as.character(sensor_id)) %>% 
    filter(sensor_id != "Parson Street School") %>% 
    polarMapStatic(pollutant = "pm2.5",
                   limits = c(1, 20),
                   facet = 'season',
                   facet.nrow = 2,
                   alpha = 0.6, cols = cls, d.icon = 30)

ggsave('images/static_polar_pm25.png',
       static_polar_pm25,
       device = "png", width = 1200, height = 900, units = "px")

polar_data %>%
    openair::cutData("daylight") %>%
    polarMapStatic(
        pollutant = "no2",
        limits = c(0, 180),
        facet = "daylight",
        facet.nrow = 2, 
        alpha = .9
    )


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
  tidyquant::scale_fill_tq() +
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
  tidyquant::scale_fill_tq() +
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

 
# Trendlevel ----

ld_time_tbl <- unnested_tbl %>% 
    filter(sensor_id == 10491) %>%
    mutate(day = (as_date(date)),
           hour = (hour(date)),
           month = month(date, label = TRUE, abbr = FALSE),
           year = year(date)) %>% 
    filter(year == 2022)


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


who_breaks_pm2.5 <- rev(c(100, 75, 50, 37.5, 25, 15))
who_labels_pm2.5 <- rev(c("IT1", "IT2", "IT3", "IT4", "AQG"))


who_breaks_pm10 <- rev(c(100, 70, 50, 30, 20, 15))
who_labels_pm10 <- rev(c("IT1", "IT2", "IT3", "IT4", "AQG"))
days <- ld_time_tbl %>%
    select(day) %>% 
    n_distinct() 

if(days > 7){
    
}

cal_chart_cols <- cls[4:9]

pm25_calplot <- ld_time_tbl %>% 
    calendarPlot(pollutant = "pm2.5",
                 labels = who_labels_pm2.5,
                 statistic = "mean",
                 breaks = who_breaks_pm2.5,
                 annotate = "value")


max_pm10_tbl <- unnested_tbl %>% 
    filter(year(date) == 2022) %>% 
    transmute(date = as_date(date),
           pm10, sensor_id) %>% 
    group_by(date, sensor_id) %>% 
    summarise(pm10 = mean(pm10, na.rm = TRUE)) %>% 
    summarise(pm10 = max(pm10, na.rm = TRUE))

    
        max_pm10_tbl

pm10_calplot <- max_pm10_tbl %>% 
    calendarPlot(pollutant = "pm10",
                 labels = labels,
                 statistic = "mean",
                 breaks = pm10.breaks,
                 annotate = "value",
                 key.header = "UK Daily Air\n Quality Index",
                 cols = cls,
                 main = "Maximum daily mean PM10 at 10 sites with good data capture")

scales::show_col(cal_chart_cols)


data_cap_chart <- ld_all_tbl %>% 
    filter(sensor_id %in% sts_sensors_vec) %>% 
    mutate(sensor_id = fct_drop(sensor_id)) %>% 
    ggplot(aes(x = date, y = sensor_id)) +
    geom_point(size = 1, colour = "firebrick") +
    labs(title = "Data Capture Chart",
         subtitle = "Slow the Smoke Sensors",
         x = "Date",
         y = "Sensor ID",
         caption = glue("From {as.Date(date_on)} to {as.Date(date_off)}"),
         colour = "Collected data") +
    theme_bw() +
        theme(panel.border = element_blank())

ggsave(filename = "plots/data_capture_chart.png",
       plot = data_cap_chart,
       device = "png")

ld_all_tbl %>% 
    filter(sensor_id %in% sts_sensors_vec) %>% 
    mutate(sensor_id = fct_drop(sensor_id)) %>% 
    group_by(sensor_id) %>% 
    summarise(data_capture = (n() * 100 / hours) %>% round(1)) %>% 
    arrange(desc(data_capture)) %>% 
    write_csv("data/datacap.csv")

tv_plot_tbl <- ld_all_tbl %>% 
    filter(sensor_id %in% 66970,
           year(date) == 2022) %>% 
    select(date, pm2.5, pm10)
    
tv_plot <- tv_plot_tbl %>% 
    timeVariation(pollutant = c("pm2.5", "pm10"),
                  main = "Time Variation Plot at 66970 (The Yard): 2022")

png(filename = "plots/tv_plot_66970.png", width = 750, height = 700, units = "px")
tv_plot
dev.off()

aurns_tbl <- importMeta(source = "aurn", all = TRUE)
aurns_tbl %>% glimpse()

pm_sites <- aurns_tbl %>% 
    filter(variable %in% c("PM10", "PM2.5"),
           site_type == "Urban Background",
           end_date == "ongoing") %>% 
    distinct(code) %>% 
    pull()

pm_aurn_tbl <- importAURN(site = pm_sites,
                          year = 2022,
                          pollutant = c("pm10", "pm2.5"),
                          data_type = "annual",
                          to_narrow = TRUE) %>% 
    filter(species %in% c("pm10", "pm2.5"),
           !is.na(value)) %>% 
    select(site, species, value) %>% 
    pivot_wider(id_cols = site, names_from = species, values_from = value) %>% 
    na.omit()
    



brs8 <- importAURN(site = "BRS8", year = 2022, pollutant = c("pm10", "pm2.5"))

my1 <- importAURN(site = "my1", year = 2022, pollutant = c("pm10", "pm2.5"))

find.ratio <- . %>% 
    summarise(pm10 = mean(pm10, na.rm = TRUE),
              pm2.5 = mean(pm2.5, na.rm = TRUE)) %>% 
    mutate(ratio = pm10/pm2.5)

tv_plot_tbl %>% find.ratio()

my1 %>% find.ratio()

pm_aurn_tbl %>%
    group_by(site) %>%
    find.ratio() %>% 
    arrange(desc(ratio))
