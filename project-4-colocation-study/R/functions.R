# # Libraries - move to _targets ----
# 
# library(pacman)
# p_load(char = c(
#        "tidyverse",
#        "lubridate",
#        "httr2",
#        "jsonlite",
#        "glue",
#        "janitor",
#        "fs",
#        "padr",
#        "fs",
#        "collapse",
#        "tidymodels",
#        "ggside",   # side plots of density
#        "ggpubr",   # easy labelling of equations on the plot
#        "openair",
#        "easystats")
#     )
# 
# # Source other functions
# 
# source("../../airquality_GIT/ods-import-httr2.R")
# source("../../airquality_GIT/gg_themes.R")
# 
# 
# # Variables ----
# start_date <- "2022-05-01" # BTW started operating

# Functions ----

#   Get Sensor Data ----

# 3.1.1 Get data from the sensor that is not registered on the
# sensor.community. This is available through the madavi.de API
# as csv and zip files
get.zip.file.urls <- function(
        data_root_url = "https://api-rrd.madavi.de/data_csv/",
        sensor = "esp8266-6496445",
        start_date) {
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

get.daily.csv.urls <- function(
        data_root_url = "https://api-rrd.madavi.de/data_csv/",
        sensor = "esp8266-6496445") {
    first_of_month <- strftime(Sys.Date(), "%Y-%m-01") %>%
        as.Date()
    
    seq_dates_curr_month <- seq.Date(from = first_of_month,
                                     to = Sys.Date(),
                                     by = "day")
    
    daily_csv_urls = glue(
        "{data_root_url}csv-files/{seq_dates_curr_month}/data-{sensor}-{seq_dates_curr_month}.csv"
    )
    return(daily_csv_urls)
}
# read the daily csv files
get.sds.csv.tbl <- function(filename) {
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
    
    sds_csv_tbl <- read_delim(filename, delim = ";", col_types = colspec)
        return(sds_csv_tbl)
    }

# read the zip and csv files and combine

get.daily.files.tbl <- function(daily_csv_urls){
    
        daily_files_tbl <- daily_csv_urls %>%
        map_df(~ get.sds.csv.tbl(.x))
        return(daily_files_tbl)
    
}

get.monthly.files.tbl <- function(zip_file_urls){
   
    # read the zip files for each month
get.sds.zip.tbl <- function(zip_url) {
    tempdir <- "data/zips/"
    destfile <-  "data/zips/temp.zip"
    download.file(zip_url, destfile)
    
    unzip(destfile, exdir = tempdir)
    
    csv_from_zip <- dir_ls(tempdir, regexp = "[.]csv$")
    
    sds_zip_tbl <- csv_from_zip %>%
        map_df(~ get.sds.csv.tbl(.x))
    
    file_delete(dir_ls(tempdir))
    
    return(sds_zip_tbl)
    
}
    
     monthly_files_tbl <- zip_file_urls %>%
        map_df(~ get.sds.zip.tbl(.x))
    return(monthly_files_tbl)
    
}


make.temple.way.sds.hr.tbl <- function(daily_files_tbl,
                                      monthly_files_tbl) {
    
   
    madavi_combined_tbl <- daily_files_tbl %>%
        bind_rows(monthly_files_tbl) %>%
        group_by(date = ceiling_date(Time, unit = "hour")) %>%
        summarise(pm10 = mean(SDS_P1, na.rm = TRUE),
                  .groups = "drop") %>%
        filter(date >= as.POSIXct(start_date))
    
    return(madavi_combined_tbl)
}


# 3.1.2 Get sensor data from the sensor at Parson Street which is registered on sensor.community and also on the open data portal

get.parson.st.sds.hr.tbl <- function(start_date) {
    parson_st_tbl <- import_ods(
        select = "sensor_id, date, pm2_5",
        where = "sensor_id = 71552",
        date_col = "date",
        dateon = start_date,
        dateoff = Sys.Date(),
        dataset = "luftdaten_pm_bristol",
        order_by = "date") %>%
        select(date, pm2.5 = pm2_5) %>%
        filter(date > start_date %>% as.POSIXct())

        return(parson_st_tbl)
}

#   Get Reference Data ----

get.ref.tbl <- function(start_date) {
    raw_ref_tbl <-
        import_ods(
            "air-quality-data-continuous",
            select = "siteid, date_time, pm25, pm10",
            where = "(siteid = 215 OR siteid = 500) AND date_time IN [date'2022'..date'2022']",
            order_by = "siteid, date_time"
        )
    
    ref_tbl <- raw_ref_tbl %>%
        filter(date_time >= start_date) %>%
        rename(date = date_time,
               pm2.5 = pm25) 
    
        return(ref_tbl)
}

# wrangling to combine reference and low cost sensors in long format
# removing data due to faults and assigning siteids manually
make.combined.long.tbl <- function(ref_tbl,
                                   temple_way_sds_hr_tbl,
                                   parson_st_sds_hr_tbl){

combined_long_tbl <- ref_tbl %>%
    mutate(type = "reference") %>%
    mutate(pm2.5 = if_else(pm2.5 > 200, NA_real_, pm2.5)) %>%
    bind_rows(temple_way_sds_hr_tbl %>%
                  mutate(
                      siteid = 500L,
                      type = "low_cost",
                      pm2.5 = NA
                  )) %>%
    bind_rows(parson_st_sds_hr_tbl %>%
                  mutate(
                      siteid = 215L,
                      type = "low_cost",
                      pm10 = NA
                  ))
return(combined_long_tbl)

}

make.model.data.tbl <- function(combined_long_tbl){
    
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
    return(model_data_tbl)
    
}

write.model.data.tbl <- function(model_data_tbl){
    path <- "data/model_date_tbl.rds"
    write_rds(model_data_tbl, path)
    return(path)
    
}

#   Plotting Functions ----


plot.drift.site.gg <- function(model_data_tbl, siteid = 215) {
    model_data_tbl %>%
        ungroup() %>%
        filter(siteid == {
            {
                siteid
            }
        }) %>%
        select(md_wide) %>%
        pluck(1, 1) %>%
        mutate(diff = reference - low_cost) %>%
        na.omit() %>%
        ggplot(aes(x = date, y = diff)) +
        geom_smooth() +
        scale_x_datetime(date_breaks = "1 month", date_labels = "%B") +
        labs(
            title = "Smoothed daily mean drift between reference and low cost sensor",
            subtitle = glue("Site ID {siteid}"),
            x = "Date",
            y = "\u00B5gm \u207B\u00B3"
        ) +
        theme_bw()
}

# function to generate scatter and density plots from the model data
# to be used to create nested plot in model output tbl
plot.scatter.site.gg <- function(model_data_tbl) {
    # create a scatter plot and side density plot with
    # equation labels on the plot
    model_data_tbl %>%
        ggplot(aes(x = reference, y = low_cost)) +
        geom_point2(alpha = 0.5) +
        geom_smooth(method = "lm") +
        geom_xsidedensity(lwd = 1) +
        geom_ysidedensity(lwd = 2) +
        scale_ysidex_continuous(guide = guide_axis(angle = 90),
                                breaks = NULL) +
        scale_xsidey_continuous(guide = guide_axis(angle = 90),
                                breaks = NULL) +
        labs(x = "Reference Instrument",
             y = "Low Cost Sensor") +
        stat_cor(label.y = 50,
                 aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"))) +
        stat_regline_equation(label.y = 44) +
        theme_web_bw()
}

plot.model.base <- function(lm_fit) {
    # take a fitted model and plot with base R
    lm_fit %>%
        pluck("fit") %>%
        plot(pch = 16,    # optional parameters to make points blue
             col = '#006EA1')
}

get.title <- function(siteid) {
    title <- case_when(
        siteid == 215 ~ ("PM2.5 at Parson Street"),
        siteid == 500 ~ ("PM10 at Temple Way"),
        TRUE ~ ""
    )
    return(title)
}

prep.timeplot.tbl <- function(model_data_tbl) {
    timeplot_tbl <- map2_df(
        .x = model_data_tbl$siteid,
        .y = model_data_tbl$md_wide,
        .f = ~ mutate(.y, siteid = .x)
    ) %>%
        mutate(
            pollutant = if_else(siteid == 215,
                                "PM2.5",
                                "PM10"),
            site = if_else(siteid == 215,
                           "Parson Street",
                           "Temple Way")
        ) %>%
        pivot_longer(
            cols = c(reference, low_cost),
            names_to = "type",
            values_to = "concentration"
        ) %>%
        padr::pad(interval = "hour",
                  group = c("siteid", "pollutant", "site", "type"))
    
        return(timeplot_tbl)
}

plot.time.series.gg <- function(timeplot_tbl, interval = "hour") {
    
    if (interval == "hour"){
        plot_data <- timeplot_tbl
        title_interval <- "hourly"
    } else if (interval == "day"){
        plot_data <- timeplot_tbl %>%
            group_by(siteid,
                     pollutant,
                     site,
                     type,
                     date = as.Date(date)) %>% 
            add_count() %>% filter(n >= 18) %>% select(-n) %>% # filter < 75% DC
            summarise(concentration = mean(concentration, na.rm = TRUE)) %>%
            padr::pad(interval = "day")
        title_interval = "daily"
    }
    
    plot_data %>% 
        mutate(sitepoll = glue("{site} {pollutant}")) %>% 
        ggplot(aes(x = date,
                   y = concentration,
                   colour = type,
                   group = type)) +
        geom_line() +
        facet_wrap(~ sitepoll,
                   ncol = 1,
                   scales = "free_y") +
        scale_color_manual(
            labels = c("Low cost", "Reference"),
            values = c("#A15766", "#1C6762")
        ) +
        labs(
            title = glue("Time series plot of {title_interval} PM at colocated sites"),
            x = "date",
            y = quickText("ugm-3"),
            colour = "Type"
        )
}

prep.sp.plot.tbl <- function(timeplot_tbl){
    sp_plot_tbl <- timeplot_tbl %>% 
        pivot_wider(id_cols = c(date, siteid, site, type),
                    names_from = pollutant,
                    values_from = concentration) %>% 
        mutate(site = paste(siteid, site, type, sep = "_") ) %>% 
        select(-siteid, -type) %>% 
        rename_with(.fn = tolower, .cols = everything())
    return(sp_plot_tbl)
}

save.png.summaryplot <- function(sp_plot_tbl, pollutant = "pm2.5"){
    clean_poll_name = janitor::make_clean_names(pollutant)
    path <- glue("plots/summary_plot_{clean_poll_name}.png")
    png(filename = path,
        height = 600,
        width = 800,
        units = "px", 
        res = 100)
    
    sp_plot_tbl %>%
        summaryPlot(pollutant = {{pollutant}},debug = TRUE,
                    main = glue("Summary plot of colocated {pollutant} data"),
                    xlab = "Date",
                    ylab = "ugm-3"
        )
    
    dev.off()
    return(path)
}





save.plot <- function(siteid, plot){
    ggsave(filename = glue("scatterplot_{siteid}.png"),
           plot = plot, device = "png",
           path = "plots",
           dpi = "print")
}




# Model Functions -----

make.model.output.tbl <- function(model_data_tbl){
    model_output_tbl <- model_data_tbl %>%
        mutate(
            model_obj = map(md_wide, ~pluck(.x) %>% 
                                lm(low_cost ~ reference, data = .)),
            coefs = map(model_obj, ~ pluck(.x) %>%
                            tidy()),
            perf = map(model_obj, ~ pluck(.x) %>%
                           glance()),
            cor_test = map(md_wide,
                           ~ cor_test(.x,
                                      "reference",
                                      "low_cost") %>%
                               as_tibble()),
            plot = map(
                md_wide,
                ~ plot.scatter.site.gg(.) +
                    labs(title = quickText(
                        glue("Scatter plot of {get.title(siteid)} (ugm-3)")
                    ),
                    subtitle = "Reference Instrument (BAM 1020) vs. Low Cost Sensor (SDS011)")
            ),
            pollutant = if_else(siteid == 215L, "PM2.5", "PM10")
        )
    return(model_output_tbl)
}

make.dashboard <- function(model_output_tbl, siteid = 215){
    # pre fill the model_dashboard function
    dash <- partial(.f = model_dashboard,
                    parameters_args = NULL,
                    performance_args = NULL,
                    output_dir = here::here(),
                    rmd_dir = system.file("templates/easydashboard.Rmd", package = "easystats")
    )
    # iteratively generate the dashboards from the table of model objects
    dash_tbl <- model_output_tbl %>% 
        ungroup() %>% 
        filter(siteid == {{siteid}}) %>% 
        select(model_obj, pollutant)
    
    file_path <- glue("site_{siteid}_{dash_tbl$pollutant}_dashboard.html")
    
    dash(model = dash_tbl$model_obj[[1]], output_file = file_path)
    
    return(file_path)
}

