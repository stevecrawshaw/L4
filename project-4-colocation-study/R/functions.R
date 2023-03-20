# Variables ----
start_date <- "2022-05-01" # BTW started operating
end_date <- "2023-02-28"

# libraries

packages <-  c(
       "tidyverse",
       "lubridate",
       "httr2",
       "jsonlite",
       "glue",
       "janitor",
       "fs",
       "padr",
       "gt",
       "data.table",
       "tidymodels",
       "ggside",   # side plots of density
       "ggpubr",   # easy labelling of equations on the plot
       "openair",
       "easystats",
       "gtsummary",
       "gtExtras",
       "webshot2")

pacman::p_load(char = packages)

# Functions ----

#   Get Sensor Data ----

# 3.1.1 Get data from the sensor that is not registered on the
# sensor.community. This is available through the madavi.de API
# as csv and zip files
get.zip.file.urls <- function(
        data_root_url = "https://api-rrd.madavi.de/data_csv/",
        sensor = "esp8266-6496445",
        start_date,
        end_date) {
    
    seq_dates <- seq.Date(from = start_date %>% as.Date(),
                          to = end_date %>% as.Date(),
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
        sensor = "esp8266-6496445",
        end_date) {
    
    
    first_of_month <- strftime(Sys.Date(), "%Y-%m-01") %>%
        as.Date()
    
    if (as.Date(end_date) > first_of_month) { #if daily csv data required 
    
    seq_dates_curr_month <- seq.Date(from = first_of_month,
                                     to = Sys.Date(),
                                     by = "day")
    
    daily_csv_urls = glue(
        "{data_root_url}csv-files/{seq_dates_curr_month}/data-{sensor}-{seq_dates_curr_month}.csv"
    )
    return(daily_csv_urls)
    } else {
        return(NULL)
    }
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
    
    if (!is.null(daily_csv_urls)){
        daily_files_tbl <- daily_csv_urls %>%
        map_df(~ get.sds.csv.tbl(.x))
        return(daily_files_tbl)
    } else {
        return(NULL)
    }
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
                                      monthly_files_tbl,
                                      start_date, 
                                      end_date) {
process_sds <- function(sds_tbl){
    sds_tbl %>% 
        group_by(date = ceiling_date(Time, unit = "hour")) %>%
        summarise(pm10 = mean(SDS_P1, na.rm = TRUE),
                  .groups = "drop") %>%
        filter(date >= as.POSIXct(start_date),
               date <= as.POSIXct(end_date)) %>% 
        return()
}

if(!is.null(daily_files_tbl)){
    
    daily_files_tbl %>%
        bind_rows(monthly_files_tbl) %>% 
        process_sds() %>%
        return()
} else {
        monthly_files_tbl %>% 
        process_sds() %>% 
        return
    }
}

# 3.1.2 Get sensor data from the sensor at Parson Street which is registered on sensor.community and also on the open data portal

get.parson.st.sds.hr.tbl <- function(start_date,
                                     end_date) {
    parson_st_tbl <- import_ods(
        select = "sensor_id, date, pm2_5",
        where = "sensor_id = 71552",
        date_col = "date",
        dateon = start_date,
        dateoff = end_date,
        dataset = "luftdaten_pm_bristol",
        order_by = "date") %>%
        select(date, pm2.5 = pm2_5) %>%
        filter(date > start_date %>% as.POSIXct(),
               date <= end_date %>% as.POSIXct())

        return(parson_st_tbl)
}

# get temp and RH data

make.temp.rh.tbl <- function(start_date, end_date, unit = 'hour'){

raw_temp_rh_tbl <- import_ods(dataset = 'met-data-bristol-lulsgate',
                          endpoint = 'exports',
                          date_col = 'date_time', 
                          dateon = start_date,
                          dateoff = end_date,
                          format = 'csv', 
                          select = 'date_time, rh as humidity, temp as temperature'
                          )

temp_rh_tbl <- raw_temp_rh_tbl %>% 
    group_by(date = ceiling_date(date_time, unit = unit)) %>% 
    summarise(across(-date_time, ~mean(.x, na.rm = TRUE)))
return(temp_rh_tbl)
}
#   Get Reference Data ----

get.ref.tbl <- function(start_date, end_date) {
    raw_ref_tbl <-
        import_ods(
            dataset = "air-quality-data-continuous",
            select = "siteid, date_time, pm25, pm10",
            where = "siteid = 215 OR siteid = 500",
            date_col = "date_time",
            dateon = start_date,
            dateoff = end_date,
            order_by = "siteid, date_time"
        )
    
    ref_tbl <- raw_ref_tbl %>%
        filter(date_time >= start_date %>% as.Date(),
               date_time <= end_date%>% as.Date()) %>%
        rename(date = date_time,
               pm2.5 = pm25) 
    
        return(ref_tbl)
}

# wrangling to combine reference and low cost sensors in long format
# removing data due to faults and assigning siteids manually
make.combined.long.tbl <- function(ref_tbl,
                                   temple_way_sds_hr_tbl,
                                   parson_st_sds_hr_tbl,
                                   temp_rh_tbl){

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
                  )) %>% 
    left_join(temp_rh_tbl, by = join_by(date == date))


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
            md,
            ~ pluck(.x) %>%
                pivot_wider(
                    id_cols = c(date, temperature, humidity),
                    names_from = type,
                    values_from = starts_with("pm")
                ) 
        ),
        daily_wide = map( # make a daily tbl for modelling
            md_wide,
            ~pluck(.x) %>%
                group_by(dat = floor_date(date, unit = "day")) %>%
                summarise(across(-date, \(x) mean(x, na.rm = TRUE))) %>% 
                mutate(month = month(dat)) %>% 
                rename(date = dat)
        )
    )
    return(model_data_tbl)
    
}

write.model.data.tbl <- function(model_data_tbl){
    path <- "data/model_data_tbl.rds"
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
        pos_ref = as.integer(max(model_data_tbl$reference,
                                      na.rm = TRUE) * 0.8)
        pos_lc = as.integer(max(model_data_tbl$low_cost,
                                     na.rm = TRUE) * 0.7)
    model_data_tbl %>%
        na.omit() %>% 
        ggplot(aes(x = low_cost , y = reference)) +
        geom_point2(alpha = 0.3) +
        geom_smooth(method = "lm") +
        geom_xsidedensity(size = 1) +
        geom_ysidedensity(size = 2) +
        scale_ysidex_continuous(guide = guide_axis(angle = 90),
                                breaks = NULL) +
        scale_xsidey_continuous(guide = guide_axis(angle = 90),
                                breaks = NULL) +
        labs(y = "Reference Instrument",
             x = "Low Cost Sensor") +
        stat_cor(label.y = pos_ref,
                 label.x = pos_lc,
                 aes(label = paste(after_stat(rr.label),
                                   after_stat(p.label),
                                   sep = "~`,`~")),
                 geom = 'label') +
        stat_regline_equation(label.y = pos_ref + 10,
                              label.x = pos_lc, 
                              geom = 'label') +
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

save.ggplot <- function(ggplot){
    
    plot_name <- 
        deparse(substitute(ggplot))
    
    filename = glue("{plot_name}.png")
    
    ggsave(filename = filename,
           plot = ggplot,
           device = "png",
           path = "plots",
           dpi = "print")
    return(glue("/plots/{filename}"))
}

# Model Functions -----


model.parameter.test <- function(split) {
    # this function tests 3 combinations of input parameters 
    # for a lm and xgboost model
    # and outputs a tbl of performance metrics for each one
    
    # the models are run on the training data and tested on the test data
    # as defined by the rsample::initial time splits function
    
    lm_model <- linear_reg() %>%
        set_engine('lm')
    
    xg_model <- boost_tree(
    mode = 'regression',
    engine = 'xgboost',
    trees = 20
) 
    
    reference_low_cost_lm <- lm_model %>%
        fit(reference ~ low_cost,
            data = training(split))
    
    reference_low_cost_humidity_lm <- lm_model %>% 
        fit(reference ~ low_cost + humidity,
            data = training(split))
    
    reference_low_cost_humidity_temperature_lm <- lm_model %>%
        fit(reference ~ low_cost + humidity + temperature,
            data = training(split))
    
    reference_low_cost_xg <- xg_model %>%
        fit(reference ~ low_cost,
            data = training(split))
    
    reference_low_cost_humidity_xg <- xg_model %>% 
        fit(reference ~ low_cost + humidity,
            data = training(split))
    
    reference_low_cost_humidity_temperature_xg <- xg_model %>%
        fit(reference ~ low_cost + humidity + temperature,
            data = training(split))    
    
    model_test_list <- list(
        "reference_lowcost_lm" = reference_low_cost_lm,
        "reference_lowcost_humidity_lm" = reference_low_cost_humidity_lm,
        "reference_lowcost_humidity_temperature_lm" =
            reference_low_cost_humidity_temperature_lm,
        "reference_lowcost_xg" = reference_low_cost_xg,
        "reference_lowcost_humidity_xg" = reference_low_cost_humidity_xg,
        "reference_lowcost_humidity_temperature_xg" =
            reference_low_cost_humidity_temperature_xg
        
    )
    
    model_metrics <- metric_set(yardstick::rmse,
                            yardstick::rsq,
                            yardstick::mae)
    
    metric.test <- function(trained_model,
                            split,
                            model_metrics = model_metrics) {
        pred_tbl <-
            augment(trained_model, new_data = testing(split)) #metrics from test
        pred_tbl %>%
            model_metrics(truth = reference, estimate = .pred) %>%
            return()
    }
    
    
    map_dfr(model_test_list,
            ~ metric.test(.x,
                          split = split,
                          model_metrics = model_metrics),
            .id = "model") %>%
        transmute(model = str_replace_all(model, "_", " "),
                  .estimator = NULL,
                  .metric,
                  .estimate) %>%
        pivot_wider(id_cols = model,
                    names_from = .metric,
                    values_from = .estimate) %>%
        return()
}

make.model.select.tbl <- function(model_data_tbl){
    # this function creates a tbl to hold the output of the model 
    # parameter test
    
    # and creates a split object based on the daily mean data 
    # for each group (siteid)
model_select_tbl <- model_data_tbl %>% 
    select(daily_wide) %>% 
    mutate(split = daily_wide %>% 
               pluck(1) %>% 
               na.omit() %>%
               initial_time_split() %>%
               list(),
           parameter_test_results =
               model.parameter.test(split = pluck(split, 1)) %>%
               list()
           )

return(model_select_tbl)
}


make.model.select.gt <- function(model_select_tbl){
# this function creates a gt table that summarises the result of the 
    # model parameter selection exercise
model_select_gt <- model_select_tbl %>% 
    select(parameter_test_results) %>% 
    unnest(parameter_test_results) %>% 
    mutate(pollutant = if_else(siteid == 215L,
                               md("PM<sub>2.5</sub>"),
                               md("PM<sub>10</sub>")),
           .after = siteid,
           model_type = str_sub(model, -2, -1),
           model = str_sub(model, 1, -3)) %>% 
    gt() %>% 
    cols_label(pollutant = "Pollutant",
               model_type = "Model",
               model = "Model terms",
               rmse = "RMSE",
               rsq = "R squared",
               mae = "MAE") %>% 
    fmt_markdown(columns = c("pollutant")) %>% 
    fmt_number(columns = c("rmse", "rsq", "mae"),
               decimals = 3) %>% 
    tab_header("Model Selection Metrics")

return(model_select_gt)

}

plot.testing.predictions <- function(augmented_tbl, siteid){
    
    # this function plots the model's prediuctions from the augmented column
    # compared to the observed data. It is used within the 
    # make.selected.model.output function to embed the plots in that tbl
    if(siteid == 215L)
        {
        pollutant  = "PM2.5"
    } else {
            pollutant = "PM10"
    }
    
predict_plot <- augmented_tbl %>% 
    pivot_longer(cols = -date,
                 names_to = 'Parameter',
                 values_to = 'ugm-3') %>% 
    filter(Parameter %in% c (".fitted", "reference")) %>% 
    ggplot(aes(x = date, y = `ugm-3`, colour = Parameter)) +
    geom_line(linewidth = 1, alpha = 0.7) +
    labs(title = "Fitted Predictions and Measured Values for Testing Split",
         subtitle = glue("Daily mean concentrations of {pollutant} at site {siteid}"),
         x = "Date") +
    theme_ppt_single()

return(predict_plot)

}

make.selected.model.output.tbl <- function(model_select_tbl){

    # this function runs the selected model on training split
    # and full data and provides glance and tidy summaries for each
    # as well as a performance gt tbl for trained and full tbls

selected_model_output_tbl <- model_select_tbl %>% 
    mutate(model_obj_train = map(split, ~training(.x) %>% # trained model
               lm(reference ~ low_cost + humidity,
                                  data = .)),
           model_obj_full = map(daily_wide, ~pluck(.x) %>% # full data model
               lm(reference ~ low_cost + humidity,
                                  data = .)),
           tidied_train = map(model_obj_train, tidy),
           glanced_train = map(model_obj_train, glance),
           tidied_full = map(model_obj_full, tidy),
           glanced_full = map(model_obj_full, glance),
           augmented = map(.x = model_obj_train,
                            ~augment(.x, 
                                     newdata = testing(pluck(split,
                                                             1)))),
           
           check_model_train = map(model_obj_train, check_model),
           check_model_full = map(model_obj_full, check_model),
           prediction_plot  = map(augmented,
                                  ~plot.testing.predictions(.x,
                                                            siteid = siteid)),
           perf_gt_train =  map(model_obj_train,
                          ~tbl_regression(.x) %>% 
                add_glance_table()),
           perf_gt_full =  map(model_obj_full,
                          ~tbl_regression(.x) %>% 
                add_glance_table()))

return(selected_model_output_tbl)
}

make.tidied.gt <- function(selected_model_output_tbl, type = tidied_train){
    # this function makes a gt for the tidied summary
    
tidied = enquo(type)
title_suffix <- str_split_i(tidied %>% quo_name(), "_", 2)
selected_model_output_tbl %>% 
    select(!!tidied) %>% 
    unnest(!!tidied) %>% 
    gt() %>% 
    fmt_number(columns = -term, decimals = 3) %>% 
    tab_header(title = glue("Model Coefficients ({title_suffix})"))
    
}

make.glanced.gt <- function(selected_model_output_tbl, type = glanced_train){
    # this function makes a gt for the glanced summary
glanced = enquo(type)
title_suffix <- str_split_i(glanced %>% quo_name(), "_", 2)

selected_model_output_tbl %>% 
    select(!!glanced) %>% 
    unnest(!!glanced) %>% 
    select(r.squared, p.value, nobs, df.residual) %>% 
    gt() %>% 
    fmt_number(columns = c("r.squared", "p.value"), decimals = 3) %>% 
    tab_header(title = glue("Model Metrics ({title_suffix})")) %>% 
    return()
}



