p <-
    c("tidyverse",
      "lubridate",
      "glue",
      "ggside",        # side plots of density
      "ggpubr",        # easy labelling of equations on the plot
      "janitor",
      "fs",
      "easystats",
      "padr",
      "openair")
library(pacman)
p_load(char = p)
rm(p)

model_data_tbl <-
    read_rds("data/model_data_tbl.rds") # from qa_low_cost_sensors_source.r

source("../../airquality_GIT/gg_themes.R") # for nice gg themes

# Plotting Functions ----

plot_drift <- function(model_data_tbl, siteid = 215) {
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
plot_scatter <- function(model_data) {
    # create a scatter plot and side density plot with
    # equation labels on the plot
    model_data %>%
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

plot_model <- function(lm_fit) {
    # take a fitted model and plot with base R
    lm_fit %>%
        pluck("fit") %>%
        plot(pch = 16,    # optional parameters to make points blue
             col = '#006EA1')
}

get_title <- function(siteid) {
    title <- case_when(
        siteid == 215 ~ ("PM2.5 at Parson Street"),
        siteid == 500 ~ ("PM10 at Temple Way"),
        TRUE ~ ""
    )
    return(title)
}

prep_timeplot <- function(model_data_tbl) {
    map2_df(
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
                  group = c("siteid", "pollutant", "site", "type")) %>%
        return()
}

plot_time_series <- function(time_plot_data, interval = "hour") {
    
    if (interval == "hour"){
        plot_data <- time_plot_data
        title_interval <- "hourly"
    } else if (interval == "day"){
        plot_data <- time_plot_data %>%
            group_by(siteid,
                     pollutant,
                     site,
                     type,
                     date = as.Date(date)) %>% 
            add_count() %>% filter(n >= 18) %>% select(-n) %>% # filter < 75% DC
            summarise(concentration = mean(concentration, na.rm = TRUE)) %>%
            pad(interval = "day")
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

prep.sp.plot.data <- function(time_plot_data){
    sp_plot_data <- time_plot_data %>% 
        pivot_wider(id_cols = c(date, siteid, site, type),
                    names_from = pollutant,
                    values_from = concentration) %>% 
        mutate(site = paste(siteid, site, type, sep = "_") ) %>% 
        select(-siteid, -type) %>% 
        rename_with(.fn = tolower, .cols = everything())
    return(sp_plot_data)
}

save.png.pm25 <- function(){
    
    png(filename = "plots/sp_pm25.png",
        height = 600,
        width = 800,
        units = "px", 
        res = 100)
    
    
    sp_plot_data %>%
        select(-pm10) %>% 
        filter(!is.na(pm2.5)) %>% 
        summaryPlot(pollutant = "pm2.5",debug = TRUE,
                    main = "Summary plot of colocated PM2.5 data",
                    xlab = "Date",
                    ylab = "ugm-3"
        )
    
    dev.off()
    
}

save.png.pm10 <- function(){
    
    png(filename = "plots/sp_pm10.png",
        height = 600,
        width = 800,
        units = "px", 
        res = 100)
    
    sp_plot_data %>%
        select(-pm2.5) %>% 
        filter(!is.na(pm10)) %>% 
        summaryPlot(pollutant = "pm10",
                    main = "Summary plot of colocated PM10 data",
                    xlab = "Date",
                    ylab = "ugm-3")
    
    
    dev.off()
}

save.plot <- function(siteid, plot){
    ggsave(filename = glue("scatterplot_{siteid}.png"),
           plot = plot, device = "png",
           path = "plots",
           dpi = "print")
}

# Time Plot ----

time_plot_data <- prep_timeplot(model_data_tbl)

daily_time_plot <- plot_time_series(time_plot_data, interval = "day")

ggsave("daily_time_series.png", daily_time_plot, device = "png", path = "plots")

sp_plot_data <- prep.sp.plot.data(time_plot_data)

save.png.pm25()
save.png.pm10()

drift_plot_215 <- plot_drift(model_data_tbl, siteid = 215)
drift_plot_500 <- plot_drift(model_data_tbl, siteid = 500)

ggsave("drift_plot_215.png", drift_plot_215, device = "png", path = "plots")
ggsave("drift_plot_500.png", drift_plot_500, device = "png", path = "plots")



