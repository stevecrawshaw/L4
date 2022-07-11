p <-
    c(
        "tidyverse",
        "lubridate",
        "glue",
        "janitor",
        "fs",
        "collapse",
        "tidymodels",
        "parsnip",
        "ggside",
        # side plots of density
        "ggpubr",
        # easy labelling of equations on the plot
        "openair",
        "easystats",
        "padr"
    )
library(xfun)
pkg_attach2(p)
rm(p)

model_data_tbl <-
    read_rds("data/model_data_tbl.rds") # from qa_low_cost_sensors.r

test_tbl <- model_data_tbl$md_wide[2][[1]]

source("../airquality_GIT/gg_themes.R") # for nice gg themes

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

# use tidymodels to fit a linear model to the data
fit_lm <- function(model_data) {
    lm_model <- linear_reg() %>%
        set_engine('lm') %>% # adds lm implementation of linear regression
        set_mode('regression')
    
    lm_fit <- lm_model %>%
        fit(low_cost ~ reference, data = model_data)
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
            summarise(concentration = mean(concentration, na.rm = TRUE))
        title_interval = "daily"
    }
    
    plot_data %>% 
        ggplot(aes(x = date,
                   y = concentration,
                   colour = type)) +
        geom_line() +
        facet_wrap(~ glue("{site} {pollutant}"),
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


# pipeline to run model and add artefacts to an output tbl
model_output_tbl <- model_data_tbl %>%
    mutate(
        coefs = map(md_wide, ~ pluck(.x) %>%
                        fit_lm() %>%
                        tidy()),
        perf = map(md_wide, ~ pluck(.x) %>%
                       fit_lm() %>%
                       glance()),
        cor_test = map(md_wide,
                       ~ cor_test(.x, "reference", "low_cost") %>%
                           as_tibble()),
        plot = map(
            md_wide,
            ~ plot_scatter(.) +
                labs(title = quickText(
                    glue("Scatter plot of {get_title(siteid)} (ugm-3)")
                ),
                subtitle = "Reference Instrument (BAM 1020) vs. Low Cost Sensor (SDS011)")
        )
    )

model_output_tbl$plot[2]

# Time Plot ----

time_plot_data <- prep_timeplot(model_data_tbl)

plot_time_series(time_plot_data, interval = "day")
