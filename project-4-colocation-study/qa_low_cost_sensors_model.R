
# Libraries, Variables, Sourced Functions ----
p <-
    c(
        "tidyverse",
        "lubridate",
        "glue",
        "janitor",
        "fs",
        "collapse",
        "tidymodels",
        "ggside",        # side plots of density
        "ggpubr",        # easy labelling of equations on the plot
        "openair",
        "easystats",
        "padr"
    )
library(pacman)
p_load(char = p)
rm(p)

model_data_tbl <-
    read_rds("data/model_data_tbl.rds") # from qa_low_cost_sensors.r

source("../../airquality_GIT/gg_themes.R") # for nice gg themes

# Dashboard Function ----
# need to install from install.packages("easystats", repos = "https://easystats.r-universe.dev")

# pipeline to run model and add artefacts to an output tbl

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
                ~ plot_scatter(.) +
                    labs(title = quickText(
                        glue("Scatter plot of {get_title(siteid)} (ugm-3)")
                    ),
                    subtitle = "Reference Instrument (BAM 1020) vs. Low Cost Sensor (SDS011)")
            ),
            pollutant = if_else(siteid == 215L, "PM2.5", "PM10")
        )
    return(model_output_tbl)
}

model_output_tbl <- make.model.output.tbl(model_data_tbl)

make_dashboard <- function(model_output_tbl){
    # pre fill the model_dashboard function
    dash <- partial(.f = model_dashboard,
                    parameters_args = NULL,
                    performance_args = NULL,
                    output_dir = here::here(),
                    rmd_dir = system.file("templates/easydashboard.Rmd", package = "easystats")
    )
    
    # iteratively generate the dashboards from the table of model objects
    model_output_tbl %>% 
        ungroup() %>% 
        transmute(model = model_obj,
                  output_file = glue("{siteid}_{pollutant}_dashboard.html")
        ) %>% 
        pwalk(dash)
    
}


model_output_tbl %>% 
    select(siteid, plot) %>% 
    pwalk(.f = save.plot)

# Dashboards ----

make_dashboard(model_output_tbl)


