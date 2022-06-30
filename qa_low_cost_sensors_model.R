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
        "ggpubr", # easy labelling of equations on the plot
        "openair",
        "easystats"
    )
library(xfun)
pkg_attach2(p)
rm(p)

model_data_tbl <-
    read_rds("data/model_data_tbl.rds") # from qa_low_cost_sensors.r

test_tbl <- model_data_tbl$md_wide[2][[1]]

source("../airquality_GIT/gg_themes.R") # for nice gg themes
# test_data <- model_data_tbl$md_wide[1][[1]]

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

get_title <- function(siteid){
    title <- case_when(
        siteid == 215 ~ ("PM2.5 at Parson Street"),
        siteid == 500 ~ ("PM10 at Temple Way"),
        TRUE ~ ""
    )
     return(title)
}

# pipeline to run model and add artefacts to an output tbl
model_output <- model_data_tbl %>%
    mutate(
        coefs = map(md_wide, ~ pluck(.x) %>%
                        fit_lm() %>%
                        tidy()),
        perf = map(md_wide, ~ pluck(.x) %>%
                       fit_lm() %>%
                       glance()),
        cor = cor(md_wide[[1]]$reference,
                  md_wide[[1]]$low_cost),
        plot = map(md_wide, ~ plot_scatter(.) +
                       labs(title = quickText(glue("Scatter plot of {get_title(siteid)} (ugm-3)")),
                            subtitle = "Reference Instrument (BAM 1020) vs. Low Cost Sensor (SDS011)"))
    )

model_output$plot[2]

par(mfrow = c(2, 2)) # plot all 4 plots in one

model_data_tbl$md_wide %>%
    walk(~ fit_lm(.) %>%
             plot_model())


test_tbl %>% 
    cor_test("reference", "low_cost") %>% 
    plot()

model_output$cor
report(test_tbl)



lm_fit <- fit_lm(test_tbl)

lm_fit %>% 
    parameters() %>% 
    plot()

# qqplot in see:: needs non parsnip model
lmt <- lm(low_cost ~ reference, data = test_tbl)

check_normality(lmt) %>% 
    plot(type = "qq")
