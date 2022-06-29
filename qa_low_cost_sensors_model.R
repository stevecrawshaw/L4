p <-
    c("tidyverse",
      "lubridate",
      "glue",
      "janitor",
      "fs",
      "collapse",
      "tidymodels",
      "parsnip")
library(xfun)
pkg_attach2(p)
rm(p)

model_data_tbl <- read_rds("data/model_data_tbl.rds")

test_data <- model_data_tbl$md_wide[2][[1]]

test_data %>% 
    ggplot(aes(x = reference, y = low_cost)) +
    # geom_density_2d(lwd = 2, show.legend = TRUE) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm") +
    geom_xsidedensity() +
    geom_ysidedensity() +
    scale_ysidex_continuous(guide = guide_axis(angle = 90), breaks = NULL) +
    scale_xsidey_continuous(guide = guide_axis(angle = 90), breaks = NULL) +
    theme_minimal()

cor(test_data$reference, test_data$low_cost)



fit_lm <- function(model_data){
    lm_model <- linear_reg() %>% 
        set_engine('lm') %>% # adds lm implementation of linear regression
        set_mode('regression')
    
    lm_fit <- lm_model %>% 
        fit(low_cost ~ reference, data = model_data)
}

model_output <- model_data_tbl %>% 
    mutate(coefs = map_df(md_wide, ~pluck(.x) %>% 
                              fit_lm() %>% 
                              tidy()) %>%
                              list(),
           perf = map_df(md_wide, ~pluck(.x) %>% 
                             fit_lm() %>% 
                             glance()) %>% 
               list(),
           cor = cor(md_wide[[1]]$reference,
                     md_wide[[1]]$low_cost)
           )


par(mfrow=c(2,2)) # plot all 4 plots in one

plot(lm_fit$fit,
     pch = 16,    # optional parameters to make points blue
     col = '#006EA1')

