#pacman::p_load(tidyverse, openair, fastverse, janitor, glue, lubridate)

# testing whether should use month in the lm
pacman::p_load(char = packages)
model_data_tbl <- readRDS("data/model_data_tbl.rds")

make.daily.site.tbl <- function(model_data_tbl, siteid = 215){

daily_site_tbl <- model_data_tbl %>%
    ungroup() %>% 
    filter(siteid == {{siteid}}) %>% 
    select(md_wide) %>% 
    pluck(1, 1) %>% 
    group_by(date = as.Date(date)) %>% 
    summarise(across(everything(),
                     \(x) mean(x, na.rm = TRUE)))

return(daily_site_tbl)
}
    
make.lm.monthly.tbl <- function(daily_site_tbl){
# nest by month and run lm for each month's data
lm_monthly_tbl <- daily_site_tbl %>% 
    na.omit() %>% 
    mutate(month = lubridate::month(date)) %>% 
    nest(data = -month) %>% 
    mutate(
        fit = map(data, ~lm(reference ~ low_cost + humidity + temperature, data = .x)),
        # fit = map(data, ~lm(reference ~ low_cost, data = .x)),
        tidied = map(fit, tidy),
        glanced = map(fit, glance),
        augmented = map(fit, augment)
    )

return(lm_monthly_tbl)
}


plot.fitted.lm <- function(lm_monthly_tbl){

fitted_lm_plot <- lm_monthly_tbl %>% 
    unnest(c(augmented, data), names_sep = "_") %>% 
    select(month,
           date = data_date, 
           low_cost = data_low_cost,
           reference = data_reference,
           fitted = augmented_.fitted) %>% 
    pivot_longer(cols = low_cost:fitted,
                 names_to = "variable",
                 values_to = "value") %>% #view()
    ggplot() +
    geom_line(aes(x = date, y = value, colour = variable), linewidth = 1)

return(fitted_lm_plot)    

}


daily_site_tbl <- make.daily.site.tbl(model_data_tbl = model_data_tbl, siteid = 215)

lm_monthly_tbl <- make.lm.monthly.tbl(daily_site_tbl)

fitted_lm_plot <- plot.fitted.lm(lm_monthly_tbl = lm_monthly_tbl)

fitted_lm_plot %>% 
    plotly::ggplotly()


print(fitted_lm_plot)
print(lm_plot_lc_ref_only)
# the one that includes temp and humidity seems better

lm_plot_lc_ref_only <- fitted_lm_plot


daily_site_tbl %>% 
   mutate(across(temperature:low_cost, \(x) x / max(x, na.rm = TRUE))) %>% 
    pivot_longer(cols = - date) %>% 
    ggplot() +
    geom_line(aes(x = date, y = value, color = name))
    










fitted_lm_plot

lm_monthly_tbl %>% 
    unnest(tidied)

lm_monthly_tbl %>% 
    unnest(glanced)


lm_fit_met <- lm(reference ~ low_cost + temperature + humidity, data = daily_site_tbl)

tidy(lm_fit_met)
glance(lm_fit_met)    
augment(lm_fit_met)


mutate(fit = list(),
        tidied = tidy(pluck(fit, 1)),
        glanced = glance(fit),
        augmented = augment(fit)
    )
    


lms <- test_tbl %>% 
    na.omit() %>%
    nest_by(month = lubridate::month(date)) %>% 
   mutate(lm = list(lm(low_cost ~ reference, data = data)))

lms %>% 
    reframe(tidy(lm))

lms %>% 
    summarise(glance(lm))

lms %>% 
    reframe(augment(lm))