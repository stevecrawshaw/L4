pacman::p_load(tidyverse, openair, fastverse, janitor, glue, lubridate)
pacman::p_load(char = packages)
model_data_tbl <- readRDS("data/model_data_tbl.rds")

test_pm25_tbl <- model_data_tbl$md_wide[[2]]


lms <- test_pm25_tbl %>% 
    na_omit() %>%
    nest_by(month = lubridate::month(date)) %>% 
   mutate(lm = list(lm(low_cost ~ reference, data = data)))

lms %>% 
    summarise(glance(lm))

lms %>% 
    reframe(augment(lm)) %>% 
    

test_pm25_tbl %>% 
    group_by(month = month(date)) %>% 
    summarise(mean = mean(low_cost, na.rm = TRUE)) %>% 
    ggplot(aes(x = month, y = mean) )+
    geom_line()
