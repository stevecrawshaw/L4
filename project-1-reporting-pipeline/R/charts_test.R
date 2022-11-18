p <- c("tidyverse", "lubridate", "httr", "openair", "data.table", "here", "scales", "ggtext", "glue")
library("xfun")
pkg_attach2(p)
source("../../airquality_GIT/gg_themes.R")
year <- 2021

#area vectors-------------------

make.plotareas_tbl <- function(){

central <- tribble(~siteid, 2, 5, 9, 11, 15, 113, 125, 147, 423, 318) %>% 
    mutate(area = "Central")
glosrd <- tribble(~siteid, 21, 22, 157, 159, 161, 163) %>% 
    mutate(area = "Gloucester Road")
bathrd <- tribble(~siteid, 10, 403, 478) %>% 
    mutate(area = "Bath Road")
a37 <- tribble(~siteid, 4, 14, 413, 438) %>% 
    mutate(area = "A37")
parson <- tribble(~siteid, 239, 242, 418, 419) %>% 
    mutate(area = "Parson Street")
m32 <- tribble(~siteid, 20, 260, 261, 263, 373, 374, 441) %>% 
    mutate(area = "M32")

plotareas_tbl <- bind_rows(central, glosrd, bathrd, a37, parson, m32)
return(plotareas_tbl)
}

make.no2.trend.chart.tbl <- function(startDate,
                                     ods_tubes_upload_tbl,
                                     plotareas_tbl,
                                     aqms_tbl){
year = year(startDate)
xyear = year -8 #for location of the AQ objective annotation

tube_chart_data <- 
    ods_tubes_upload_tbl %>%  
    filter(siteid %in% plotareas_tbl$siteid) %>% 
    inner_join(plotareas_tbl, by = "siteid") %>% 
    inner_join(aqms_tbl %>% 
                   select(siteid, location), by = "siteid") %>% 
    mutate(location = str_wrap(location, width = 25)) %>%
    group_by(area) %>%
    nest() %>% #creaet a ggplot object for each area and nest into this DF
    mutate(plot = map(data,
                      ~ggplot(., aes(year,
                                     conc_ugm3,
                                     colour = location)) +
                          geom_line(linewidth = 1) +
                          labs(title = quickText(
                              paste0("NO2 trends at diffusion tube sites: ", area)
                              ),
                               y = quickText("NO2 ugm-3"),
                               x = "Year",
                               colour = "Location",
                               caption = "Bias adjusted and annualised, not distance adjusted") +
                          geom_hline(yintercept = 40,
                                     linewidth = 2,
                                     lty = 5,
                                     colour = "red") +
                          annotate("label",
                                   x = xyear,
                                   y = 42,
                                   label = "Air Quality Objective") +
                          scale_x_continuous(breaks = scales::breaks_pretty()) + 
                          theme_web_bw()),
           filename = glue("plots/{area}_{xyear}_to_{year}_no2_trend_.png"))
}

write.no2.trend.charts <- function(no2_trend_chart_tbl){
#plot the outputs to file
    no2_trend_chart_tbl %>%
    ungroup() %>% 
    select(plot, filename) %>% 
    pwalk(ggsave, width = 10, height = 7)
}
# PM2.5 chart

make.pm25.trend.chart <- function(startDate){

year_range <- seq.int(year(startDate) - 5, year(startDate))

pm_25_tbl <- importAURN(year = year_range,
                        data_type = "annual") %>% 
    filter(code == "BRS8") %>% 
    select(date, pm2.5) %>% 
    mutate(year = year(date))

pm25_chart <- pm_25_tbl %>%
    ggplot(aes(x = year, y = pm2.5)) +
    geom_line(lwd = 1) +
    geom_text(aes(label = round(pm2.5, 1)),
              vjust = 2) +
    geom_hline(yintercept = 5,
               color = "red",
               lty = 5,
               linewidth = 1,
               alpha = 0.5) +
    scale_y_continuous(breaks = scales::breaks_pretty()) +
    labs(x = "Year",
         y = quickText("ugm-3"),
         title = quickText("Trend in Annual Mean PM2.5 Bristol St. Pauls")) +
    annotate("label",
             x = min(pm_25_tbl$year) + 0.5,
             y = 5.6,
             label = "WHO guideline value") +
    expand_limits(y = 0) + 
    theme_web_bw() +
    theme(panel.border = element_blank())

return(pm25_chart)
}

write.pm25.trend.chart <- function(pm25_trend_chart){

yearvec <- pm25_trend_chart$data$year
yearmin <- min(yearvec)
yearmax = max(yearvec)
    
filename <- glue("plots/PM2.5_trend_{yearmin}_to_{yearmax}.png")
    
ggsave(pm25_trend_chart, filename = filename)

return(glue("chart image file saved to {filename}"))
}

plotareas_tbl <- make.plotareas_tbl()

no2_trend_chart_tbl <- make.no2.trend.chart.tbl(startDate,
                                                ods_tubes_upload_tbl,
                                                plotareas_tbl,
                                                aqms_tbl)

pm25_trend_chart <- make.pm25.trend.chart(startDate)

