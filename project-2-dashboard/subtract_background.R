pacman::p_load(tidyverse,
               openair,
               glue,
               lubridate)


meta <- importMeta(all = TRUE, year = 2022)

back <- meta %>% 
    filter(site_type == "Rural Background",
           variable == "PM2.5")

#chilbolton as charlton mackrell most missing

bris_chil_tbl <- importAURN(site = c("BRS8", "CHBO"),
                            pollutant = "pm2.5",
                            year = 2019:2022)

bc_list <- bris_chil_tbl %>% 
    pivot_wider(id_cols = date,
                names_from = code,
                values_from = pm2.5) %>% 
    mutate(difference = if_else(BRS8 - CHBO < 0, 0, BRS8 - CHBO),
           year = year(date)) %>% 
    select(date, difference, year) %>% 
    split(.$year)

plot.tv.diff <- function(listel, yr){

    png(filename = glue("plots/tv_plot_pm2.5_difference_{yr}.png"),
        width = 1600,
        height = 1000,
        units = "px")
    timeVariation(listel, pollutant = 'difference',
                  main = glue("Time variation of differenced PM2.5 in {yr}"),
                  sub = "Bristol St. Pauls and Chilbolton",
                  name.pol = "Bristol - Chilbolton",
                  cols = "firebrick",
                  type = "season")
    dev.off()
    
}

bc_list %>% 
iwalk(plot.tv.diff) 
# iwalk iterates over the list and its index, so name of list item is passed to second arg - neat!
