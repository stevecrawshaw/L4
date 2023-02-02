library(pacman)
p_load(tidyverse,
       gt,
       gtExtras,
       glue,
       lubridate,
       config,
       here,
       googlesheets4,
       janitor)

start_date <- "2022-01-01"
end_date <- "2022-12-31"

# Functions ----

config <- config::get(file = here::here("config.yml"),
                          config = "google_cal")

get.cal.tbl <- function(config){

        cal_raw_tbl <- read_sheet(config$cal_sheet_path,
                              sheet = "Workings",
                              col_types = "______ddccdddd--")
        
        out_tbl <- cal_raw_tbl %>%
            rename_with(tolower) %>% 
            clean_names() %>% 
            mutate(date = as.Date(date, format = "%d/%m/%Y")) %>% 
            return()
}

get.gas.tbl <- function(config){
    gas_raw_tbl <- read_sheet(config$cal_sheet_path,
                              sheet = "Workings",
                              col_types = "cii", range = "A1:C31")
    
    gas_tbl <- gas_raw_tbl %>%
        rename_with(tolower) %>% 
        clean_names() %>% 
        na.omit()
    return(gas_tbl)
}

get.responses.tbl <- function(config){

responses_raw_tbl <- read_sheet(config$cal_sheet_path,
                                sheet = "Form responses 1",
                                col_types = "cc_cccdddddddddddddddddd_cd_")

goodname <- function(string){
    
    poll <- string    %>% 
        str_extract("no\\d|nox|no")
    
    time <- string %>% 
        str_extract("\\d..")
    
    cal <- string %>%
        str_extract("zero|span")
    
    glue("{poll}_{time}_{cal}") %>% 
        return()
    
}

responses_tbl <- responses_raw_tbl %>% 
    rename_with(tolower) %>% 
    clean_names() %>%
    rename(cylinder_number = enter_the_cylinder_number,
           warnings = note_any_warnings_or_problems_on_site) %>% 
    rename_with(.fn = goodname, .cols = starts_with("enter")) %>% 
    mutate(date = as.POSIXct(timestamp,
                             format = "%d/%m/%Y %H:%M:%S")
           ) %>% 
    select(date,
           site,
           cylinder_number,
           everything()) %>% 
    select(!c(timestamp,
              starts_with("select"),
              starts_with("no_off"))) %>% 
    pivot_longer(cols = starts_with("no")) %>% 
    separate(name, into = c("pollutant", "order", "cal"))

return(responses_tbl)
}

make.cal.plot.tbl <- function(cal_tbl, start_date, end_date){

cal_plot_tbl <- cal_tbl %>% 
    filter(between(date, as.Date(start_date), as.Date(end_date))) %>% 
    rename(NOx_span = noxvs,
           NO_span = novs,
           NOx_zero = noxvz_nox_zero,
           NOx_sensitivity = nox_sensitivity_fnox,
           NO_zero = novz_no_zero,
           NO_sensitivity = no_sensitivity_fno) %>%
    mutate(NOx_NO_span_delta = (NOx_span - NO_span) * 100 / NOx_span) %>% 
    pivot_longer(cols = starts_with("NO"),
                 names_to = "calibration",
                 values_to = "value")
return(cal_plot_tbl)
}

make.date.list <- function(start_date, end_date){
days <- (as.Date(end_date) - as.Date(start_date)) %>% as.integer()

if (days > 100){
    db <- "3 months"
    dl <- "%b"
} else if (days > 31) {
    db <- "7 days"
    dl <- "%e"
} else {
    db <- "3 days"
    dl <- "%e"
}

end_month <- strftime(end_date, "%B")
start_month <- strftime(start_date, "%B")

date_breaks_labels_list <- list(
    "breaks" = db,
    "labels" = dl,
    "start_date" = start_date,
    "end_date" = end_date,
    "start_month" = start_month,
    "end_month" = end_month,
    "days" = days)

return(date_breaks_labels_list)
}

plot.span.diff <- function(cal_plot_tbl, date_list){

span_diff_plot <- cal_plot_tbl %>% 
    filter(calibration == "NOx_NO_span_delta") %>% 
    ggplot(aes(x = date, y = value)) +
    geom_line() +
    geom_hline(yintercept = c(10, -10),
               colour = "firebrick",
               lty = 5) +
    scale_x_date(date_breaks = date_list$breaks,
                 date_labels = date_list$labels) +
    facet_wrap(~site) +
    labs(title = "NOx and NO Span Value Divergence",
         subtitle = glue("Between {date_list$start_date} and {date_list$end_date}"),
         y = "% difference",
         x = "Date",
         caption = "Values > 10% indicate possible cylinder oxidation")

return(span_diff_plot)

}

make.cal.factor.gt <- function(cal_plot_tbl, date_list){

cal_factor_gt <- cal_plot_tbl %>% 
    filter(str_detect(calibration, "sensitivity|zero")) %>% 
    separate(col = calibration,
             into = c("pollutant",
                      "point"),
             remove = TRUE) %>%
    mutate(target = if_else(point == "zero", 0.05, 1),
           val = round(value, 2),
           point = str_to_title(point)) %>%
    group_by(site, date) %>% 
    gt(rowname_col = "date", groupname_col = "site") %>% 
    tab_options(row_group.as_column = TRUE,
                column_labels.font.weight = "bold"
                ) %>% 
    fmt_number(columns = value, decimals = 2) %>% 
    cols_label(pollutant = "Pollutant",
               point = "Factor",
               value = "Value",
               val = "Target") %>%
    gt_plt_bullet(column = val, target = target) %>% 
    tab_header(
        title = "NOx Calibration Factors Summary",
        subtitle = glue("{date_list$start_date} to {date_list$end_date}")) %>% 
    tab_style(style = list(cell_text(weight = "bold")),
              locations = cells_row_groups())
    
    return(cal_factor_gt)
}

# TESTING ----


responses_tbl <- get.responses.tbl(config)
    
cal_tbl <- get.cal.tbl(config)  

cal_plot_tbl <- make.cal.plot.tbl(cal_tbl, start_date, end_date)

date_list <- make.date.list(start_date, end_date)

span_diff_plot <- plot.span.diff(cal_plot_tbl, date_list)

cal_factor_gt <- make.cal.factor.gt(cal_plot_tbl, date_list)

cal_factor_gt
