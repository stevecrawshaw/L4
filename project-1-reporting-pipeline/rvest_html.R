
library(pacman)
p <-
    c("rvest",
      "tidyverse",
      "janitor",
      "glue",
      "lubridate")
p_load(char = p)


get.cal.content <- function(){
url <- "https://laqm.defra.gov.uk/air-quality/air-quality-assessment/diffusion-tube-monitoring-calendar/"

return(read_html(url))
}

get.html.tables <- function(content){
    # return a named list of the html tables holding the DT calendar dates
    get.cal.tbl <- function(tbl){
        if(dim(tbl)[1] == 12){
            return(tbl)
        } else {
            return(NULL)
        }   
    }
    
    tables <- content %>% 
        html_table(fill = TRUE)
    
    table_tbls <- map(tables, .f = ~get.cal.tbl(.x))
    table_list <- table_tbls[!sapply(table_tbls, is.null)]
    
    table_years_names <- html_elements(content, "caption") %>% 
        html_text() %>% 
        str_extract("[0-9]{1,4}") %>% 
        paste0("tbl_", .)
    
    names(table_list) <- table_years_names
    
    return(table_list)
    
}


get.calendar.dates <- function(table_list, year, start_end = "start"){
    # parse the dates in the calendar table for the year
    # return a named vector of start and end dates for the year
    
table_name <- paste0("tbl_", year)

p.date <- function(string, year, end = FALSE){
    string <- glue("{string} {year}")
    as.Date(string, format = "%e %B %Y") %>% 
        return()
}

dates_table <- table_list[names(table_list) == table_name][[1]] %>% 
    clean_names() %>% 
    mutate(sd = p.date(start_date, {{year}}),
           ed = if_else(month(as.Date(end_date, format = "%e %B")) == 1,
                        p.date(end_date, ({{year}} + 1)),
                        p.date(end_date, {{year}})),
           mth = month(sd + ((ed - sd) / 2)))

cal_start_date <- dates_table$sd[dates_table$mth == 1]
cal_end_date <- dates_table$ed[dates_table$mth == 12]

calendar_dates <- c(cal_start_date, cal_end_date) 
names(calendar_dates) <- c("cal_start", "cal_end")

if (start_end == "start"){
    return(calendar_dates[1])
} else if (start_end == "end") {
    return(calendar_dates[2])
}


}

#testing
# content <- get.cal.content()
# table_list <- get.html.tables(content)
# cal_end <- get.calendar.dates(table_list, 2023, start_end = "end")
# cal_end
