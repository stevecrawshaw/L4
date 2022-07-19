# parse the OTJ learning data from google drive and make a nice table for upload
# to aptem
# https://googlesheets4.tidyverse.org/
# Libraries ----
p <- c("tidyverse",
       "gt",
       "glue",
       "lubridate",
       "config",
       "here",
       "googlesheets4",
       "janitor")

library("xfun")
pkg_attach2(p)
# Functions ----
get_googlesheet <- function(){
    # config.yml ignored by git for security
    config <- config::get()
    sheet_path <- config$sheet_path
    learning_raw_tbl <- read_sheet(sheet_path,
                                   col_types = "TTccccc")
    return(learning_raw_tbl)
}

# function to bold the KSB's 
ksb_bold <- function(ksb_text){
    patt_text <- str_extract_all(ksb_text,
                                 pattern = "K[0-9]+|B[0-9]+|S[0-9]+") %>% 
        unlist()
    repl_text <- paste0("**", patt_text, "**")
    # named vector to replace with markdown bolded text
    names(repl_text) <- patt_text
    str_replace_all(ksb_text,
                    repl_text) %>% 
        return()
}
# custom function to add hours for a month
sum_period_fnc <- function(period_vec){

        period_vec %>%
            period_to_seconds() %>%
            sum(na.rm = TRUE) %>% 
            seconds_to_period() %>% 
            as.character() %>% 
            return()    
}

# theme function for GT
basic_theme <- function(data, ...){
    data %>% 
        tab_options(
            table.background.color = "white",
            ...
        )
}

get_gt_table <- function(learning_date, OTJ_only = TRUE){
    stopifnot("Entered date must be YYYY-MM-DD" = ymd(learning_date) %>% is.Date())
    month_of_learning <- glue("{month.name[month(learning_date)]} {year(learning_date)}")    

learning_raw_tbl <- get_googlesheet()
# transform the raw data
learning_clean_tbl <- learning_raw_tbl %>%
    clean_names() %>% 
    select(-timestamp) %>% 
    filter(month(date_of_learning_activity) == month(learning_date)) %>% 
    mutate(hours = lubridate::hms(time_spent_on_activity_hh_mm), #period
           ksb = str_replace_all(select_ks_bs_that_apply, # make CR/LF
                                        pattern = ", ",
                                        replacement = "<br>")) %>% 
    mutate(ksb = map_chr(ksb, ksb_bold)) %>%  # highlight the KSB ID
           mutate(ksb = map_chr(ksb, ksb_bold)) # highlight the KSB ID

if(OTJ_only){
    learning_clean_tbl <- learning_clean_tbl %>% 
        filter(is_this_off_the_job_learning_or_in_your_own_time == "Off the job")
}

# create list for the labels in the GT table
orig_names <- names(learning_raw_tbl %>% select(-Timestamp))
new_names <- c("date_of_learning_activity", "hours", "description_of_activity", "outcome_of_activity_roi", "ksb", "is_this_off_the_job_learning_or_in_your_own_time")
orig_list <- map(orig_names, list)
names(orig_list) <- new_names

# create the gt table
month_gt <- learning_clean_tbl %>%
    select(all_of(new_names)) %>% 
    gt(groupname_col = "is_this_off_the_job_learning_or_in_your_own_time") %>% 
    
    fmt_markdown(columns = ksb) %>% 
    gt::cols_label(.list = orig_list) %>%
    tab_options(table.width = px(1080)) %>% 
    summary_rows(columns = hours,
                 groups = TRUE,
                 fns = list(`Total time`  = "sum_period_fnc"),
                 formatter = fmt_passthrough, # this formatter used for 
                 pattern = "{x}") %>% #character output where function makes char
    tab_options(summary_row.border.color = "black",
                row_group.font.weight = "bold") %>% 
    cols_width(
        date_of_learning_activity ~ pct(8),
               hours ~ pct(10),
               description_of_activity ~ pct(15),
               outcome_of_activity_roi ~ pct(15),
               ksb ~ pct(45),
               is_this_off_the_job_learning_or_in_your_own_time ~ pct(10)) %>% 
    
    tab_header(title = "Learning activity record: Steve Crawshaw",
               subtitle = month_of_learning) %>% 
    basic_theme(
        column_labels.font.size = px(15),
        column_labels.font.weight = "bold"
    )
return(month_gt)
}

learning_date <- "2022-06-01"

get_gt_table(learning_date, OTJ_only = FALSE) %>% 
    gtsave(filename = glue("learning_record_table_{learning_date}.png"),
           vwidth = 1920)
