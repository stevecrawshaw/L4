# parse the OTJ learning data from google drive and make a nice table for upload
# to aptem
# https://googlesheets4.tidyverse.org/

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
# ignored by git for security
config <- config::get()
sheet_path <- config$sheet_path
learning_raw_tbl <- read_sheet(sheet_path, col_types = "TTccccc")

orig_names <- names(learning_raw_tbl)
new_names <- c("timestamp, date_of_learning_activity, hours, description_of_activity", "outcome_of_activity_roi, ksb, is_this_off_the_job_learning_or_in_your_own_time")


orig_list <- map(new_names, list)
names(orig_list) <- orig_names

learning_clean_tbl <- learning_raw_tbl %>%
    clean_names() %>% 
    mutate(hours = lubridate::hms(time_spent_on_activity_hh_mm),
           ksb = str_replace_all(select_ks_bs_that_apply,
                                        pattern = ", ",
                                        replacement = "<br>"))

learning_clean_tbl %>%
    select(timestamp, date_of_learning_activity, hours, description_of_activity,
           outcome_of_activity_roi, ksb, is_this_off_the_job_learning_or_in_your_own_time) %>% 
    gt() %>% 
    fmt_markdown(columns = ksb) %>% 
    gt::cols_label(.list = orig_list)



format(Sys.time(), format = "%T") %>% class()

lubridate::hms(Sys.time() %>% as.character())

?strftime()

