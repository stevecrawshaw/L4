# load packages
library("xfun")
p <- c("tidyverse", "docxtractr", "glue", "fs", "here", "officer", "janitor", "ggtext", "gt")
pkg_attach2(p)
#read file
file <- "data/Goals for Progress Reviews.docx"
#read doc for extracting objective names
doc <- officer::read_docx(file)

# objectives
# vector of the 4 objectives
objs <- docx_summary(doc) %>% as_tibble() %>% 
    filter(str_starts(text, "Objective [0-9]")) %>% 
    pull(text)
# read doc and extract tables
doc_table_list <- file %>% 
    docxtractr::read_docx() %>% 
    docx_extract_all_tbls(guess_header = FALSE)
# function to format tables nicely
make_gt <- . %>% 
    rename(SMART = V1,
           Description = V2,
           Notes = V3) %>%
    mutate(SMART = map(SMART, ~str_split(.x, pattern = " ")[[1]][2]),
           Notes = stringr::str_remove(Notes, "Notes:")) %>% 
    gt()
# create list of formatted gt tables
gt_list <- doc_table_list %>% 
    map(make_gt)