library(tidyverse)
# parse the ksbs from https://www.instituteforapprenticeships.org/apprenticeship-standards/data-analyst-v1-1#K1
ksb_parsed <- read_delim("data/rawksb", delim = ": ", col_names = c("KSB_ID", "description")) 

ksb_parsed %>% write_delim("data/ksb.csv", delim = ";")

# now parse the duties

duties_raw = read_delim("data/duties.txt", delim = "Duty", col_names = "id")

duties <- duties_raw %>% 
    filter(str_detect(id, "Duty")) %>% 
    rename(duty = id)

k <- duties_raw %>% 
    filter(!str_detect(id, "KSBS"),
        str_starts(id, "K"))%>% 
    rename(knowledge = id)

s <- duties_raw %>% 
    filter(str_starts(id, "S"))%>% 
    rename(skills = id)

b <- duties_raw %>% 
    filter(str_starts(id, "B")) %>% 
    rename(behaviours = id)

duties_ksb <- tibble(duties, k, s, b)

duties_ksb %>% 
    write_delim(file = "data/duties_ksb.csv", delim = ";")
