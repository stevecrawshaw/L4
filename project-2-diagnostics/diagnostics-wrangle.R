packages <-
    c("tidyverse",
      "openair",
      "glue",
      "timetk",
      "lubridate")

pacman::p_load(char = packages)

#tidyverse::tidyverse_packages()

startDate = "2022-09-01"
endDate = "2022-09-30"

final_tbl <- read_delim(file = "S:/SUSTAIN/Sustain-Common/SCCCS/write_gis_r/R Projects/air_quality_data_management/data/finalContinTablerhtemp.csv", delim = ",", col_types = "ciiciccc")

siteids <- c(215, 270, 203, 463, 501, 672)

diag_tbl <- read_rds("../data/diag_tbl.rds")


samp_diag_tbl <- diag_tbl %>% 
    tail(1000)


# function to pivot longer the names, units and values from the 
# diagnostics table and combine into one dataframe
lengthen.diag <- function(diagnostics_tbl){
names_df <- diagnostics_tbl %>% 
    select(DIG_DateTime, DIG_Station, contains("Name")) %>% 
    pivot_longer(cols = contains("Name"),
                 names_to = "DIG_Name",
                 values_to = "parameter")

units_df <- diagnostics_tbl %>% 
    select(DIG_DateTime, DIG_Station, contains("Unit")) %>% 
    pivot_longer(cols = contains("Unit"),
                 names_to = "DIG_Unit",
                 values_to = "unit")

values_df <- diagnostics_tbl %>% 
    select(DIG_DateTime, DIG_Station, contains("Value")) %>% 
    pivot_longer(cols = contains("Value"),
                 names_to = "DIG_Value",
                 values_to = "value")

combined_tbl <- cbind(names_df %>% 
                      select(-DIG_Name),
                  units_df %>%
                      select(unit),
                  values_df %>% 
                      select(value)) %>% 
    as_tibble()
return(combined_tbl)
}

combined_tbl <- lengthen.diag(diag_tbl)

# clean it
combined_tbl %>% 
    na.omit() %>% 
    mutate(parameter = str_trim(parameter),
           unit = str_trim(unit))

