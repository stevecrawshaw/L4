library(tidyverse)


tubes_orig_tbl <- read_csv("data/asr_tubes_2019.csv")

annualised <- tibble::tribble(
                   ~siteid,
                   14L,
                  314L,
                  461L,
                  557L,
                  560L,
                  561L,
                  562L,
                  563L,
                  564L,
                  565L,
                  567L,
                  568L,
                  569L,
                  570L,
                  571L
                  )

a_tbl <- annualised %>% 
    mutate(annual = 1)


tubes_19_append <- tubes_orig_tbl %>% 
    left_join(a_tbl) %>% 
    transmute(LocID = siteid,
              dYear = 2019L,
              concentration = raw,
              BAFconc = raw * 0.82,
              annualised_ba_conc_ugm3 = if_else(annual == 1, baf_annual, BAFconc),
              final_adjusted_conc = baf_annual,
              distance_corrected_conc = if_else(distance == final_adjusted_conc,
                                                NA_real_,
                                                distance))
tubes_19_append %>% write_csv("data/2019_append.csv")
    