

site_metadata_1 <- aqm %>% 
    transmute(
        siteid,
        location,
        la = "Bristol City Council",
        laqm_locationclass,
        locationclass,
        area_type = "Urban",
        monitoring_technique = "Passive sampling",
        year_of_measurement = 2020,
        annual_mean_no2_concentration_mg_m3 = NA,
        geo_point_2d,
        tube_kerb_distance_m,
        sampling_time_unit = "Month",
        sample_height,
        inlet_positioned_away_from_emission_sources = "y",
        co_located_with_continuous_analyser_y_n = if_else(!is.na(colocated),
                                                          "y",
                                                          "n")
    ) %>% 
    separate(geo_point_2d,
             into = c("measurement_site_latitude_decimal_degrees",
                      "measurement_site_longitude_decimal_degrees"),
             sep = ", ",
             convert = TRUE,
             remove = TRUE)

site_metadata_1 %>% 
    glimpse()
