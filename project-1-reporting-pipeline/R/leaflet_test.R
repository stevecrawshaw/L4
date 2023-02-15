p <- c("tidyverse", "leaflet", "leafem", "sf")
library(pacman)
p_load(char = p)

aqms_map_tbl <-   aqms_tbl %>%
    filter(current) %>% 
    mutate(color = case_when(
        instrumenttype == "Diffusion Tube" ~ "blue",
        instrumenttype == "Continuous (Reference)" ~ "red")) %>% 
    st_as_sf(coords = c("easting", "northing"), crs = 27700) %>% 
    st_transform(crs = 4326) %>% 
    mutate(lat = st_coordinates(.)[,2],
           lon = st_coordinates(.)[,1])
    


aqma_boundary <- st_read("data/air-quality-management-areas.geojson")

aqgs <- read_file("data/air-quality-management-areas.geojson")

livenox <- aqms_tbl %>% 
    filter(str_detect(pollutants, "NOX"),
           current == 1) %>% 
    nrow()

glimpse(aqms_map_tbl)

leaflet(aqms_map_tbl) %>%
    setView(lng = -2.6, lat = 51.46, zoom = 12) %>%
    addTiles(urlTemplate = "http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png")  %>% 
    
    addCircleMarkers(~lon,
                     ~lat,
                     radius = 5,
                     opacity = 0.8,
                     fill = FALSE,
                     data = aqms_map_tbl %>%
                         filter(instrumenttype == "Diffusion Tube"),
                     popup = ~paste0(location,
                                     "<br/>",
                                     "Pollutants: ", pollutants,
                                     "<br/>",
                                     "Location class: ", laqm_locationclass),
                     label = ~ as.character(siteid),
                     color = ~ color) %>% 
    addCircleMarkers(~lon,
                     ~lat,
                     radius = 10,
                     opacity = 0.8,
                     fill = FALSE,
                     data = aqms_map_tbl %>%
                         filter(instrumenttype == "Continuous (Reference)"),
                     popup = ~paste0(location,
                                     "<br/>",
                                     "Pollutants: ", pollutants,
                                     "<br/>",
                                     "Location class: ", laqm_locationclass),
                     label = ~ as.character(siteid),
                     color = ~ color) %>% 
    addGeoJSON(aqgs, color = "purple", opacity = 0.8, fill = FALSE) %>% 
    addLegend(position = "bottomleft", colors = ~unique(color), labels = ~unique(instrumenttype))

library(maptiles)

bristiles <- get_tiles(aqma_boundary,
                       crop = TRUE,
                       provider = "Esri.WorldStreetMap")
plot_tiles(bristiles)

