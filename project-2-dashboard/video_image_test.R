
libs <- c("fastverse", "tidyverse", "openair")
library(xfun)
pkg_attach2(libs)
source("../airquality_GIT/importODS.R")

# 2.0 Data Loading

datafilepath <- "data/vidtest_data.rds"

if(file.exists(datafilepath)){
  aq_data <- read_rds(datafilepath)
} else{
  aq_data <- getODSExport(select_str = "siteid, date_time, location, no2, nox, no, pm25, o3, pm10",
                          date_col = "date_time",
                          dateon = "2021-01-01",
                          dateoff = "2021-12-31 23:59:59",
                          refine = "current:True",
                          where_str = "siteid = 452",
                          order_by = "siteid, date_time", #necessary for rolling mean calc
                          dataset = "air-quality-data-continuous")
  write_rds(aq_data, datafilepath)
  
}

plot_dot <- function(data){
  dt <- as.Date(data$date_time)[1] %>% as.character()
ggplot(data, 
       mapping = aes(x = date_time, y = no2)
       ) +
  geom_point(colour = "red") +
    labs(title = dt) +
  expand_limits(x = c(as.POSIXct("2021-01-01 00:00:00"),
                      as.POSIXct("2021-12-31 23:59:59")),
                y = c(0, 91))
}

plot_dot(aq_data)

max(aq_data$nox, na.rm = T)

openair_plot <- function(data){
  data %>% 
    rename(date = date_time) %>% 
  openair::timePlot(pollutant = "nox",
                    ylim = c(0, 540))
  
}

vid_tbl <- aq_data %>% 
  nest_by(date = as.Date(date_time)) %>% 
  # summarise(no2 = mean(no2, na.rm = TRUE))
  mutate(plot = list(
plot_dot(data = data)
  ),
openair_plot = list(openair_plot(data = data))
) %>% 
  ungroup() %>% 
  mutate(filename = glue("images/plot_{1:nrow(.)}.png"))

#ggplot

vid_tbl %>% 
  select(filename, plot) %>% 
  pwalk(.f = ggsave)

# openair

# function to save as png

png_fnc <- function(filename, openair_plot){
  png(filename = filename)
  openair_plot %>% 
    pluck("plot") %>% 
    plot() # must explicitly call plot
  dev.off()
}

vid_tbl %>% 
  select(filename, openair_plot) %>% 
pwalk(.f = png_fnc)



# https://www.thewindowsclub.com/how-to-create-a-video-from-image-sequence-in-windows
# put plot files in C:\Users\User\Documents\ffmpeg-2022-02-24-git-8ef03c2ff1-full_build\bin
# run from cmd
# C:\Users\User\Documents\ffmpeg-2022-02-24-git-8ef03c2ff1-full_build\bin>ffmpeg -r 60 -i plot_%d.png output.mp4

# change delay https://stackoverflow.com/questions/7015489/building-video-from-images-with-ffmpeg-w-pause-between-frames


