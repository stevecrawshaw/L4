libs <- c("tidyverse", "fastverse", "here", "lubridate", "openair", "glue", "janitor", "magick", "fs", "stringr")
library(xfun)
pkg_attach2(libs)

path <- "images/orig/"
sts_sensors_vec <- 
  c("66963", 
    "66966",
    "66970",
    "66972",
    "66974",
    "66979",
    "66987",
    "67568",
    "67655",
    "67665")
fullpath <- glue("{path}{sts_sensors_vec}")

make_gif <- function(fullpath){
  files <- fs::dir_ls(path = fullpath, glob = "*.png")
  outpath <- "images/"
  sensor_id <- stringr::str_sub(fullpath, -6, -1)
  sensor_img <- image_read(files)
  
  sensor_img %>% 
    #image_morph() %>% 
    image_animate(optimize = TRUE, delay = 50) %>% 
    image_write(path = glue("{outpath}{sensor_id}.gif"))
}

walk(fullpath, .f = make_gif)
