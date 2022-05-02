library(rmarkdown)
library(tidyverse)
library(xfun)
library(fs)
library(glue)
library(here)
#library(reticulate)
#use_condaenv(condaenv = "ds01")

unzip_convert <- function(relpath){
# function to unzip and convert ipynb notebooks to Rmd
    respath = glue("{relpath}/")
    resources_path <- glue("{respath}{list.files(path = respath,
                                 pattern = '.zip')}")
    walk(resources_path, ~unzip(.x, exdir = respath))
    
    notebook_files <- glue('{respath}{list.files(path = respath, pattern = "ipynb")}')
    
    walk(notebook_files, ~convert_ipynb(.x))
    
    rmds <- glue("{respath}{list.files(path = respath, pattern = '.Rmd')}")
    
    file_move(rmds, new_path = "/home/steve/r-projects/L4/")

    pr <- read_rds("data/processed_resources.rds") # append to vector of already processed resources
    justfiles <- path_ext_remove(notebook_files) %>% 
        path_file()
    
    pr <- append(pr, justfiles)
    write_rds(pr, "data/processed_resources.rds")

}

unzip_convert("resources")

