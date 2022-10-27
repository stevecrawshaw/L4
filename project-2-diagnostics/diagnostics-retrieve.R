library(pacman)
p <- c("odbc", "dplyr", "DBI", "config", "lubridate")
p_load(char = p)


connect.envista <- function(){
    con_params <- get() # credentials
    
    #make connection to Envista using details in the config file
    dbConnect(odbc(),
                     Driver = con_params$driver,
                     Server = con_params$server,
                     Database = con_params$database,
                     UID = con_params$uid,
                     PWD = con_params$pwd,
                     Port = con_params$port
    ) %>% 
        return()
    
}


get.diag.tbl <- function(con, dateon, dateoff){
# function to get the raw diagnostics table from the envista database
tbl(con, "TB_DIAGNOSTICS") %>% 
    filter(between(DIG_DateTime,
                   as_datetime(dateon),
                   as_datetime(dateoff))) %>% 
    collect() %>% 
        return()

}

