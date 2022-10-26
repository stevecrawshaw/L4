library(pacman)
p <- c("odbc", "tidyverse", "dbplyr", "DBI", "glue", "here", "timetk", "config")
p_load(char = p)

Sys.setenv(TZ = "UTC")



con_params <- get()

#'make connection to Envista using details in the config file
con <- dbConnect(odbc::odbc(),
                 Driver = con_params$driver,
                 Server = con_params$server,
                 Database = con_params$database,
                 UID = con_params$uid,
                 PWD = con_params$pwd,
                 Port = con_params$port
)


diag_tbl <- tbl(con, "TB_DIAGNOSTICS") %>% 
    collect()

# diag_tbl %>% 
#     saveRDS(file = "../data/diag_tbl.rds")


con %>% dbDisconnect()
