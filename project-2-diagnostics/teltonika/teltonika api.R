libs <- c("httr2", "jsonlite", "tidyverse", "rlist", "fastverse", "janitor", "lubridate")
library("xfun")
pkg_attach2(libs)

# use the teltonika API to extract useful info about the routers

pat <- "eyJ0eXAiOiJKV1QiLCJhbGciOiJSUzI1NiJ9.eyJqdGkiOiI4MmNhNjk5ZTI5ZTM0ZDU2NGM4OGQzZjkzMjBmNThhZjMxNGNlMmRjODkzYzQxOTJjN2JlZmI3ZjU0MWFjY2UzMjM2NjJkOGU0NGIzMjM2ZSIsImlzcyI6Imh0dHBzOlwvXC9ybXMudGVsdG9uaWthLW5ldHdvcmtzLmNvbVwvYWNjb3VudCIsImlhdCI6MTY0MDI3ODc0NiwibmJmIjoxNjQwMjc4NzQ2LCJzdWIiOiI0ODIzMSIsImNsaWVudF9pZCI6IjkxMjNlYTY2LTJmMWQtNDM5Yy1iMWMyLTMxMWFjMDEwYWFhZCIsImZpcnN0X3BhcnR5IjpmYWxzZX0.HO44S5joUevYFvCrRBcMHtZmUMdJ9Jx1D8DgRwO13vq2vWU_aWDJjl4TJx1-KXI0OcrsOKI1Qx1Oq0KXTc7ujtPnfdqZ9Zw-bEpPGHx-Qqo-mfFvx58WRHvLPXf_7zjE3LVP2L9tzrKhky8ZcaDGqZNA0bSHYxHUnbbODYtsYfJX3N9eeQZUdZwMrFU9eUpSh49nHvRhFRt1ejXTd5uxF6w6Ads9OAz9eQTMZwLaviSc6e2JoRrNswg0py8pqmfvjPia_rloNig58PItsz8Q6na3kgum1G_bCwjXGR93Cp8oJ-4X0DQjbSUbgJtrin_ZG-jn_c-3nv3Y5BaFgfGTjg"

# Device info from json  - long winded ----

url950 <- "https://rms.teltonika-networks.com/api/devices?limit=10&status=online&model=RUT950"
urlall <- "https://rms.teltonika-networks.com/api/devices?limit=10"
shorturl <- "https://rms.teltonika-networks.com/api/devices/"

req <- request(shorturl)

response <- req %>% 
    req_headers(Accept = "application/json") %>% 
    req_auth_bearer_token(pat) %>% 
    req_perform()

content <- response %>% 
    resp_body_json() %>% 
    pluck("data") 

#get the ids - strangely not available from the csv endpoint
ids <- map_chr(content, pluck("id"))
names <- map_chr(content, pluck("name"))
# for use joining later in plot
name_id_tbl <- tibble("id" = ids, "name" = names)

rdt <- map_dfr(content, .f = ~unlist(.x) %>%
             bind_rows()) %>% 
    type_convert()

sim_table_lister <- rdt %>% 
    select(mobile_ip, iccid, operator, name) %>% 
    filter(!is.na(iccid))



fs::dir_create(path = "data")
sim_table_lister %>% 
    write_csv("data/sim_table_lister.csv")

# Device info as csv ----

csv_url <- "https://rms.teltonika-networks.com/api/devices/export/csv"
# returns incorrect iccid's
req_csv <- request(csv_url)

response_devices <- req_csv %>% 
    req_headers(Accept = "text/csv") %>% 
    req_auth_bearer_token(pat) %>% 
    req_url_query(
        # fields separate with commas no spaces
        fields = "id,model,name,serial,mac,wlan_mac,mqtt,last_connection_at,updated_at,status,temperature,signal,cell_id,connection_uptime,iccid,operator,router_uptime,wan_ip"
        ) %>% 
    req_perform()

devices_tbl <- response_devices %>%  
    resp_body_string() %>% 
    read_csv() %>% 
    janitor::clean_names()

devices_tbl %>% 
    pull(iccid) %>% 
    as.character()

# Data usage per device

req_data_usage <- request(shorturl)

# id <- "401760"
# end_date <- Sys.time() %>% as.character()
# start_date <- (Sys.time() - lubridate::ddays(1)) %>% as.character()

start_date <- "2021-12-11"
    # (Sys.Date() - 2) %>% as.character()
end_date <- (Sys.Date() -1 ) %>% as.character()

get_data_use_fnc <- function(start_date, end_date, id){

    start_date <- paste0(start_date, " 00:00:00")
    end_date <- paste0(end_date, " 23:59:59")
    
response_data_usage <- req_data_usage %>% 
    req_headers(Accept = "application/json") %>% 
    req_auth_bearer_token(pat) %>% 
    req_url_path_append(id) %>% 
    req_url_path_append("data-usage") %>% 
    req_url_query(start_date = start_date,
                  end_date = end_date) %>% 
    req_perform()
    # req_dry_run()


response_data_usage %>% 
    resp_body_json() %>% 
    pluck("data") %>% 
    list.rbind() %>% 
    as.data.frame() %>% 
    mutate(id = id) %>% 
    return()
}


# test datause <- get_data_use_fnc(start_date, end_date, id)

data_use_partial_fnc <- partial(.f = get_data_use_fnc,
                               start_date = start_date,
                               end_date = end_date)

# get data for all ids with vecorised partial function

datause_allids_tbl <- map_dfr(ids,
                              .f = ~data_use_partial_fnc(.x))
# plot data use per day ----
daily_data_tbl <- datause_allids_tbl %>% 
    mutate(across(.cols = everything(), .fns = ~as.character(.x))) %>% 
    select(- starts_with("sim2")) %>% 
    pivot_longer(cols = starts_with("sim1"),
                 names_to = "r_t",
                 values_to = "bytes") %>% 
    mutate(r_t = str_sub(r_t, start = 6, end = 7),
           MB = as.integer(bytes) / 1000000L,
           bytes = NULL,
           date = lubridate::ymd(date),
           id = as_factor(id)) %>% 
    inner_join(name_id_tbl, by = "id")

daily_data_tbl %>%
    filter(month(date) == 2,
           r_t == "rx") %>% 
    ggplot(aes(x = date, y = MB, fill = name)) +
    geom_col(position = "dodge") +
    # facet_wrap(~ name, ncol = 1) +
    labs(title = "Mobile telemetry data use by routers",
         subtitle = "Daily Totals",
         fill = "Router \ Site",
         x = "Date")

iccid_tbl <- devices_tbl    %>% 
    transmute(name, serial, iccid = iccid %>% as.character(), wan_ip)


iccid_tbl$iccid %>% unique()
