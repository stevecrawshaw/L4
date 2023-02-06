pacman::p_load(tidyverse, jsonlite, httr2,
               janitor, glue, lubridate, config, rlist, plotly)

# use the teltonika API to extract useful info about the routers

dateon <-  "2022-12-01"
dateoff <-  "2022-12-31"
device_url <- "https://rms.teltonika-networks.com/api/devices/"

get.router.pat <- function(){
    
    get(config = "teltonika")$pat %>% 
        return()
}

get.device.data <- function(device_url){
    
    req <- request(device_url)
    
    response <- req %>% 
        req_headers(Accept = "application/json") %>% 
        req_auth_bearer_token(pat) %>% 
        req_perform()
    
    content <- response %>% 
        resp_body_json() %>% 
        pluck("data") 
    return(content)
}

make.device.id.tbl <- function(device_data){
    #get the ids - strangely not available from the csv endpoint
    ids <- map_int(device_data, pluck("id")) %>% 
        as.character()
    names <- map_chr(device_data, pluck("name"))
    # for use joining later in plot
    device_id_tbl <- tibble("id" = ids, "name" = names)
    return(device_id_tbl)
}

make.device.wide.tbl <- function(device_data){
    
    device_wide_tbl <- map_dfr(device_data, .f = ~unlist(.x) %>%
                                   bind_rows()) %>% 
        type_convert()
    
    return(device_wide_tbl)
}

make.sim.data.tbl <- function(device_wide_tbl){
    sim_data_tbl <- device_wide_tbl %>% 
        select(mobile_ip, iccid, operator, name) %>% 
        filter(!is.na(iccid))
    return(sim_data_tbl)
}

get.devices.details.tbl <- function(device_url){
    req_csv <- request(device_url) %>% 
        req_url_path_append("export") %>% 
        req_url_path_append("csv")
    
    response_devices <- req_csv %>% 
        req_headers(Accept = "text/csv") %>% 
        req_auth_bearer_token(pat) %>% 
        req_url_query(
            # fields separate with commas no spaces
            fields = "id,model,name,serial,mac,wlan_mac,mqtt,last_connection_at,updated_at,status,temperature,signal,cell_id,connection_uptime,iccid,operator,router_uptime,wan_ip"
        ) %>% 
        req_perform()
    
    devices_details_tbl <- response_devices %>%  
        resp_body_string() %>% 
        read_csv() %>% 
        janitor::clean_names() %>% 
        return()
}
# Data usage per device
get.data.use <- function(dateon, dateoff, device_url, id) {
    req <- request(device_url)
    start_date <- paste0(dateon, " 00:00:00")
    end_date <- paste0(dateoff, " 23:59:59")
    
    response_data_usage <- req %>%
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

get.data.use.partial <- partial(.f = get.data.use,
                                dateon = dateon,
                                dateoff = dateoff,
                                device_url = device_url)

# get data for all ids with vecorised partial function

make.daily.data.tbl <- function(data_use_tbl, device_id_tbl){
    daily_data_tbl <- data_use_tbl %>% 
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
        inner_join(device_id_tbl, by = "id")
    
    return(daily_data_tbl)
}

make.datelabel <- function(dateon, dateoff){
    
    if(var(c(month(dateon), month(dateoff))) == 0){
        datelabel = glue("{month(dateon, label = TRUE, abbr = FALSE)} {year(dateon)}")
    } else {
        datelabel = glue("{dateon} to {dateoff}")
    }
    return(datelabel)
    
}

make.daily.data.use.plot <- function(daily_data_tbl){
daily_data_tbl %>%
    filter(name != "RUT950_Spare_000") %>%
    ggplot(aes(x = date, y = MB, fill = r_t)) +
    geom_col() +
    facet_wrap(~ name, ncol = 2) +
    labs(title = "Mobile telemetry data use by routers",
         subtitle = "Daily Totals",
         fill = "Receive \nTransmit",
         x = "Date") %>% 
        return()
    
}

make.cumulative.tbl <- function(daily_data_tbl){
daily_data_tbl %>% 
    mutate(site = str_sub(name, 8, -5) %>% str_replace_all("_", " ")) %>% 
    filter(site != "Spare") %>% 
    group_by(site) %>% 
    mutate(cum_data = cumsum(MB)) %>% 
    
        return()
}

make.cumulative.plot <- function(cumulative_tbl){
cumulative_tbl %>% 
    ggplot() +
    geom_line(aes(x = date, y = cum_data, colour = site), linewidth = 1) +
    labs(x = "Date", y = "MB", 
         title = glue("Cumulative data use for period: {datelabel}"),
         caption = "Airport and BTW are low because no datalogger present\n
                    Use rises at the end of the month due to updates",
         colour = "Site") +
    theme_minimal() 
    }

make.data.summary.tbl <- function(daily_data_tbl, device_id_tbl, datelabel){

    total_data_period <- daily_data_tbl %>% 
        summarise(total_data_period = sum(MB, na.rm = TRUE)) %>% 
        pull() %>% 
        `/`(1024)
    
        
        num_sites <- device_id_tbl %>% 
        filter(!str_detect(name, "Spare")) %>% 
        nrow()
        
        daily_allowance_site <-  (3 * 1.024) / 30
    
        if(str_match(datelabel, pattern = "\\w+")[1] %in% month.name){
            allowance <-  daily_allowance_site * 30 * num_sites
        } else {
            allowance <- 
            str_split(datelabel, pattern = " to ")[[1]] %>%
                as.character() %>%
                as.Date() %>%
                diff() %>% 
                as.integer() %>% 
                `*`(daily_allowance_site * num_sites)
        }
        
    headroom_gb <- round((allowance - total_data_period) / 1.024, 1)
    
    headroom_pc <- round((allowance - total_data_period) * 100 / allowance, 1)
    
    data_summary_tbl <- tribble(
        ~ "Period",
        ~ "Total data used (GB)",
        ~ "Allowance (GB)",
        ~ "Headroom (GB)",
        ~ "Headroom (%)",
        datelabel,
        total_data_period,
        allowance,
        headroom_gb,
        headroom_pc
    )
    
    return(data_summary_tbl)

}



    #-------------TESTING ------------------
    
    pat <- get.router.pat()
    device_data <- get.device.data(device_url = device_url)
    device_id_tbl <- make.device.id.tbl(device_data = device_data)
    device_wide_tbl <- make.device.wide.tbl(device_data = device_data)
    sim_data_tbl <- make.sim.data.tbl(device_wide_tbl = device_wide_tbl)
    devices_details_tbl <- get.devices.details.tbl(device_url = device_url)

        data_use_tbl <- map_dfr(device_id_tbl$id,
                        .f = ~get.data.use.partial(.x))
        
    daily_data_tbl <- make.daily.data.tbl(data_use_tbl = data_use_tbl,
                                          device_id_tbl = device_id_tbl)
    datelabel = make.datelabel(dateon, dateoff)
    daily_use_plot <- make.daily.data.use.plot(daily_data_tbl = daily_data_tbl)
    cumulative_tbl <- make.cumulative.tbl(daily_data_tbl = daily_data_tbl)
        
        
    cumulative_plot <- make.cumulative.plot(cumulative_tbl = cumulative_tbl)

    plotly::ggplotly(cumulative_plot)
    data_summary_tbl <- make.data.summary.tbl(daily_data_tbl = daily_data_tbl,
                                          device_id_tbl = device_id_tbl,
                                          datelabel = datelabel)