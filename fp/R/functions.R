pacman::p_load(odbc,
               DBI, 
               config,
               tidyverse,
               openair,
               glue,
               timetk,
               lubridate,
               gt,
               gtExtras,
               googlesheets4,
               janitor,
               rlist,
               plotly,
               httr2,
               naniar)

# 1.0 Global Variables ----

dateon <-  "2023-03-01"
dateoff <-  "2023-03-31"
testing <- FALSE
google_config <- config::get(file = "../config.yml",
                             config = "google_cal")

device_url <- "https://rms.teltonika-networks.com/api/devices/"

check.dates <- function(dateon, dateoff) {
    date_on <- as.Date(dateon)
    
    date_off <- as.Date(dateoff)
    
    stopifnot(
        "dateon is invalid" = is.Date(date_on),
        "dateoff is invalid" = is.Date(date_off),
        "dateoff is before dateon" = date_off > date_on,
        "Selected period exceeds one year" = as.integer(date_off - date_on) < 367,
        "Selected period is less than one month" = as.integer(date_off - date_on) > 27
        
    )
    return(TRUE)
    
}


get.final.tbl <- function(){
    read_delim(file = "S:/SUSTAIN/Sustain-Common/SCCCS/write_gis_r/R Projects/air_quality_data_management/data/finalContinTablerhtemp.csv", delim = ",", col_types = "ciiciccc") %>% 
        return()
}
make.limits.tbl <- function(){
    limits_tbl <- tribble(
        ~parameter, ~warning_low, ~warning_high, ~normal_low, ~normal_high,
        "Converter temp", 305, 325, 310, 320,
        "Internal box temp", 7, 48, 7, 40,
        "React cell temp", 45, 55, 49, 51,
        "Ozone flow rate", 50, 150, 80, 100,
        "PMT temp", 5, 12, 6, 8,
        "React cell pres", 4, 10, 5, 5.6,
        "React cell Pres", 4, 10, 5, 5.6,
        "Sample flow rate", 350, 600, 450, 550,
        "Sample pres", 28, 30, 15, 35,
        "Sample Pres", 28, 30, 15, 35
    )
    return(limits_tbl)
}
make.sites.tbl <- function(){
    
    siteid <- c(215L, 270L, 203L, 463L, 501L, 672L)
    site_name <- c("Parson St",
                   "Wells Road",
                   "Brislington",
                   "Fishponds",
                   "Colston Ave",
                   "Marlborough St")
    sites_tbl <- tibble(siteid, site_name)
}

# 2.0 Analyser Diagnostics ----

#   Database Connection
connect.envista <- function(){
    con_params <- get(config = "envista") # credentials
    con_params$driver
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
con <- connect.envista()

get.diag.tbl <- function(con, dateon, dateoff){
    # function to get the raw diagnostics table from the envista database
    tbl(con, "TB_DIAGNOSTICS") %>% 
        filter(between(DIG_DateTime,
                       as_datetime(dateon),
                       as_datetime(dateoff)),
               DIG_Channel == 1) %>% 
        collect() %>% 
        return()
}

make.long.diag.tbl <- function(diag_tbl){
    # function to pivot longer the names, units and values from the 
    # diagnostics table and combine into one dataframe
    
    names_df <- diag_tbl %>% 
        select(DIG_DateTime, DIG_Station, contains("Name")) %>% 
        pivot_longer(cols = contains("Name"),
                     names_to = "DIG_Name",
                     values_to = "parameter")
    
    units_df <- diag_tbl %>% 
        select(DIG_DateTime, DIG_Station, contains("Unit")) %>% 
        pivot_longer(cols = contains("Unit"),
                     names_to = "DIG_Unit",
                     values_to = "unit")
    
    values_df <- diag_tbl %>% 
        select(DIG_DateTime, DIG_Station, contains("Value")) %>% 
        pivot_longer(cols = contains("Value"),
                     names_to = "DIG_Value",
                     values_to = "value")
    
    long_diag_tbl <- cbind(names_df %>% 
                               select(-DIG_Name),
                           units_df %>%
                               select(unit),
                           values_df %>% 
                               select(value)) %>% 
        as_tibble()
    return(long_diag_tbl)
}

make.clean.plot <- function(data, site, datelabel){
    # takes the nested df, strips out conc data (not interesting)
    # and produces ggplot with facets

    p <- data %>% 
        filter(!str_detect(parameter, "Conc")) %>% 
        mutate(par_unit = glue("{parameter} {unit}")) %>% 
        ggplot(aes(x = DIG_DateTime, y = value)) +
        geom_line() +
        geom_line(aes(x = DIG_DateTime,
                      y = normal_low),
                  color = "blue",
                  lty = 5,
                  alpha = 0.6) +
        geom_line(aes(x = DIG_DateTime,
                      y = normal_high),
                  color = "blue",
                  lty = 5,
                  alpha = 0.6) +
        facet_wrap(~par_unit, scales = "free_y", ncol = 2) +
        labs(title = glue("Diagnostics plots for {site}: {datelabel}"),
             x = "Date") +
        scale_x_datetime(breaks = "1 weeks", date_labels = "%d")+
        theme_minimal() +
        theme(strip.background = element_rect(fill = 'azure2',
                                              linewidth = 0,
                                              linetype = 0)) +
        theme(strip.text = element_text(face = "bold",
                                        size = 8))
    return(p)
} ##

make.station.site.tbl <- function(final_tbl, sites_tbl){
    station_site_tbl <- final_tbl %>% 
        mutate(station = str_remove(tablename, "S") %>% as.integer()) %>% 
        distinct(station, siteid) %>% 
        inner_join(sites_tbl, by = "siteid")
    
    return(station_site_tbl)
    
}

make.clean.long.diag.tbl <- function(long_diag_tbl, limits_tbl, station_site_tbl){
    clean_long_diag_tbl <- long_diag_tbl %>% 
        na.omit() %>% 
        mutate(parameter = str_trim(parameter),
               unit = str_trim(unit)) %>%
        filter(value != -9999) %>% 
        inner_join(station_site_tbl, by = c("DIG_Station" = "station")) %>% 
        left_join(limits_tbl, by = join_by(parameter == parameter))
    
    return(clean_long_diag_tbl)
}

make.all.sites.plots.tbl <- function(clean_long_diag_tbl,
                                     datelabel){
    all_sites_plots_tbl <- clean_long_diag_tbl %>%
        select(-DIG_Station) %>% 
        nest_by(siteid, site_name) %>% 
        mutate(plot = list(make.clean.plot(data,
                                           site = site_name,
                                           {{datelabel}})))
    return(all_sites_plots_tbl)
}

# 3.0 Calibration Factors

get.cal.tbl <- function(google_config){
    
    cal_raw_tbl <- read_sheet(google_config$cal_sheet_path,
                              sheet = "Workings",
                              col_types = "______ddccdddd--")
    
    out_tbl <- cal_raw_tbl %>%
        rename_with(tolower) %>% 
        clean_names() %>% 
        mutate(date = as.Date(date, format = "%d/%m/%Y")) %>% 
        return()
}

get.gas.tbl <- function(google_config){
    gas_raw_tbl <- read_sheet(google_config$cal_sheet_path,
                              sheet = "Workings",
                              col_types = "cii", range = "A1:C31")
    
    gas_tbl <- gas_raw_tbl %>%
        rename_with(tolower) %>% 
        clean_names() %>% 
        na.omit()
    return(gas_tbl)
}

get.responses.tbl <- function(google_config){
    gs4_auth(email = google_config$email)
    responses_raw_tbl <- read_sheet(google_config$cal_sheet_path,
                                    sheet = "Form responses 1",
                                    col_types = "cc_cccdddddddddddddddddd_cd_")
    
    goodname <- function(string){
        
        poll <- string    %>% 
            str_extract("no\\d|nox|no")
        
        time <- string %>% 
            str_extract("\\d..")
        
        cal <- string %>%
            str_extract("zero|span")
        
        glue("{poll}_{time}_{cal}") %>% 
            return()
        
    }
    
    responses_tbl <- responses_raw_tbl %>% 
        rename_with(tolower) %>% 
        clean_names() %>%
        rename(cylinder_number = enter_the_cylinder_number,
               warnings = note_any_warnings_or_problems_on_site) %>% 
        rename_with(.fn = goodname, .cols = starts_with("enter")) %>% 
        mutate(date = as.POSIXct(timestamp,
                                 format = "%d/%m/%Y %H:%M:%S")
        ) %>% 
        select(date,
               site,
               cylinder_number,
               everything()) %>% 
        select(!c(timestamp,
                  starts_with("select"),
                  starts_with("no_off"))) %>% 
        pivot_longer(cols = starts_with("no")) %>% 
        separate(name, into = c("pollutant", "order", "cal"))
    
    return(responses_tbl)
}

make.cal.plot.tbl <- function(cal_tbl, dateon, dateoff){
    
    cal_plot_tbl <- cal_tbl %>% 
        filter(between(date, as.Date(dateon), as.Date(dateoff))) %>% 
        rename(NOx_span = noxvs,
               NO_span = novs,
               NOx_zero = noxvz_nox_zero,
               NOx_sensitivity = nox_sensitivity_fnox,
               NO_zero = novz_no_zero,
               NO_sensitivity = no_sensitivity_fno) %>%
        mutate(NOx_NO_span_delta = (NOx_span - NO_span) * 100 / NOx_span) %>% 
        pivot_longer(cols = starts_with("NO"),
                     names_to = "calibration",
                     values_to = "value")
    return(cal_plot_tbl)
}

make.date.list <- function(dateon, dateoff){
    days <- (as.Date(dateoff) - as.Date(dateon)) %>% as.integer()
    
    if (days > 100){
        db <- "3 months"
        dl <- "%b"
    } else if (days > 31) {
        db <- "7 days"
        dl <- "%e"
    } else {
        db <- "3 days"
        dl <- "%e"
    }
    
    end_month <- strftime(dateoff, "%B")
    start_month <- strftime(dateon, "%B")
    
    date_breaks_labels_list <- list(
        "breaks" = db,
        "labels" = dl,
        "dateon" = dateon,
        "dateoff" = dateoff,
        "start_month" = start_month,
        "end_month" = end_month,
        "days" = days)
    
    return(date_breaks_labels_list)
}

plot.span.diff <- function(cal_plot_tbl, date_list){
    # check to see there are two spans to compare
    cals_twice_month_tbl <- cal_plot_tbl %>%
        filter(calibration == "NOx_NO_span_delta") %>% 
    add_count(site, calibration) %>% 
    filter(n > 1) 
    
    if (nrow(cals_twice_month_tbl) > 0){ # there are two spans to compare

    
    span_diff_plot <- cals_twice_month_tbl %>% 
        filter(calibration == "NOx_NO_span_delta") %>% 
        ggplot(aes(x = date, y = value)) +
        geom_line(linewidth = 1, colour = "#3200D1") +
        geom_hline(yintercept = c(10, -10),
                   colour = "firebrick",
                   lty = 5) +
        scale_x_date(date_breaks = date_list$breaks,
                     date_labels = date_list$labels) +
        facet_wrap(~site, ncol = 2) +
        labs(title = "Divergence between NOx and NO Span Value",
             subtitle = glue("Between {date_list$dateon} and {date_list$dateoff}"),
             y = "% difference",
             x = "Date",
             caption = "Values > 10% indicate possible cylinder oxidation") +
        theme_minimal() +
        theme(strip.background = element_rect(fill = 'azure2',
                                              linewidth = 0,
                                              linetype = 0)) +
        theme(strip.text = element_text(face = "bold",
                                        size = 12))
    
    return(span_diff_plot)} else { # only one span to compare
        
        return("Twice monthly calibration data does not exist to make span plot")
    }
    
} ##

make.cal.factor.gt <- function(cal_plot_tbl, date_list){
    
    cal_factor_gt <- cal_plot_tbl %>% 
        filter(str_detect(calibration, "sensitivity|zero")) %>% # just cal data 
        separate(col = calibration,
                 into = c("pollutant",
                          "point"),
                 remove = TRUE) %>%
        mutate(target = if_else(point == "zero", 0.05, 1),
               val = round(value, 2), # set red line targets for factors
               point = str_to_title(point)) %>%
        group_by(site, date) %>% 
        gt(rowname_col = "date", groupname_col = "site") %>% # code below is
        tab_options(row_group.as_column = TRUE, # gt formatting
                    column_labels.font.weight = "bold"
        ) %>% 
        fmt_number(columns = value, decimals = 2) %>% 
        cols_label(pollutant = "Pollutant",
                   point = "Factor",
                   value = "Value",
                   val = "Target") %>%
        gt_plt_bullet(column = val, target = target) %>% 
        tab_header(
            title = "NOx Calibration Factors Summary",
            subtitle = glue("{date_list$dateon} to {date_list$dateoff}")) %>% 
        tab_style(style = list(cell_text(weight = "bold")),
                  locations = cells_row_groups())
    
    return(cal_factor_gt)
}

# 4.0 Routers

get.router.pat <- function(){
    
    get(config = "teltonika")$pat %>% 
        return()
}

# pat <- get.router.pat()

get.device.data <- function(device_url, pat){
    
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

get.devices.details.tbl <- function(device_url, pat){
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
get.data.use <- function(dateon, dateoff, device_url, pat, device_id_tbl) {
    
        stopifnot("Data only available for past three months: adjust dateon" =
                  difftime(as.Date(dateon), Sys.Date()) > -90)
    
    single.site.data <- function(dateon, dateoff, device_url, id, pat){
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
    # req_dry_run() #- for testing
    
        response_data_usage %>%
        resp_body_json() %>%
        pluck("data") %>%
        list.rbind() %>%
        as.data.frame() %>%
        mutate(id = id) %>%
            return()
    }
    # create a partial function for implementing the data retrieval
    # over multiple sites
    get.data.use.partial <- partial(.f = single.site.data,
                                dateon = dateon,
                                dateoff = dateoff,
                                device_url = device_url,
                                pat = pat)
    # map the partial function over the site id's
    # returning a data frame by row
    
    # get.data.use.partial(413202)
    all_sites_data_tbl <- map_dfr(device_id_tbl$id,
                        .f = ~get.data.use.partial(.x)) %>% 
    mutate(across(where(is.list), unlist))
    
            return(all_sites_data_tbl)
}



make.daily.data.tbl <- function(data_use_tbl, device_id_tbl){
    daily_data_tbl <- data_use_tbl %>% 
        select(- starts_with("sim2")) %>% 
        pivot_longer(cols = starts_with("sim1"),
                     names_to = "r_t",
                     values_to = "bytes") %>% 
        mutate(r_t = str_sub(r_t, start = 6, end = 7),
               MB = bytes %/% 1e6,
               bytes = NULL,
               date = lubridate::ymd(date),
               id = as_factor(id)) %>% 
        inner_join(device_id_tbl, by = "id", relationship = "many-to-many")
    
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
             x = "Date") +
         theme_minimal() +
        theme(strip.background = element_rect(fill = 'azure2',
                                              linewidth = 0,
                                              linetype = 0)) +
        theme(strip.text = element_text(face = "bold",
                                        size = 12)) %>% 
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

make.cumulative.plot <- function(cumulative_tbl, datelabel){
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

# 5.0 Missingness of Continuous data ----

get.aq_data.tbl <- function(final_tbl,
                        dateon = "2022-12-01",
                        dateoff = "2022-12-31",
                        siteid = c(215, 270, 463, 203, 501, 672),
                        timebase = 60){

#variable to use in padding function
padby <- case_when(
  timebase == 15 ~ "15 min",
  timebase == 60 ~ "hour",
  timebase == 1440 ~ "day",
  TRUE ~ "hour"
)

if (any(siteid %in% "all")) siteid = unique(final_tbl$siteid) 

#this tbl filters for the sites and timebase supplied to the function
  filtered_tbl <- final_tbl %>% 
  filter(siteid %in% {{siteid}},
    minutes == timebase) 
  
# this tbl adds the start and end date  
arg_tbl <- filtered_tbl %>% 
  select(siteid, value, status, table) %>% 
  mutate(dateStartPOS = as.POSIXct(dateon),
         dateEndPOS = as.POSIXct(glue("{dateoff} 24:00")),
         table_site = glue("{siteid}_{table}"))
#we convert to a list with tibles in each element
# to iterate over and supply the siteids when mapped to a df
site_list <- arg_tbl %>%
      split(arg_tbl$table_site) %>% 
  map(select, -c(siteid, table_site)) #remove siteid and table_site as it is the list element name

#function to extract data for each line of the tibble =
#combination of pollutant and timebase for each site
getData <- function(value,
                    status,
                    table,
                    dateStartPOS,
                    dateEndPOS,
                    con = connect.envista()){
# dbplyr to get each table
  tbl(con, as.character(table)) %>%
    filter(between(Date_Time, dateStartPOS, dateEndPOS)) %>%
    select(Date_Time, all_of(value), all_of(status)) %>%
    collect() 
}

output <- map_dfr(site_list,
                  ~pmap_dfr(.l = .x,
                            .f = getData),
                  .id = "table_site")

# output

long <- output %>% 
  separate(table_site, into = c("siteid", "table"), sep = "_", remove = T) %>% 
  pivot_longer(cols = -(siteid:Date_Time), names_to = "metric") %>% 
inner_join(filtered_tbl %>% 
             mutate(siteid = as.character(siteid)) %>% 
                      pivot_longer(cols = value:status) %>% 
             select(siteid, pollutant, metric = value, table),
           by = c("siteid" = "siteid",
                  "metric" = "metric",
                  "table" = "table")) %>% 
  filter(!is.na(value)) %>% 
  mutate(field = str_sub(metric, 1, 1)) %>% 
  select(-metric) %>% 
  pivot_wider(id_cols = c(siteid, table, Date_Time, pollutant),
              names_from = field,
              values_from = value) %>% 
  mutate(V = replace(V, S != 1, NA)) %>% 
  pivot_wider(id_cols = c(siteid, Date_Time),
              names_from = pollutant,
              values_from = V) %>% 
  group_by(siteid) %>% 
  pad_by_time(.date_var = Date_Time, 
              .by = padby) %>%
  rename(date = Date_Time) %>% 
  mutate(siteid = as.integer(siteid))

long %>% return()

}

filter.aq.data.tbl <- function(aq_data_cumu_tbl, dateon, dateoff){
     aq_data_cumu_tbl %>% 
    filter(date >= as.Date(dateon),
           date <= as.Date(dateoff) + days(1)) %>% 
        return()
    
}

make.missing.data.tbl <- function(aq_data_tbl, station_site_tbl){
    aq_data_tbl %>% 
    split(.$siteid) %>% 
    map_dfr(~remove_empty(.x, which = "cols") %>% 
            miss_var_summary()) %>% 
    filter(str_detect(variable, "no2|pm10|pm2\\.5|rh|temp")) %>% 
    inner_join(station_site_tbl, by = "siteid", multiple = "all", relationship = "many-to-many") %>%
    ungroup() %>%
    transmute(site_name,
              pollutant = toupper(variable),
              n_miss,
              pct_miss = round(pct_miss, 1),
              pct_dc = 100 - pct_miss) %>% 
    distinct() %>% 
        arrange(pollutant, pct_dc) %>% 
        return()
}


if(testing){

# TESTING --------------------------

# ------------------DIAGNOSTICS ----------------

final_tbl <- get.final.tbl()
limits_tbl <- make.limits.tbl()
sites_tbl <- make.sites.tbl()
station_site_tbl <-  make.station.site.tbl(final_tbl, sites_tbl)

con <- connect.envista()
diag_tbl <- get.diag.tbl(con, dateon, dateoff)
dbDisconnect(con)

long_diag_tbl <- make.long.diag.tbl(diag_tbl)

clean_long_diag_tbl <- make.clean.long.diag.tbl(long_diag_tbl,
                                                limits_tbl,
                                                station_site_tbl)

datelabel = make.datelabel(dateon, dateoff)
all_sites_plots_tbl <- make.all.sites.plots.tbl(clean_long_diag_tbl, datelabel)

# all_sites_plots_tbl$plot

#------------------ Calibrations ---------------

responses_tbl <- get.responses.tbl(google_config)

cal_tbl <- get.cal.tbl(google_config)  

cal_plot_tbl <- make.cal.plot.tbl(cal_tbl, dateon, dateoff)

date_list <- make.date.list(dateon, dateoff)

span_diff_plot <- plot.span.diff(cal_plot_tbl, date_list)

cal_factor_gt <- make.cal.factor.gt(cal_plot_tbl, date_list)

# cal_factor_gt

# ggsave("../images/span_diff_plot.png", plot = span_diff_plot, bg = "white")

#------------------Routers -------------------

pat <- get.router.pat()
device_data <- get.device.data(device_url = device_url, pat = pat)
device_id_tbl <- make.device.id.tbl(device_data = device_data)
device_wide_tbl <- make.device.wide.tbl(device_data = device_data)
sim_data_tbl <- make.sim.data.tbl(device_wide_tbl = device_wide_tbl)
devices_details_tbl <- get.devices.details.tbl(device_url = device_url, pat = pat)

data_use_tbl <- get.data.use(dateon, dateoff, device_url, pat, device_id_tbl)

daily_data_tbl <- make.daily.data.tbl(data_use_tbl = data_use_tbl,
                                      device_id_tbl = device_id_tbl)

daily_use_plot <- make.daily.data.use.plot(daily_data_tbl = daily_data_tbl)
cumulative_tbl <- make.cumulative.tbl(daily_data_tbl = daily_data_tbl)


cumulative_plot <- make.cumulative.plot(cumulative_tbl = cumulative_tbl,
                                        datelabel = datelabel)

plotly::ggplotly(cumulative_plot)
data_summary_tbl <- make.data.summary.tbl(daily_data_tbl = daily_data_tbl,
                                          device_id_tbl = device_id_tbl,
                                          datelabel = datelabel)

#--------------Missingness--------------------
# get all data from start of year
aq_data_cumu_tbl <- get.aq_data.tbl(final_tbl = final_tbl,
                dateon = lubridate::floor_date(as.Date(dateon), unit = "year"),
                dateoff = dateoff,
                siteid =  c(215, 270, 463, 203, 501, 672),
                timebase = 60)

# get missing for cumulative data
aq_missing_cumu_tbl <- make.missing.data.tbl(aq_data_cumu_tbl, station_site_tbl = station_site_tbl)

# get aq data just for stated period
aq_data_tbl <- aq_data_cumu_tbl %>% 
    filter(date >= as.Date(dateon),
           date <= as.Date(dateoff) + days(1))
# get missing summary just for stated period
missing_data_tbl <- make.missing.data.tbl(aq_data_tbl, station_site_tbl)

}


