

single.site.data <- function(dateon, dateoff, device_url, id, pat){
    
    stopifnot("Data only available for past three months: adjust dateon" =
                  difftime(as.Date(dateon), Sys.Date()) > -90)
    
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
    
    dateon = "2022-12-15"

        bris <- single.site.data(dateon, dateoff, id = 400809, 
                             device_url = device_url, pat = pat)
    difftime(as.Date("2022-12-29"), Sys.Date())
    
    # get.data.use.partial(413202)
    all_sites_data_tbl <- map_dfr(device_id_tbl$id,
                        .f = ~get.data.use.partial(.x))
    
            return(all_sites_data_tbl)