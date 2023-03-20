
make.model.output.tbl <- function(model_data_tbl){
    model_output_tbl <- model_data_tbl %>%
        mutate(
            model_obj = map(md_wide, ~pluck(.x) %>% 
                                lm(reference ~ low_cost, data = .)),
            coefs = map(model_obj, ~ pluck(.x) %>%
                            tidy()),
            perf = map(model_obj, ~ pluck(.x) %>%
                           glance()),
            cor_test = map(md_wide,
                           ~ cor_test(.x,
                                      "reference",
                                      "low_cost") %>%
                               as_tibble()),
            plot = map(
                md_wide,
                ~ plot.scatter.site.gg(.) +
                    labs(title = quickText(
                        glue("Scatter plot of {get.title(siteid)} (ugm-3)")
                    ),
                    subtitle = "Reference Instrument (BAM 1020) vs. Low Cost Sensor (SDS011)")
            ),
            pollutant = if_else(siteid == 215L, "PM2.5", "PM10")
        )
    return(model_output_tbl)
}

make.dashboard <- function(model_output_tbl, siteid = 215){
    # pre fill the model_dashboard function
    dash <- partial(.f = model_dashboard,
                    parameters_args = NULL,
                    performance_args = NULL,
                    output_dir = here::here(),
                    rmd_dir = system.file("templates/easydashboard.Rmd", package = "easystats")
    )
    # iteratively generate the dashboards from the table of model objects
    dash_tbl <- model_output_tbl %>% 
        ungroup() %>% 
        filter(siteid == {{siteid}}) %>% 
        select(model_obj, pollutant)
    
    file_path <- glue("site_{siteid}_{dash_tbl$pollutant}_dashboard.html")
    
    dash(model = dash_tbl$model_obj[[1]], output_file = file_path)
    
    return(file_path)
}

make.model.perf.tbl <- function(model_output_tbl){
    
    model_perf_tbl <- model_output_tbl %>% 
        select(siteid, pollutant, perf) %>% 
        unnest(perf) 
    return(model_perf_tbl)
    
}

transpose.model.perf.tbl <- function(model_perf_tbl){

    headings <- glue("Site {model_perf_tbl$siteid}: {model_perf_tbl$pollutant}")
    rownms <- names(model_perf_tbl[-c(1:2)])
    
    t_perf_tbl <- model_perf_tbl %>%
        mutate(across(.cols = 2:last_col(), ~round(as.double(.x), 2))) %>%
        setDT() %>%
        transpose(.) %>%
        .[-c(1:2), metric := rownms] %>%
        na.omit()
    
    setnames(t_perf_tbl, old = c("V1", "V2"), new = headings)
    setcolorder(t_perf_tbl, neworder = c(3, 1, 2))
    
    return(t_perf_tbl)
}

make.model.perf.tbl.gt <- function(model_output_tbl){
    
    models <- model_output_tbl$model_obj
    
    regression_tbl_list <- models %>% 
        map(~tbl_regression(.x) %>% 
                add_glance_table())
    
    table_names <- glue("Site {model_output_tbl$siteid}: {model_output_tbl$pollutant}")
    
    tbl_merge(regression_tbl_list, tab_spanner = table_names)
    
}



