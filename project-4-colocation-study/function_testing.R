
#######################################################################
# TESTING ----
# pacman::p_load(char = packages)
source("../../airquality_GIT/ods-import-httr2.R")
source("../../airquality_GIT/gg_themes.R")
# functions below used by get.temple.way.sds.hr.tbl()
zip_file_urls <- get.zip.file.urls(start_date = start_date,
                                   end_date = end_date)

daily_csv_urls = get.daily.csv.urls(end_date = end_date) # returns null if not needed
test_csv_tbl <- get.sds.csv.tbl(daily_csv_urls[1])
get.sds.zip.tbl(zip_file_urls[1])
daily_files_tbl <- get.daily.files.tbl(daily_csv_urls)
monthly_files_tbl <- get.monthly.files.tbl(zip_file_urls)

# Sourcing Tests ----

temple_way_sds_hr_tbl <-
    make.temple.way.sds.hr.tbl(daily_files_tbl,
                               monthly_files_tbl,
                               start_date = start_date,
                               end_date = end_date)

parson_st_sds_hr_tbl <-
    get.parson.st.sds.hr.tbl(start_date = start_date,
                             end_date = end_date) # parson st low cost

ref_tbl <- get.ref.tbl(start_date = start_date,
                       end_date = end_date) # BAM data from both sites

# temp_rh_tbl_raw <- get.temp.rh.raw.tbl(start_date, end_date, sensor_id = "71553")
# 
# temp_rh_tbl <- make.temp.rh.tbl(temp_rh_tbl_raw)
temp_rh_tbl <- make.temp.rh.tbl(start_date = start_date,
                                end_date = end_date,
                                unit = 'hour')

# temple way missing a lot of data from May to July 2022
combined_long_tbl <- make.combined.long.tbl(ref_tbl,
                                   temple_way_sds_hr_tbl,
                                   parson_st_sds_hr_tbl,
                                   temp_rh_tbl)

model_data_tbl <- make.model.data.tbl(combined_long_tbl)

model_data_tbl %>% 
    saveRDS(file = "data/model_data_tbl.rds")


# Plotting Tests ----

model_data_tbl <- read_rds(file = "data/model_data_tbl.rds")

plot.drift.site.gg(model_data_tbl, site = 215)

plot.scatter.site.gg(model_data_tbl$md_wide[1][[1]])
plot.scatter.site.gg(model_data_tbl$md_wide[2][[1]])

timeplot_tbl <- prep.timeplot.tbl(model_data_tbl)

tpgg <- plot.time.series.gg(timeplot_tbl, interval = "day")

sp_plot_tbl <- prep.sp.plot.tbl(timeplot_tbl)

save.png.summaryplot(sp_plot_tbl, pollutant = "pm10")

md_wide_split <- model_data_tbl$md_wide[1][[1]] %>% 
    na.omit() %>% 
    initial_time_split()

model.parameter.test <- function(md_wide_split){
# this function tests 3 combinations of input paramaters for a lm
    # and outputs a tbl of performance metrics for each one
    
    # the models are run on the training data and tested on the test data
    # as defined by the rsample::initial time splits function
trained_model_reference_low_cost <- linear_reg() %>% 
    set_engine('lm') %>% 
    fit(reference ~ low_cost,
        data = training(md_wide_split))

trained_model_reference_low_cost_humidity <- linear_reg() %>% 
    set_engine('lm') %>% 
    fit(reference ~ low_cost + humidity,
        data = training(md_wide_split))

trained_model_reference_low_cost_humidity_temperature <- linear_reg() %>% 
    set_engine('lm') %>% 
    fit(reference ~ low_cost + humidity + temperature,
        data = training(md_wide_split))

model_test_list <- list("reference_lowcost" = trained_model_reference_low_cost,
                        "reference_lowcost_humidity" = trained_model_reference_low_cost_humidity,
                        "reference_lowcost_humidity_temperature" = trained_model_reference_low_cost_humidity_temperature)

metric.test <- function(trained_model, md_wide_split){
    pred_tbl <- augment(trained_model, new_data = testing(md_wide_split))
    pred_tbl %>% 
    model_metrics(truth = reference, estimate = .pred) %>% 
        return()
}


map_dfr(model_test_list, ~metric.test(.x,
                                      md_wide_split = md_wide_split),
        .id = "model") %>% 
    transmute(model = str_replace_all(model, "_", " "),
              .estimator = NULL, .metric, .estimate) %>% 
    pivot_wider(id_cols = model, names_from = .metric, values_from = .estimate) %>% 
    return()
}

make.model.select.tbl <- function(model_data_tbl){

model_select_tbl <- model_data_tbl %>% 
    select(md_wide) %>% 
    mutate(md_wide_split = md_wide %>%
               pluck(1) %>%
               na.omit() %>% 
               initial_time_split() %>%
               list(),
           parameter_test_results = 
               model.parameter.test(md_wide_split = pluck(md_wide_split,1)) %>%
               list())

return(model_select_tbl)
}

model_select_tbl <- make.model.select.tbl(model_data_tbl)

make.model.select.gt <- function(model_select_tbl){

model_select_gt <- model_select_tbl %>% 
    select(-md_wide_split, -md_wide) %>% 
    unnest(parameter_test_results) %>% 
    mutate(pollutant = if_else(siteid == 215L,
                               md("PM<sub>2.5</sub>"),
                               md("PM<sub>10</sub>")),
           .after = siteid) %>% 
    
    gt() %>% 
    cols_label(pollutant = "Pollutant",
               model = "Model terms",
               rmse = "RMSE",
               rsq = "R squared",
               mae = "MAE") %>% 
    fmt_markdown(columns = c("pollutant")) %>% 
    fmt_number(columns = c("rmse", "rsq", "mae"),
               decimals = 3) %>% 
    tab_header("Model Selection Metrics")

return(model_select_gt)

}

model_select_gt <- make.model.select.gt(model_select_tbl = model_select_tbl)

gtsave(model_select_gt, 'plots/model_select_gt.png')



check_model(trained_model)    
tidy(trained_model)
glance(trained_model)

pred_tbl <- augment(trained_model, new_data = testing(md_wide_split)) 

pred_tbl %>% 
    select(date, reference, .pred) %>% 
    pivot_longer(cols = -date) %>% 
    ggplot(aes(x = date, y = value, colour = name)) +
    geom_line()

model_metrics <- metric_set(yardstick::rmse,
                            yardstick::rsq,
                            yardstick::mae)

pred_tbl %>% 
    model_metrics(truth = reference, estimate = .pred)



training(md_wide_split)





testing(md_wide_split)


model_output_tbl <- make.model.output.tbl(model_data_tbl)

scatter_gg_215 <- model_output_tbl[model_output_tbl$siteid == 215,]$plot[[1]]
scatter_gg_500 <- model_output_tbl[model_output_tbl$siteid == 500,]$plot[[1]]

# Model Tests ----

model_data_tbl <- read_rds("data/model_data_tbl.rds")

model_perf_tbl <- make.model.perf.tbl(model_output_tbl)

model_perf_tbl_gt <- make.model.perf.tbl.gt(model_output_tbl)


