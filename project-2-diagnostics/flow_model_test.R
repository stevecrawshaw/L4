# Load packages ----
pacman::p_load(tidyverse,
               tidymodels,
               openair,
               glue,
               janitor,
               timetk,
               modeltime,
               lubridate,
               collapse)

# Read data ----
# just colston avenue for 2022
colston_flow_data_tbl <- read_rds("data/clean_combined_diagnostics_tbl.rds") %>% 
    filter(siteid == 203,
           parameter == "Sample Pres") %>%
    select(date = DIG_DateTime, value)

    unique(colston_flow_data_tbl$parameter)

aqdc <- read_csv('data/aqdc_esri.csv')

aurn <- aqdc %>% 
    filter(siteid == 452,
           between(date_time, as.POSIXct("2021-01-01"), as.POSIXct("2022-12-31"))) %>% 
    select(-siteid, -rh, -rh, -press) %>% 
    arrange(date_time)

data <- aurn %>% 
    transmute(date = date_time, value = pm10) %>% 
    na.omit()

skimr::skim(data)
# Wrangling ----

# strip anomalies - these are often cause by calibration processes when sample lines are disconnected

data %>% 
    timetk::plot_anomaly_diagnostics(.date_var = date, .value = value, .interactive = TRUE)

clean_data <- data %>% 
    timetk::tk_anomaly_diagnostics(.date_var = date, .value = value) %>% 
    # filter(anomaly == "No") %>% 
    select(date, value = observed) %>% 
    group_by(date = as.Date(date)) %>% 
    summarise(value = mean(value, na.rm = TRUE)) %>%
    mutate(centred = scale(value, center = TRUE, scale = TRUE))
    

clean_data %>% 
    plot_time_series(.date_var = date, .value = centred)

breaks <-  findBreakPoints(clean_data, pollutant = "value")

testBreakPoints(clean_data, pollutant = "value", breaks = breaks)

quantBreakPoints(clean_data, pollutant = "value", breaks = breaks)


# split the data
splits <- time_series_split(clean_data,
                            date_var = date,
                            assess = "3 months", cumulative = TRUE)

# visualise the CV plan

splits %>% 
    tk_time_series_cv_plan() %>% 
    plot_time_series_cv_plan(date, value)

# forecast

# ARIMA 
model_arima <- arima_reg() %>% 
    set_engine("auto_arima") %>% 
    fit(value ~ date, training(splits))

model_arima

# PROPHET

model_prophet <- prophet_reg(
    seasonality_yearly = TRUE,
    seasonality_weekly = FALSE,
    seasonality_daily = TRUE) %>% 
    set_engine("prophet") %>% 
    fit(value ~ date, training(splits))

model_prophet

# ML Model GLM
# elastic net penalised regression
model_glmnet <- linear_reg(penalty = 0.01) %>% 
    set_engine("glmnet") %>% 
    fit(value ~ date
         + as.numeric(as.Date(date)),
        training(splits))

model_glmnet

# Compare models ----

model_tbl <- modeltime_table(
    model_arima,
    model_prophet,
    model_glmnet
)

# Calibrate

calib_tbl <- model_tbl %>% 
    modeltime_calibrate(testing(splits))

calib_tbl %>% modeltime_accuracy()

# Forecast

calib_tbl %>% 
    modeltime_forecast(
        new_data = testing(splits),
        actual_data = clean_data
    ) %>% 
    plot_modeltime_forecast()

# Refit and forecast future

future_forecast_tbl <- calib_tbl %>% 
    modeltime_refit(clean_data) %>% 
    modeltime_forecast(
        h = "3 months",
        actual_data = clean_data
    )


future_forecast_tbl %>% 
    plot_modeltime_forecast()
    