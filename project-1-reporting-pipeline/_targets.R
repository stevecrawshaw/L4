# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed. # nolint

# Set target options:
tar_option_set(
  packages = c("odbc",
               "here",
               "dplyr",
               "DBI",
               "config",
               "tidyverse",
               "openair",
               "fastverse",
               "janitor",
               "timetk",
               "glue",
               "rvest",
               "lubridate",
               "readxl",
               "openxlsx2",
               "scales",
               "ggtext"), # packages that your targets need to run
  format = "rds",
  memory = "transient",
  garbage_collection = TRUE # default storage format
  # Set other options as needed.
)

# tar_make_clustermq() configuration (okay to leave alone):
options(clustermq.scheduler = "multiprocess")

# tar_make_future() configuration (okay to leave alone):
# Install packages {{future}}, {{future.callr}}, and {{future.batchtools}} to allow use_targets() to configure tar_make_future() options.

# Run the R scripts in the R/ folder with your custom functions:
tar_source("R/functions.R")
# source("other_functions.R") # Source other scripts as needed. # nolint

# Replace the target list below with your own:
list(
  tar_target(
    name = final_tbl,
    command = get.final.tbl()
  ),
  tar_target(
      name = last_years_sites_tbl,
      command = get.lastyears.sites(connect.access(), startDate = startDate)
  ),
  tar_target(
      name = aqms_tbl,
      command = get.aqms(connect.access())
  ),
  tar_target(
      name = no2_data,
      command = get.no2.data.db(connect.access(), startDate, endDate)
  ),
  tar_target(
      name = pivoted_tubes_tbl,
      command = pivot.tubes.monthly(no2_data)
  ),
  tar_target(
      name = step_2_tbl,
      command = make.step.2.table(aqms_tbl,
                                  pivoted_tubes_tbl,
                                  last_years_sites_tbl)
  ),
  tar_target(
      name = step_2a_tbl,
      command = get.background.data(no2_data)
  ),
  tar_target(
      name = bias_site_list,
      command = make.bias.site.list(aqms_tbl, no2_data)
  ),
  tar_target(
      name = coloc_divisor_tbl,
      command = make.coloc.divisor.tbl(aqms_tbl)
  ),
  tar_target(
      name = data_cap_period_tbl,
      command = make.data.cap.period.tbl(coloc_divisor_tbl,
                                         no2_data)
  ),
  tar_target(
      name = annual_tube_data_4years_tbl,
      command = get.annual.tube.data.4yrs.tbl(connect.access(),
                                              startDate)
  ),
  tar_target(
      name = count_tubes_tbl,
      command = get.count.tubes.tbl(connect.access())
  ),
  tar_target(
      name = ods_tubes_upload_tbl,
      command =  make.ods.upload.tube.tbl(connect.access(),
                                          count_tubes_tbl,
                                          path = "../../tubes/data/read_dt_data.xlsx",
                                          startDate = startDate)
  ),
  tar_target(
      name = contin_4yrs_tbl,
      command = get.aq.data.all(startDate = as.Date(startDate) - years(4), endDate = endDate)
  ),
  tar_target(
      name = no2_data_cap_tbl,
      command = make.datacap.tbl(contin_4yrs_tbl,
                                 startDate = startDate,
                                 pollutant = no2)
  ),
  tar_target(
      name = pm2.5_data_cap_tbl,
      command = make.datacap.tbl(contin_4yrs_tbl,
                                 startDate = startDate,
                                 pollutant = pm2.5)
  ),
  tar_target(
      name = pm10_data_cap_tbl,
      command = make.datacap.tbl(contin_4yrs_tbl,
                                 startDate = startDate,
                                 pollutant = pm10)
  ),
  tar_target(
      name = table_a1,
      command = make.table.a1(aqms_tbl)
  ),
  tar_target(
      name = table_a2,
      command =  make.table.a2(aqms_tbl)
  ),
  tar_target(
      name = table_a3,
      command = make.table.a3(contin_4yrs_tbl,
                              startDate,
                              aqms_tbl,
                              no2_data_cap_tbl)
  ),
  tar_target(
      name = table_a4,
      command = make.table.a4(annual_tube_data_4years_tbl,
                              data_cap_period_tbl)
  ),
  tar_target(
      name = table_a5,
      command = make.table.a5(contin_4yrs_tbl, startDate,
                              aqms_tbl,
                              no2_data_cap_tbl)
  ),
  tar_target(
      name = table_a6,
      command = make.table.a6(contin_4yrs_tbl,
                              aqms_tbl = aqms_tbl,
                              startDate = startDate,
                              pm10_data_cap_tbl = pm10_data_cap_tbl)
  ),
  tar_target(
      name = table_a7,
      command = make.table.a7(contin_4yrs_tbl,
                              startDate,
                              aqms_tbl,
                              pm10_data_cap_tbl)
  ),
  tar_target(
      name = table_a8,
      command = make.table.a8(contin_4yrs_tbl,
                              startDate,
                              aqms_tbl,
                              pm2.5_data_cap_tbl)
  ),
  tar_target(
    name = table_list,
    command = make.table.list(step_2_tbl,
                           step_2a_tbl,
                           table_a1,
                           table_a2,
                           table_a3,
                           table_a4,
                           table_a5,
                           table_a6,
                           table_a7,
                           table_a8)
  ),
  tar_target(
      name = write_spreadsheets,
      command = write.spreadsheets(table_list,
                                   bias_site_list,
                                   ods_tubes_upload_tbl,
                                   startDate)
  ),
  tar_target(
      name = plotareas_tbl,
      command = make.plotareas_tbl()
  ),
  tar_target(
      name = no2_trend_chart_tbl,
      command = make.no2.trend.chart.tbl(startDate,
                                         ods_tubes_upload_tbl,
                                         plotareas_tbl,
                                         aqms_tbl)
  ),
  tar_target(
      name = pm25_trend_chart,
      command = make.pm25.trend.chart(startDate)
  ),
  tar_target(
      name = write_no2_trend_charts,
      command = write.no2.trend.charts(no2_trend_chart_tbl)
  ),
  tar_target(
      name = write_pm25_trend_chart,
      command = write.pm25.trend.chart(pm25_trend_chart)
  )
    
)
