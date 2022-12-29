# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed. # nolint
library(pacman)
p_load(char = c(
       "tidyverse",
       "lubridate",
       "httr2",
       "jsonlite",
       "glue",
       "janitor",
       "fs",
       "padr",
       "gt",
       "data.table",
       "tidymodels",
       "ggside",   # side plots of density
       "ggpubr",   # easy labelling of equations on the plot
       "openair",
       "easystats",
       "gtsummary")
    )
# Set target options:
tar_option_set(
  format = "rds" # default storage format
  # Set other options as needed.
)
# Variables ----
start_date <- "2022-05-01" # BTW started operating
# tar_make_clustermq() configuration (okay to leave alone):
options(clustermq.scheduler = "multicore")

# tar_make_future() configuration (okay to leave alone):
# Install packages {{future}}, {{future.callr}}, and {{future.batchtools}} to allow use_targets() to configure tar_make_future() options.

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# source("other_functions.R") # Source other scripts as needed. # nolint
source("../../airquality_GIT/ods-import-httr2.R")
source("../../airquality_GIT/gg_themes.R")
# Replace the target list below with your own:
list(
  tar_target(
    name = zip_file_urls,
    command = get.zip.file.urls(
        data_root_url = "https://api-rrd.madavi.de/data_csv/",
        sensor = "esp8266-6496445",
        start_date)
  ),
  tar_target(
    name = daily_csv_urls,
    command = get.daily.csv.urls(
        data_root_url = "https://api-rrd.madavi.de/data_csv/",
        sensor = "esp8266-6496445")
  ),
   tar_target(
    name = daily_files_tbl,
    command = get.daily.files.tbl(daily_csv_urls)
  ),
  tar_target(
    name = monthly_files_tbl,
    command = get.monthly.files.tbl(zip_file_urls)
  ),
  tar_target(
    name = temple_way_sds_hr_tbl,
    command = make.temple.way.sds.hr.tbl(daily_files_tbl,
                                         monthly_files_tbl)
  ),
  tar_target(
    name = parson_st_sds_hr_tbl,
    command = get.parson.st.sds.hr.tbl(start_date)
  ),
  tar_target(
    name = ref_tbl,
    command = get.ref.tbl(start_date)
  ),
  tar_target(
    name = combined_long_tbl,
    command = make.combined.long.tbl(ref_tbl,
                                     temple_way_sds_hr_tbl,
                                     parson_st_sds_hr_tbl)
  ),
  tar_target(
    name = model_data_tbl,
    command = make.model.data.tbl(combined_long_tbl)
  ),
  tar_target(
      name = write_model_data_tbl,
      command = write.model.data.tbl(model_data_tbl),
      format = "file"
  ),
  tar_target(
    name = timeplot_tbl,
    command = prep.timeplot.tbl(model_data_tbl)
  ),
  tar_target(
      name = time_series_hour_gg,
      command = plot.time.series.gg(timeplot_tbl, interval = "hour")
  ),
  tar_target(
      name = time_series_day_gg,
      command = plot.time.series.gg(timeplot_tbl, interval = "day")
  ),
  tar_target(
    name = sp_plot_tbl,
    command = prep.sp.plot.tbl(timeplot_tbl)
  ),
  tar_target(
    name = model_output_tbl,
    command = make.model.output.tbl(model_data_tbl)
  ),
  tar_target(
    name = dashboard_215,
    command = make.dashboard(model_output_tbl, siteid = 215),
    format = "file"
  ),
  tar_target(
    name = dashboard_500,
    command = make.dashboard(model_output_tbl, siteid = 500),
    format = "file"
  ),
  tar_target(
    name = save_png_pm25,
    command = save.png.summaryplot(sp_plot_tbl, "pm2.5"),
    format = "file"
  ),
  tar_target(
    name = save_png_pm10,
    command = save.png.summaryplot(sp_plot_tbl, "pm10"),
    format = "file"
  ),
  tar_target(
      name = drift_plot_500_gg,
      command = plot.drift.site.gg(model_data_tbl, site = 500)
  ),
  tar_target(
      name = drift_plot_215_gg,
      command = plot.drift.site.gg(model_data_tbl, site = 215)
  ),
  tar_target(
      name = save_gg_time_series_hour_gg,
      command = save.ggplot(time_series_hour_gg)
  ),
  tar_target(
      name = save_gg_time_series_day_gg,
      command = save.ggplot(time_series_day_gg)
  ),
  tar_target(
      name = save_drift_plot_500_gg,
      command = save.ggplot(drift_plot_500_gg)
  ),
  tar_target(
      name = save_drift_plot_215_gg,
      command = save.ggplot(drift_plot_215_gg)
  ),
  tar_target(
      name = scatter_gg_215,
      command = model_output_tbl[model_output_tbl$siteid == 215,]$plot[[1]]
  ),
  tar_target(
      name = scatter_gg_500,
      command = model_output_tbl[model_output_tbl$siteid == 500,]$plot[[1]]
  ),
  tar_target(
      name = save_scatter_gg_215,
      command = save.ggplot(scatter_gg_215)
  ),
  tar_target(
      name = save_scatter_gg_500,
      command = save.ggplot(scatter_gg_500)
  ),
  tar_target(
      name = model_perf_tbl,
      command = make.model.perf.tbl(model_output_tbl)
  ),
  tar_target(
      name = transpose_model_perf_tbl,
      command = transpose.model.perf.tbl(model_perf_tbl)
  ),
  tar_target(
      name = model_perf_tbl_gt,
      command = make.model.perf.tbl.gt(model_output_tbl)
  ),
  tar_target(
      name = save_model_perf_tbl_gt,
      command = save.model.perf.tbl.gt(model_perf_tbl_gt),
      format = "file"
  )
)
