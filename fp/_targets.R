# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed. # nolint

# Set target options:
tar_option_set(
  packages = c("tibble"), # packages that your targets need to run
  format = "rds",
  workspace_on_error = TRUE# https://books.ropensci.org/targets/debugging.html
  # Set other options as needed.
)

# tar_make_clustermq() configuration (okay to leave alone):
options(clustermq.scheduler = "multiprocess")

# tar_make_future() configuration (okay to leave alone):
# Install packages {{future}}, {{future.callr}}, and {{future.batchtools}} to allow use_targets() to configure tar_make_future() options.

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# source("other_functions.R") # Source other scripts as needed. # nolint

list(
    tar_target(
        name = check_dates_lgl,
        check.dates(dateon = dateon, dateoff = dateoff)
    ),
  tar_target(
    name = final_tbl,
    command = get.final.tbl()
  ),
    tar_target(
    name = limits_tbl,
    command = make.limits.tbl()
  ),
    tar_target(
    name = sites_tbl,
    command = make.sites.tbl()
  ),
    tar_target(
    name = diag_tbl,
    command = get.diag.tbl(con, dateon, dateoff)
  ),
    tar_target(
    name = long_diag_tbl,
    command = make.long.diag.tbl(diag_tbl)
  ),
    tar_target(
    name = station_site_tbl,
    command = make.station.site.tbl(final_tbl, sites_tbl)
  ),
    tar_target(
    name = clean_long_diag_tbl,
    command = make.clean.long.diag.tbl(long_diag_tbl, limits_tbl, station_site_tbl)
  ),
    tar_target(
    name = all_sites_plots_tbl,
    command = make.all.sites.plots.tbl(clean_long_diag_tbl, datelabel)
  ),
    tar_target(
    name = cal_tbl,
    command = get.cal.tbl(google_config)
  ),
    tar_target(
    name = gas_tbl,
    command = get.gas.tbl(google_config)
  ),
    tar_target(
    name = responses_tbl,
    command = get.responses.tbl(google_config)
  ),
    tar_target(
    name = cal_plot_tbl,
    command = make.cal.plot.tbl(cal_tbl, dateon, dateoff)
  ),
    tar_target(
    name = date_list,
    command = make.date.list(dateon, dateoff)
  ),
    tar_target(
    name = cal_factor_gt,
    command = make.cal.factor.gt(cal_plot_tbl, date_list)
  ),
    tar_target(
        name = span_diff_plot,
        command = plot.span.diff(cal_plot_tbl, date_list)
    ),
    tar_target(
    name = pat,
    command = get.router.pat()
  ),
    tar_target(
    name = device_data,
    command = get.device.data(device_url, pat)
  ),
    tar_target(
    name = device_id_tbl,
    command = make.device.id.tbl(device_data)
  ),
    tar_target(
    name = device_wide_tbl,
    command = make.device.wide.tbl(device_data)
  ),
    tar_target(
    name = sim_data_tbl,
    command = make.sim.data.tbl(device_wide_tbl)
  ),
    tar_target(
    name = devices_details_tbl,
    command = get.devices.details.tbl(device_url, pat)
  ),
    tar_target(
    name = data_use_tbl,
    command = get.data.use(dateon, dateoff, device_url, pat, device_id_tbl)
  ),
    tar_target(
    name = daily_data_tbl,
    command = make.daily.data.tbl(data_use_tbl, device_id_tbl)
  ),
    tar_target(
    name = datelabel,
    command = make.datelabel(dateon, dateoff)
  ),
    tar_target(
    name = daily_use_plot,
    command = make.daily.data.use.plot(daily_data_tbl)
  ),
    tar_target(
    name = cumulative_tbl,
    command = make.cumulative.tbl(daily_data_tbl)
  ),
    tar_target(
    name = cumulative_plot,
    command = make.cumulative.plot(cumulative_tbl, datelabel)
  ),
    tar_target(
    name = data_summary_tbl,
    command = make.data.summary.tbl(daily_data_tbl,
                                          device_id_tbl,
                                          datelabel)
  ),
  tar_target(name = aq_data_cumu_tbl,
             command = get.aq_data.tbl(final_tbl = final_tbl,
                dateon = lubridate::floor_date(as.Date(dateon), unit = "year"),
                dateoff = dateoff,
                siteid =  c(215, 270, 463, 203, 501, 672),
                timebase = 60)),
  tar_target(name = aq_data_tbl,
             command = filter.aq.data.tbl(aq_data_cumu_tbl,
                                          dateon,
                                          dateoff)),
  tar_target(name = aq_missing_cumu_tbl,
             command = make.missing.data.tbl(aq_data_cumu_tbl,
                                             station_site_tbl)),
    tar_target(
      name = missing_data_tbl,
      command =  make.missing.data.tbl(aq_data_tbl,
                                       station_site_tbl)
  )
)
