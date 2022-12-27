
#######################################################################
# TESTING ----

# functions below used by get.temple.way.sds.hr.tbl()
zip_file_urls <- get.zip.file.urls(start_date = start_date)
daily_csv_urls = get.daily.csv.urls()
test_csv_tbl <- get.sds.csv.tbl(daily_csv_urls[1])
get.sds.zip.tbl(zip_file_urls[1])
daily_files_tbl <- get.daily.files.tbl(daily_csv_urls)
monthly_files_tbl <- get.monthly.files.tbl(zip_file_urls)

# Sourcing Tests ----

temple_way_sds_hr_tbl <-
    make.temple.way.sds.hr.tbl(daily_files_tbl,
                                      monthly_files_tbl)


# write_rds(
#     temple_way_hr_tbl,
#     glue("data/{sensor}_{Sys.Date()}_raw.rds")
# )

# temple_way_hr_tbl <-
#     read_rds("../air quality analysis/data/esp8266-6496445_2022-08-08_raw.rds")

parson_st_sds_hr_tbl <-
    get.parson.st.sds.hr.tbl(start_date = start_date) # parson st low cost

ref_tbl <- get.ref.tbl(start_date = start_date) # BAM data from both sites

combined_long_tbl <- make.combined.long.tbl(ref_tbl,
                                   temple_way_sds_hr_tbl,
                                   parson_st_sds_hr_tbl)

model_data_tbl <- make.model.data.tbl(combined_long_tbl)

model_data_tbl %>% 
    saveRDS(file = "data/model_data_tbl.rds")

# Plotting Tests ----

plot.drift.site.gg(model_data_tbl, site = 215)

plot.scatter.site.gg(model_data_tbl$md_wide[1][[1]])

timeplot_tbl <- prep.timeplot.tbl(model_data_tbl)

plot.time.series.gg(timeplot_tbl, interval = "day")

sp_plot_tbl <- prep.sp.plot.tbl(timeplot_tbl)

save.png.summaryplot(sp_plot_tbl, pollutant = "pm10")
# Model Tests ----

model_data_tbl <- read_rds("data/model_data_tbl.rds")

model_output_tbl <- make.model.output.tbl(model_data_tbl)