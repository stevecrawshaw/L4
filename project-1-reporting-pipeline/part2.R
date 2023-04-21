# This script to be run when the final bias and distance adjusted and
# annualised data are available.

# The tube data are sourced from the output (export) of the DTDES spreadsheet
# A csv file is written which can be appended to the tbl_final_ba_annual
# table in the no2_data MS access database - not safe to do this in R

source("R/functions.R")
p_load(char = p)

# get the path to the DTDES excel file exported by the DTPT spreadsheet
dtdes_path <-
    choose.files(
        default = here("data", "dtdes_test.xlsx"),
        caption = "Select the exported DTDES spreadsheet",
        multi = FALSE,
        filters = "xlsx"
    )

# initiate connection for access
if (exists("con")) {
    con %>% dbDisconnect()
} else {
con <- connect.access()
}

annual_tube_data_append_tbl <- make.annual.tube.append.tbl(dtdes_path, 
                                                           startDate = startDate)

count_tubes_tbl <- get.count.tubes.tbl(con)

ods_tubes_upload_tbl <-
    make.ods.upload.tube.tbl(con,
                             count_tubes_tbl,
                             annual_tube_data_append_tbl,
                             startDate)

plotareas_tbl <- make.plotareas_tbl()
aqms_tbl <- get.aqms(con)
no2_trend_chart_tbl <- make.no2.trend.chart.tbl(startDate,
                                                ods_tubes_upload_tbl,
                                                plotareas_tbl,
                                                aqms_tbl)

no2_trend_chart_tbl <- make.no2.trend.chart.tbl(startDate,
                                                ods_tubes_upload_tbl,
                                                plotareas_tbl,
                                                aqms_tbl)

pm25_trend_chart <- make.pm25.trend.chart(startDate)


annual_tube_data_append_tbl %>%
    write_csv(file = "data/annual_tube_data_append_tbl.csv", na = "")

# don't write the new data into the table in MS Access no2_data with R
# Export and run append query
ods_tubes_upload_tbl_file = glue("data/ods_tubes_upload_{year(startDate)}.csv")
write.csv2(ods_tubes_upload_tbl, ods_tubes_upload_tbl_file)


write.no2.trend.charts(no2_trend_chart_tbl)
write.pm25.trend.chart(pm25_trend_chart)

annual_tubes_tbl <- make.annual.tubes.tbl(aqms_tbl,
                                  annual_tube_data_append_tbl)

write.tube.shapefile.year(annual_tubes_tbl, startDate)


con %>% dbDisconnect()




