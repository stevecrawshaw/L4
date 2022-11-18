# Testing ----
siteids <- c(175L, 239L, 405L, 502L, 567L)
con <- connect.access()
last_years_sites_tbl <- get.lastyears.sites(con, startDate)
aqms_tbl <- get.aqms(con)
no2_data <- get.no2.data.db(con, startDate, endDate)
pivoted_tubes_tbl <- pivot.tubes.monthly(no2_data)
step_2_tbl <- make.step.2.table(aqms_tbl, pivoted_tubes_tbl, last_years_sites_tbl)
step_2a_tbl <- get.background.data(no2_data)
coloc_divisor_tbl <- make.coloc.divisor.tbl(aqms_tbl)
data_cap_period_tbl <- make.data.cap.period.tbl(coloc_divisor_tbl,
                                                no2_data)
annual_tube_data_4years_tbl <- get.annual.tube.data.4yrs.tbl(con, startDate)
count_tubes_tbl <- get.count.tubes.tbl(con)

ods_tubes_upload_tbl <- 
    make.ods.upload.tube.tbl(con,
                             count_tubes_tbl,
                             path = "../../tubes/data/read_dt_data.xlsx",
                             startDate = startDate)
dbDisconnect(con)
con <- connect.envista()

contin_4yrs_tbl <- get.aq.data.all(startDate = as.Date(startDate) - years(4), endDate = endDate)

no2_data_cap_tbl <- make.datacap.tbl(contin_4yrs_tbl, startDate = startDate, pollutant = no2)
pm2.5_data_cap_tbl <- make.datacap.tbl(contin_4yrs_tbl, startDate = startDate, pollutant = pm2.5)
pm10_data_cap_tbl <- make.datacap.tbl(contin_4yrs_tbl, startDate = startDate, pollutant = pm10)

table_a1 <- make.table.a1(aqms_tbl)
table_a2 <- make.table.a2(aqms_tbl)
table_a4 <- make.table.a4(annual_tube_data_4years_tbl, data_cap_period_tbl)
table_a3 <- make.table.a3(contin_4yrs_tbl,
                          startDate, aqms_tbl, no2_data_cap_tbl)
table_a5 <- make.table.a5(contin_4yrs_tbl, startDate,
                          aqms_tbl, no2_data_cap_tbl)
table_a6 <- make.table.a6(contin_4yrs_tbl,
                          aqms_tbl = aqms_tbl,
                          startDate = startDate,
                          pm10_data_cap_tbl = pm10_data_cap_tbl)

table_a7 <- make.table.a7(contin_4yrs_tbl, startDate, aqms_tbl, pm10_data_cap_tbl)

table_a8 <- make.table.a8(contin_4yrs_tbl, startDate, aqms_tbl, pm2.5_data_cap_tbl)

write_spreadsheets <- write.spreadsheets()

plotareas_tbl <- make.plotareas_tbl()

no2_trend_chart_tbl <- make.no2.trend.chart.tbl(startDate,
                                                ods_tubes_upload_tbl,
                                                plotareas_tbl,
                                                aqms_tbl)

pm25_trend_chart <- make.pm25.trend.chart(startDate)



# to do
# add to targets
# write out shapefiles of >36, >40 etc
# maps?


# sites needing distance correction (needs to come from DTDP spreadsheet)

siteids <- c(175L, 239L, 405L, 502L, 567L)

dbDisconnect(con)

# write data ----
