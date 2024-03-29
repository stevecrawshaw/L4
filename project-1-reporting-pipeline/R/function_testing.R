# Testing ----

html_tables_list <- make.html.tables.list()

step1_dt_calendar_dates_tbl <- make.step1.dt.calendar.dates.tbl(html_tables_list, startDate)

step1_dt_first_last_dates_tbl <- make.step1.dt.first.last.dates.tbl(step1_dt_calendar_dates_tbl = step1_dt_calendar_dates_tbl)

# 2022 tubes that need distancxe adjusting
siteids <- c("175", "239", "242", "261", "405", "487", "502", "512", "567", "586", "593", "602", "608", "624", "652", "665", "667", "670") %>% as.integer()


con <- connect.access()

last_years_sites_tbl <- get.lastyears.sites(con, startDate)
aqms_tbl <- get.aqms(con)
gridconcs_da_tbl <- get.gridconcs.da.tubes(con = con,
                                       startDate = startDate,
                                       siteids = siteids,
                                       aqms_tbl = aqms_tbl)
write_csv(gridconcs_da_tbl, "data/gridconcs_da.csv")
no2_data <- get.no2.data.db(con, startDate, endDate)
pivoted_tubes_tbl <- pivot.tubes.monthly(no2_data)
step_2_tbl <- make.step.2.table(aqms_tbl, pivoted_tubes_tbl, last_years_sites_tbl)
step_2a_tbl <- get.background.data(no2_data)
coloc_divisor_tbl <- make.coloc.divisor.tbl(aqms_tbl)
data_cap_period_tbl <- make.data.cap.period.tbl(coloc_divisor_tbl,
                                                no2_data)
annual_tube_data_4years_tbl <- get.annual.tube.data.4yrs.tbl(con, startDate)
count_tubes_tbl <- get.count.tubes.tbl(con)

bias_no2_tube_tbl <- make.bias.no2.tube.tbl(aqms_tbl, no2_data)



dbDisconnect(con)
con <- connect.envista()

contin_4yrs_tbl <- get.aq.data.all(startDate = as.Date(startDate) - years(4), endDate = endDate)
contin_bias_data_tbl <- make.contin.bias.data.tbl(contin_4yrs_tbl, bias_no2_tube_tbl)

final_tbl <- get.final.tbl()

bias_site_list <- make.bias.site.list(bias_no2_tube_tbl, contin_bias_data_tbl, aqms_tbl)

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

table_list <- make.table.list(step_2_tbl,
                              step_2a_tbl,
                              table_a1,
                              table_a2,
                              table_a3,
                              table_a4,
                              table_a5,
                              table_a6,
                              table_a7,
                              table_a8)


write.bias.spreadsheet(bias_site_list, startDate)

write.table.list(startDate, table_list = table_list)

dbDisconnect(con)

