hist_interp_inflow <- interpolate_targets(targets = paste0(config$location$site_id,"-targets-inflow.csv"),
                                          lake_directory = lake_directory,
                                          targets_dir = 'targets',
                                          site_id = config$location$site_id,
                                          #variables = c('FLOW', 'SALT', 'TEMP'),
                                          variables <- c("time", "FLOW", "TEMP", "SALT",
                                                         'OXY_oxy',
                                                         'CAR_dic',
                                                         'CAR_ch4',
                                                         'SIL_rsi',
                                                         'NIT_amm',
                                                         'NIT_nit',
                                                         'PHS_frp',
                                                         'OGM_doc',
                                                         'OGM_docr',
                                                         'OGM_poc',
                                                         'OGM_don',
                                                         'OGM_donr',
                                                         'OGM_pon',
                                                         'OGM_dop',
                                                         'OGM_dopr',
                                                         'OGM_pop',
                                                         'PHY_cyano',
                                                         'PHY_green',
                                                         'PHY_diatom'),
                                          depth = F,
                                          method = 'linear')


hist_interp_inflow <- hist_interp_inflow |>
  mutate(flow_number = 1,
         parameter = 1) |>
  rename(prediction = observation)

arrow::write_dataset(hist_interp_inflow, path = file.path(lake_directory, "drivers/inflow/historical/model_id=historical_interp/site_id=fcre"))


# generate a simple "forecast" that has ensemble members
forecast_date <- config$run_config$forecast_start_datetime
future_inflow <- hist_interp_inflow |>
  filter(datetime > forecast_date,
         datetime <= as_date(forecast_date) + config$run_config$forecast_horizon) |>
  mutate(parameter = 1,
         flow_number = 1) |>
  #reframe(prediction = rnorm(10, mean = observation, sd = 1),
  #        parameter = 1:10,
  #        .by = c(site_id, datetime, variable)) |>
  mutate(reference_datetime = as_date(forecast_date))

arrow::write_dataset(future_inflow,
                     file.path(lake_directory, "drivers/inflow/future/model_id=historical_interp"),
                     partitioning = c('reference_datetime', 'site_id'))


#==========================================#

# outflows
hist_interp_outflow <- interpolate_targets(targets = paste0(config$location$site_id,"-targets-inflow.csv"),
                                           lake_directory = lake_directory,
                                           targets_dir = 'targets',
                                           site_id = config$location$site_id,
                                           #variables = c('FLOW', 'SALT', 'TEMP'),
                                           variables <- c("time", "FLOW"),
                                           depth = F,
                                           method = 'linear')

hist_interp_outflow <- hist_interp_outflow |>
  mutate(flow_number = 1,
         parameter = 1) |>
  rename(prediction = observation)

arrow::write_dataset(hist_interp_outflow, path = file.path(lake_directory, "drivers/outflow/historical/model_id=historical_interp/site_id=fcre"))


# generate a simple "forecast" that has ensemble members
future_outflow <- hist_interp_outflow |>
  filter(datetime > forecast_date,
         datetime <= as_date(forecast_date) + config$run_config$forecast_horizon) |>
  mutate(parameter = 1,
         flow_number = 1) |>
  #reframe(prediction = rnorm(10, mean = observation, sd = 1),
  #        parameter = 1:10,
  #        .by = c(site_id, datetime, variable)) |>
  mutate(reference_datetime = as_date(forecast_date))


arrow::write_dataset(future_outflow,
                     file.path(lake_directory, "drivers/outflow/future/model_id=historical_interp"),
                     partitioning = c('reference_datetime', 'site_id'))
