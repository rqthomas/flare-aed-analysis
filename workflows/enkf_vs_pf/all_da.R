library(tidyverse)
library(lubridate)
set.seed(201)

config_set_name <- "enkf_vs_pf"
site <- "fcre"
configure_run_file <- "configure_aed_run.yml"
config_flare_file <- "configure_flare_glm_aed_3groups_manual.yml"
starting_index <- 1
use_s3 <- FALSE

Sys.setenv('GLM_PATH'='GLM3r')

options(future.globals.maxSize = 891289600)

lake_directory <- here::here()

walk(list.files(file.path(lake_directory, "R"), full.names = TRUE), source)

experiments <- c("enkf", "pf")

num_forecasts <- 52/4
days_between_forecasts <- 52*7 / num_forecasts
forecast_horizon <- 0
starting_date <- as_date("2021-10-01")
#starting_date <- as_date("2021-12-01")
second_date <- as_date("2021-12-31")


all_dates <- seq.Date(starting_date,second_date + days(days_between_forecasts * num_forecasts), by = 1)

potential_date_list <- list(enkf = all_dates,
                            pf = all_dates,
                            no_da = starting_date)

date_list <- potential_date_list[which(names(potential_date_list) %in% experiments)]

models <- names(date_list)

start_dates <- as_date(rep(NA, num_forecasts + 1))
end_dates <- as_date(rep(NA, num_forecasts + 1))
start_dates[1] <- starting_date
end_dates[1] <- second_date
if(num_forecasts > 0){
for(i in 2:(num_forecasts+1)){
  start_dates[i] <- as_date(end_dates[i-1])
  end_dates[i] <- start_dates[i] + days(days_between_forecasts)
}
}

sims <- expand.grid(paste0(start_dates,"_",end_dates,"_", forecast_horizon), models)

names(sims) <- c("date","model")

sims$start_dates <- stringr::str_split_fixed(sims$date, "_", 3)[,1]
sims$end_dates <- stringr::str_split_fixed(sims$date, "_", 3)[,2]
sims$horizon <- stringr::str_split_fixed(sims$date, "_", 3)[,3]

sims <- sims |>
  mutate(model = as.character(model)) |>
  select(-date) |>
  distinct_all() |>
  arrange(start_dates)

sims$horizon[1:length(models)] <- 0

#sims <- sims[1:2, ]

for(i in starting_index:nrow(sims)){

  message(paste0("index: ", i))
  message(paste0("     Running model: ", sims$model[i], " "))

  model <- sims$model[i]
  sim_names <- paste0(config_set_name, "_" ,model)

  sim_names <- paste0("year_da", "_" ,model)


  config <- FLAREr::set_up_simulation(configure_run_file, lake_directory, config_set_name = config_set_name, sim_name = sim_names, clean_start = TRUE)

  yml <- yaml::read_yaml(file.path(lake_directory, "configuration", config_set_name, configure_run_file))
  yml$sim_name <- sim_names
  yaml::write_yaml(yml, file.path(lake_directory, "configuration", config_set_name, configure_run_file))

  yml <- yaml::read_yaml(file.path(lake_directory, "configuration", config_set_name, config_flare_file))
  if(sims$model[i] %in% c("enkf", "pf")){
    yml$da_setup$da_method<-  sims$model[i]
    yml$da_setup$use_obs_constraint <- TRUE
  }else if(i %in% 1:6 & sims$model[i] == "no_da"){
    yml$da_setup$use_obs_constraint <- FALSE
  }
  yaml::write_yaml(yml, file.path(lake_directory, "configuration", config_set_name, config_flare_file))

  run_config <- yaml::read_yaml(file.path(lake_directory, "configuration", config_set_name, configure_run_file))
  run_config$configure_flare <- config_flare_file
  run_config$sim_name <- sim_names
  run_config$start_datetime <- as.character(paste0(sims$start_dates[i], " 00:00:00"))
  run_config$forecast_start_datetime <- as.character(paste0(sims$end_dates[i], " 00:00:00"))
  run_config$forecast_horizon <- as.numeric(sims$horizon[i])
  run_config$configure_flare <- config_flare_file
  if(i <= length(models)){
    config$run_config$restart_file <- NA
  }else{
    run_config$restart_file <- paste0(config$location$site_id, "-", lubridate::as_date(run_config$start_datetime), "-", sim_names, ".nc")
    if(!file.exists(file.path(config$file_path$restart_directory, paste0(config$location$site_id, "-", lubridate::as_date(run_config$start_datetime), "-", sim_names, ".nc")) )){
      warning(paste0("restart file: ", run_config$restart_file, " doesn't exist"))
    }
  }

  yaml::write_yaml(run_config, file = file.path(lake_directory, "restart", site, sim_names, configure_run_file))

  config <- FLAREr::set_up_simulation(configure_run_file, lake_directory, config_set_name = config_set_name, sim_name = sim_names, clean_start = FALSE)

  config <- FLAREr:::get_restart_file(config, lake_directory)

  file.copy(file.path(config$file_path$configuration_directory,"FCR_SSS_inflow_2013_2021_20220413_allfractions_2DOCpools.csv"),
            file.path(config$file_path$execute_directory,"FCR_SSS_inflow_2013_2021_20220413_allfractions_2DOCpools.csv"))

  pars_config <- readr::read_csv(file.path(config$file_path$configuration_directory, config$model_settings$par_config_file), col_types = readr::cols())
  obs_config <- readr::read_csv(file.path(config$file_path$configuration_directory, config$model_settings$obs_config_file), col_types = readr::cols())
  states_config <- readr::read_csv(file.path(config$file_path$configuration_directory, config$model_settings$states_config_file), col_types = readr::cols())

  # Inflows
  source(file.path(lake_directory, "workflows", config_set_name, "make_flow_drivers.R"))
  met_start_datetime <- lubridate::as_datetime(config$run_config$start_datetime)
  met_forecast_start_datetime <- lubridate::as_datetime(config$run_config$forecast_start_datetime)

  met_out <- FLAREr:::create_met_files(config, lake_directory = lake_directory, met_forecast_start_datetime, met_start_datetime)


  inflow_outflow_files <- FLAREr:::create_inflow_outflow_files(config, config_set_name, lake_directory)


  if(config$model_settings$model_name == "glm_aed"){
    inflow_outflow_files$inflow_file_names <- cbind(inflow_outflow_files$inflow_file_names, rep(file.path(config$file_path$execute_directory,"FCR_SSS_inflow_2013_2021_20220413_allfractions_2DOCpools.csv"), length(inflow_outflow_files$inflow_file_name)))
  }

  #Create observation matrix
  obs <- FLAREr:::create_obs_matrix(cleaned_observations_file_long = file.path(config$file_path$qaqc_data_directory,paste0(config$location$site_id, "-targets-insitu.csv")),
                                    obs_config = obs_config,
                                    config)

  obs_non_vertical <- FLAREr:::create_obs_non_vertical(cleaned_observations_file_long = file.path(config$file_path$qaqc_data_directory,paste0(config$location$site_id, "-targets-insitu.csv")),
                                                       obs_config,
                                                       start_datetime = config$run_config$start_datetime,
                                                       end_datetime = config$run_config$end_datetime,
                                                       forecast_start_datetime = config$run_config$forecast_start_datetime,
                                                       forecast_horizon =  config$run_config$forecast_horizon)


  states_config <- FLAREr:::generate_states_to_obs_mapping(states_config, obs_config)

  model_sd <- FLAREr:::initiate_model_error(config, states_config)

  init <- FLAREr:::generate_initial_conditions(states_config,
                                               obs_config,
                                               pars_config,
                                               obs,
                                               config,
                                               obs_non_vertical = obs_non_vertical)

  # states_init = init$states
  # pars_init = init$pars
  # aux_states_init = init$aux_states_init
  # obs = obs
  # obs_sd = obs_config$obs_sd
  # model_sd = model_sd
  # working_directory = config$file_path$execute_directory
  # met_file_names = met_out$filenames
  # inflow_file_names = inflow_outflow_files$inflow_file_names[,1]
  # outflow_file_names = inflow_outflow_files$outflow_file_names
  # config = config
  # pars_config = pars_config
  # states_config = states_config
  # obs_config = obs_config
  # da_method = config$da_setup$da_method
  # par_fit_method = config$da_setup$par_fit_method
  # obs_secchi = obs_non_vertical$obs_secchi
  # obs_depth = obs_non_vertical$obs_depth


  da_forecast_output <- FLAREr:::run_da_forecast(states_init = init$states,
                                                 pars_init = init$pars,
                                                 aux_states_init = init$aux_states_init,
                                                 obs = obs,
                                                 obs_sd = obs_config$obs_sd,
                                                 model_sd = model_sd,
                                                 working_directory = config$file_path$execute_directory,
                                                 met_file_names = met_out$filenames,
                                                 inflow_file_names = inflow_outflow_files$inflow_file_names[,1],
                                                 outflow_file_names = inflow_outflow_files$outflow_file_names,
                                                 config = config,
                                                 pars_config = pars_config,
                                                 states_config = states_config,
                                                 obs_config = obs_config,
                                                 da_method = config$da_setup$da_method,
                                                 par_fit_method = config$da_setup$par_fit_method,
                                                 obs_secchi = obs_non_vertical$obs_secchi,
                                                 obs_depth = obs_non_vertical$obs_depth)

  # Save forecast

  saved_file <- FLAREr:::write_restart(da_forecast_output = da_forecast_output,
                                       forecast_output_directory = config$file_path$restart_directory,
                                       use_short_filename = TRUE)

  forecast_df <- FLAREr:::write_forecast(da_forecast_output = da_forecast_output,
                                         use_s3 = use_s3,
                                         bucket = config$s3$forecasts_parquet$bucket,
                                         endpoint = config$s3$forecasts_parquet$endpoint,
                                         local_directory = file.path(lake_directory, "forecasts/parquet"))



  targets_df <- read_csv(file.path(config$file_path$qaqc_data_directory,paste0(config$location$site_id, "-targets-insitu.csv")),show_col_types = FALSE)

  FLAREr:::plotting_general(forecast_df, targets_df, file_name = paste0(tools::file_path_sans_ext(basename(saved_file)),".pdf") , plots_directory = config$file_path$plots_directory)

  generate_forecast_score_arrow(targets_df = targets_df,
                                forecast_df = forecast_df,
                                use_s3 = FALSE,
                                bucket = NULL,
                                endpoint = NULL,
                                local_directory = file.path(lake_directory, "scores/parquet"),
                                variable_types = c("state","parameter","diagnostic"))

}
