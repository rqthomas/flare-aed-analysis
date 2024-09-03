library(tidyverse)
library(lubridate)
set.seed(201)

sim_name <- "test"
config_set_name <- "analysis"
configure_run_file <- "configure_aed_run.yml"

Sys.setenv('GLM_PATH'='GLM3r')
# Write the interpolated data as the historal file
Sys.setenv(AWS_ACCESS_KEY_ID=Sys.getenv("OSN_KEY"),
           AWS_SECRET_ACCESS_KEY=Sys.getenv("OSN_SECRET"))

options(future.globals.maxSize = 891289600)

lake_directory <- here::here()

use_s3 <- FALSE

walk(list.files(file.path(lake_directory, "R"), full.names = TRUE), source)

yml <- yaml::read_yaml(file.path(lake_directory, "configuration", config_set_name, configure_run_file))
yml$sim_name <- sim_name
yaml::write_yaml(yml, file.path(lake_directory, "configuration", config_set_name, configure_run_file))

site <- "fcre"

# Set up configurations for the data processing

config_obs <- yaml::read_yaml(file.path(lake_directory,"configuration",config_set_name, "observation_processing.yml"))
config_obs$file_path$data_directory <- file.path(lake_directory, "data_raw")
dir.create(config_obs$file_path$data_directory, recursive = TRUE, showWarnings = FALSE)
config_obs$file_path$targets_directory <- file.path(lake_directory, "targets")

#' Clone or pull from data repositories

FLAREr:::get_git_repo(lake_directory,
                      directory = config_obs$realtime_insitu_location,
                      git_repo = "https://github.com/FLARE-forecast/FCRE-data.git")

FLAREr:::get_git_repo(lake_directory,
                      directory = config_obs$realtime_met_station_location,
                      git_repo = "https://github.com/FLARE-forecast/FCRE-data.git")

FLAREr:::get_git_repo(lake_directory,
                      directory = config_obs$realtime_inflow_data_location,
                      git_repo = "https://github.com/FLARE-forecast/FCRE-data.git")

get_edi_file(edi_https = "https://pasta.lternet.edu/package/data/eml/edi/389/7/02d36541de9088f2dd99d79dc3a7a853",
             file = config_obs$met_raw_obs_fname[2],
             lake_directory)

get_edi_file(edi_https = "https://pasta.lternet.edu/package/data/eml/edi/271/7/71e6b946b751aa1b966ab5653b01077f",
             file = config_obs$insitu_obs_fname[2],
             lake_directory)

get_edi_file(edi_https = "https://pasta.lternet.edu/package/data/eml/edi/198/11/81f396b3e910d3359907b7264e689052",
             file = config_obs$secchi_fname,
             lake_directory)

get_edi_file(edi_https = "https://pasta.lternet.edu/package/data/eml/edi/200/13/27ceda6bc7fdec2e7d79a6e4fe16ffdf",
             file = config_obs$ctd_fname,
             lake_directory)

get_edi_file(edi_https = "https://pasta.lternet.edu/package/data/eml/edi/199/11/509f39850b6f95628d10889d66885b76",
             file = config_obs$nutrients_fname,
             lake_directory)


get_edi_file(edi_https = "https://pasta.lternet.edu/package/data/eml/edi/202/9/c065ff822e73c747f378efe47f5af12b",
             file = config_obs$inflow_raw_file1[2],
             lake_directory)

get_edi_file(edi_https = "https://pasta.lternet.edu/package/data/eml/edi/542/1/791ec9ca0f1cb9361fa6a03fae8dfc95",
             file = "silica_master_df.csv",
             lake_directory)

get_edi_file(edi_https = "https://pasta.lternet.edu/package/data/eml/edi/551/7/38d72673295864956cccd6bbba99a1a3",
             file = "Dissolved_CO2_CH4_Virginia_Reservoirs.csv",
             lake_directory)

#' Clean up observed meteorology

cleaned_met_file <- met_data_bind(realtime_file = file.path(config_obs$file_path$data_directory, config_obs$met_raw_obs_fname[1]),
                                  qaqc_file = file.path(config_obs$file_path$data_directory, config_obs$met_raw_obs_fname[2]),
                                  cleaned_met_file = file.path(config_obs$file_path$targets_directory, config_obs$site_id,paste0("observed-met_",config_obs$site_id,".csv")),
                                  input_file_tz = "EST",
                                  nldas = NULL,
                                  site_id = config_obs$site_id)

#' Clean up observed inflow

cleaned_inflow_file <- inflow_data_combine(realtime_file = file.path(config_obs$file_path$data_directory, config_obs$inflow_raw_file1[1]),
                                           qaqc_file = file.path(config_obs$file_path$data_directory, config_obs$inflow_raw_file1[2]),
                                           nutrients_file = file.path(config_obs$file_path$data_directory, config_obs$nutrients_fname),
                                           silica_file = file.path(config_obs$file_path$data_directory,  config_obs$silica_fname),
                                           co2_ch4 = file.path(config_obs$file_path$data_directory, config_obs$ch4_fname),
                                           cleaned_inflow_file = file.path(config_obs$file_path$targets_directory, config_obs$site_id, paste0(config_obs$site_id,"-targets-inflow.csv")),
                                           input_file_tz = 'EST',
                                           site_id = config_obs$site_id)
#read_csv(cleaned_inflow_file) |>
#  mutate(observation = ifelse(variable == "PHS_frp", observation * 100, observation)) |>
#  write_csv(cleaned_inflow_file)

#' Clean up observed insitu measurements

cleaned_insitu_file <- in_situ_qaqc_csv(insitu_obs_fname = file.path(config_obs$file_path$data_directory,config_obs$insitu_obs_fname),
                                        data_location = config_obs$file_path$data_directory,
                                        maintenance_file = file.path(config_obs$file_path$data_directory,config_obs$maintenance_file),
                                        ctd_fname = file.path(config_obs$file_path$data_directory, config_obs$ctd_fname),
                                        nutrients_fname =  file.path(config_obs$file_path$data_directory, config_obs$nutrients_fname),
                                        secchi_fname = file.path(config_obs$file_path$data_directory, config_obs$secchi_fname),
                                        ch4_fname = file.path(config_obs$file_path$data_directory, config_obs$ch4_fname),
                                        cleaned_insitu_file = file.path(config_obs$file_path$targets_directory, config_obs$site_id, paste0(config_obs$site_id,"-targets-insitu.csv")),
                                        lake_name_code = config_obs$site_id,
                                        config = config_obs)

d <- read_csv(cleaned_insitu_file, show_col_types = FALSE) |>
  filter((variable %in% c("NH4", "NO3NO2", "SRP", "TN", "TP") & depth == 1.5) |
           variable %in% c("temperature", "oxygen", "secchi", "chla", "fdom", "depth")) |>
  #variable != "depth" & datetime == as_datetime("2021-01-01 00:00:00") | variable == "depth") |>
  #datetime == as_datetime("2021-01-01 00:00:00")) |>
  write_csv(cleaned_insitu_file)


## Stage met data
s3 <- arrow::s3_bucket("bio230121-bucket01/flare/drivers/met/gefs-v12/stage3/site_id=fcre/", endpoint_override = "renc.osn.xsede.org", anonymous = TRUE)
arrow::open_dataset(s3) |>
  arrow::write_dataset(path = file.path(lake_directory, "drivers/met/gefs-v12/stage3/site_id=fcre/"))

s3 <- arrow::s3_bucket("bio230121-bucket01/flare/drivers/met/gefs-v12/stage2", endpoint_override = "renc.osn.xsede.org", anonymous = TRUE)
arrow::open_dataset(s3) |>
  filter(site_id == "fcre",
         reference_datetime >= "2022-01-01" & reference_datetime <= "2022-12-31") |>
  arrow::write_dataset(path = file.path(lake_directory, "drivers/met/gefs-v12/stage2"), partitioning = c("reference_datetime", "site_id"))
