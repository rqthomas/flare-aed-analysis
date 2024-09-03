extract_CTD <- function(fname,
                        input_file_tz,
                        focal_depths,
                        config){

  d_ctd <- readr::read_csv(fname, show_col_types = FALSE) %>%
    dplyr::mutate(Date = lubridate::force_tz(DateTime, tzone = input_file_tz)) %>%
    dplyr::filter(Reservoir == "FCR" & Site == "50") %>%
    dplyr::select(Date, Depth_m, Temp_C, DO_mgL, Chla_ugL) %>%
    dplyr::rename("time" = Date,
           "depth" = Depth_m,
           "temperature" = Temp_C,
           "oxygen" = DO_mgL,
           "chla" = Chla_ugL) %>%
    dplyr::mutate(oxygen = oxygen * 1000/32,
           chla = config$ctd_2_exo_sensor_chla[1] + config$ctd_2_exo_sensor_chla[2] * chla,
           oxygen = config$ctd_2_exo_sensor_do[1] + config$ctd_2_exo_sensor_do[2] * oxygen) %>%
    tidyr::pivot_longer(cols = c("temperature", "oxygen", "chla"), names_to = "variable", values_to = "observed") %>%
    dplyr::mutate(method = "ctd") %>%
    dplyr::select(time , depth, observed, variable, method) %>%
    dplyr::mutate(time = lubridate::as_datetime(time, tz = "UTC"))

  if(!is.na(focal_depths)){
    d_ctd <- d_ctd %>% dplyr::filter(depth %in% focal_depths)
  }

  return(d_ctd)
}
