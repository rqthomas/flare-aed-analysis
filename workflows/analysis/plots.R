library(tidyverse)
df <- arrow::open_dataset("scores/parquet/") |>
  collect()


df |>
  filter(variable == "HAB_fish1") |>
  select(mean) |> tail()

df |>
  filter(variable == "HAB_fish1",
         datetime > reference_datetime) |>
  mutate(modelxref = paste(model_id,reference_datetime)) |>
  ggplot(aes(x = datetime, group = modelxref)) +
  geom_ribbon(aes(ymax = quantile90, ymin = quantile10, fill = model_id), alpha = 0.5) +
  geom_line(aes(y = mean, color = model_id)) +
  geom_point(aes(y = observation), color = "black") +
  #geom_vline(aes(xintercept = reference_datetime)) +
  #facet_wrap(~reference_datetime) +
  theme_bw()


df |>
  filter(variable == "chla",
         depth == 1.5,
         datetime > reference_datetime) |>
  mutate(modelxref = paste(model_id,reference_datetime)) |>
  ggplot(aes(x = datetime, group = modelxref)) +
  geom_ribbon(aes(ymax = quantile90, ymin = quantile10, fill = model_id), alpha = 0.5) +
  geom_line(aes(y = mean, color = model_id)) +
  geom_point(aes(y = observation), color = "black") +
  #geom_vline(aes(xintercept = reference_datetime)) +
  #facet_wrap(~reference_datetime) +
  theme_bw()


df |>
  filter(variable == "chla",
         depth == 1.5,
         datetime <= reference_datetime) |>
  mutate(modelxref = paste(model_id,reference_datetime)) |>
  ggplot(aes(x = datetime, group = modelxref)) +
  geom_ribbon(aes(ymax = quantile90, ymin = quantile10, fill = model_id), alpha = 0.5) +
  geom_line(aes(y = mean, color = model_id)) +
  geom_point(aes(y = observation), color = "black") +
  #geom_vline(aes(xintercept = reference_datetime)) +
  #facet_wrap(~reference_datetime) +
  theme_bw()


df |>
  filter(variable %in% c("PHY_diatom_fPho", "PHY_cyano_fPho", "PHY_green_fPho", "PHY_diatom_fNit", "PHY_cyano_fNit", "PHY_green_fNit") ,
         depth == 1.5,
         datetime <= reference_datetime) |>
  mutate(nut = str_split_fixed(variable, "_", n = 3)[,3],
         phy = str_split_fixed(variable, "_", n = 3)[, 2]) |>
  mutate(modelxref = paste(model_id,reference_datetime)) |>
  ggplot(aes(x = datetime, group = modelxref)) +
  geom_ribbon(aes(ymax = quantile90, ymin = quantile10, fill = model_id), alpha = 0.5) +
  geom_line(aes(y = mean, color = model_id)) +
  geom_point(aes(y = observation), color = "black") +
  #geom_vline(aes(xintercept = reference_datetime)) +
  facet_grid(rows = vars(phy), cols = vars(nut)) +
  theme_bw()

###
df |>
  filter(variable %in% c("chla", "temperature", "oxygen", "fdom"),
         depth == 1.5,
         datetime > reference_datetime) |>
  mutate(sq_error = (observation - mean)^2,
         horizon = datetime - reference_datetime) |>
  group_by(model_id, horizon, variable) |>
  summarise(rmse = sqrt(mean(sq_error, na.rm = TRUE))) |>
  ggplot(aes(x = horizon, y = rmse, color = model_id)) +
  geom_line()+
  facet_wrap(~variable, scales = "free_y")

##
df |>
  filter(variable %in% c("chla", "temperature", "oxygen", "fdom"),
         depth == 1.5,
         datetime > reference_datetime) |>
  mutate(horizon = datetime - reference_datetime) |>
  group_by(model_id, horizon, variable) |>
  summarise(crps = mean(crps, na.rm = TRUE)) |>
  ggplot(aes(x = horizon, y = crps, color = model_id)) +
  geom_line() +
  facet_wrap(~variable, scales = "free_y")



