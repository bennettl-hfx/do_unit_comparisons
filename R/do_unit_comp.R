#Liam Bennett, 2024-08-05

#library(devtools)
#devtools::install_github("dempsey-CMAR/docalcs", dependencies = TRUE)

library(docalcs)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(here)
library(lubridate)
library(qaqcmar)
library(sensorstrings)
library(plotly)


dat_raw <- ss_import_data(county = c("Halifax", "Lunenburg"))

dat <- dat_raw %>%
  select(-contains("flag")) %>%
  ss_pivot_longer() %>%
  distinct(
    county, station,
    deployment_range,
    sensor_type, sensor_serial_number, sensor_depth_at_low_tide_m,
    variable
  ) %>%
  filter(variable == "dissolved_oxygen_percent_saturation" | variable == "dissolved_oxygen_uncorrected_mg_per_l")

# Birchy Head, 2022-11-03 ------------------------------------------------------

depl_bh <- dat_raw %>%
  select(-contains("flag")) %>%
  filter(county == "Lunenburg" & 
           station == "Birchy Head" & 
           deployment_range == "2022-Nov-03 to 2023-May-15" & 
           sensor_depth_at_low_tide_m == 5)

depl_bh_dat_sat <- depl_bh %>%
  select(-salinity_psu) %>%
  filter(!is.na(dissolved_oxygen_percent_saturation))


depl_bh_dat_mgl <- depl_bh %>%
  select(-salinity_psu) %>%
  filter(!is.na(dissolved_oxygen_uncorrected_mg_per_l)) %>%
  do_salinity_correction(sal = 35) %>%
  do_solubility(p_atm = 1, return_factors = TRUE) %>%
  mutate(
    dissolved_oxygen_mg_per_l = F_s * dissolved_oxygen_uncorrected_mg_per_l,
    dissolved_oxygen_percent_saturation = 100 * dissolved_oxygen_mg_per_l / C_p
  ) %>%
  select(
    sensor_type:F_s, temperature, C_p,
    dissolved_oxygen_mg_per_l, dissolved_oxygen_percent_saturation
  )


p_1 <- ggplot(depl_bh_dat_sat, aes(x=timestamp_utc, y=dissolved_oxygen_percent_saturation)) + 
  geom_line()

p_1

p_2 <- ggplot(depl_bh_dat_mgl, aes(x=timestamp_utc, y=dissolved_oxygen_percent_saturation)) + 
  geom_line()

p_2 # dip is at line 16337 (passes DOTables sanity check)

ggplotly(p_2)


## Plotting both on one plot ------------------------------------

bh_mgl <- depl_bh_dat_mgl %>%
  select("sensor_type", "timestamp_utc", "dissolved_oxygen_percent_saturation", "dissolved_oxygen_uncorrected_mg_per_l")

bh_sat <- depl_bh_dat_sat %>%
  select("sensor_type", "timestamp_utc", "dissolved_oxygen_percent_saturation", "dissolved_oxygen_uncorrected_mg_per_l")

new_bh_dat <- rbind(bh_mgl, bh_sat) %>%
  arrange(timestamp_utc)

#Note how lines diverge only when biofouling starts (according to qc report all observations from just before April on are suspect)
bh <- ggplot(new_bh_dat, aes(x=timestamp_utc, y=dissolved_oxygen_percent_saturation)) +
  geom_line(aes(color = sensor_type)) +
  ggtitle("Birchy Head, 2022-11-03")

bh
ggplotly(bh)

## Same timestamps ------------------------------------------------------------

bh_both_dat <- cbind(bh_mgl, rep(NA, length(bh_mgl$timestamp_utc))) %>%
  rename(dissolved_oxygen_percent_saturation_converted = dissolved_oxygen_percent_saturation) %>%
  data.frame()

names(bh_both_dat)[5] <- "dissolved_oxygen_percent_saturation_avg"

minutes <- minutes(7)
seconds <- seconds(30)

for(i in 1:length(bh_both_dat$timestamp_utc)){
  curr_time <- bh_both_dat$timestamp_utc[i]
  curr_time_lb <- curr_time - minutes - seconds
  curr_time_ub <- curr_time + minutes + seconds
  
  temp_dat <- bh_sat %>%
    filter(timestamp_utc >= curr_time_lb & timestamp_utc < curr_time_ub)
  
  #sat_avg = mean(temp_dat$dissolved_oxygen_percent_saturation)
  
  bh_both_dat$dissolved_oxygen_percent_saturation_avg[i] <- mean(temp_dat$dissolved_oxygen_percent_saturation)
}

bh_2 <- ggplot(bh_both_dat, aes(timestamp_utc)) +
  geom_line(aes(y=dissolved_oxygen_percent_saturation_converted), color="black") +
  geom_line(aes(y=dissolved_oxygen_percent_saturation_avg), color="red")

bh_2
ggplotly(bh_2)

bh_both_dat <- bh_both_dat %>%
  mutate(abs_difference = abs(dissolved_oxygen_percent_saturation_avg - dissolved_oxygen_percent_saturation_converted))

mean(bh_both_dat$abs_difference)
median(bh_both_dat$abs_difference)
max(bh_both_dat$abs_difference)
min(bh_both_dat$abs_difference)

hist(bh_both_dat$abs_difference)

# Flat Island, 2022-11-19 --------------------------------------------

depl_fi <- dat_raw %>%
  select(-contains("flag")) %>%
  filter(county == "Lunenburg" & 
           station == "Flat Island" & 
           deployment_range == "2022-Nov-19 to 2023-Jan-10" & 
           sensor_depth_at_low_tide_m == 5)

depl_fi_dat_sat <- depl_fi %>%
  select(-salinity_psu) %>%
  filter(!is.na(dissolved_oxygen_percent_saturation))


depl_fi_dat_mgl <- depl_fi %>%
  select(-salinity_psu) %>%
  filter(!is.na(dissolved_oxygen_uncorrected_mg_per_l)) %>%
  do_salinity_correction(sal = 35) %>%
  do_solubility(p_atm = 1, return_factors = TRUE) %>%
  mutate(
    dissolved_oxygen_mg_per_l = F_s * dissolved_oxygen_uncorrected_mg_per_l,
    dissolved_oxygen_percent_saturation = 100 * dissolved_oxygen_mg_per_l / C_p
  ) %>%
  select(
    sensor_type:F_s, temperature, C_p,
    dissolved_oxygen_mg_per_l, dissolved_oxygen_percent_saturation
  )

fi_1 <- ggplot(depl_fi_dat_sat, aes(x=timestamp_utc, y=dissolved_oxygen_percent_saturation)) + 
  geom_line()

fi_1

fi_2 <- ggplot(depl_fi_dat_mgl, aes(x=timestamp_utc, y=dissolved_oxygen_percent_saturation)) + 
  geom_line()

fi_2 


fi_mgl <- depl_fi_dat_mgl %>%
  select("sensor_type", "timestamp_utc", "dissolved_oxygen_percent_saturation", "dissolved_oxygen_uncorrected_mg_per_l")

fi_sat <- depl_fi_dat_sat %>%
  select("sensor_type", "timestamp_utc", "dissolved_oxygen_percent_saturation", "dissolved_oxygen_uncorrected_mg_per_l")

new_fi_dat <- rbind(fi_mgl, fi_sat) %>%
  arrange(timestamp_utc)

fi <- ggplot(new_fi_dat, aes(x=timestamp_utc, y=dissolved_oxygen_percent_saturation)) +
  geom_line(aes(color = sensor_type)) +
  ggtitle("Flat Island, 2022-11-19")

fi
ggplotly(fi)


## Same timestamps ------------------------------------------------------------

fi_both_dat <- cbind(fi_mgl, rep(NA, length(fi_mgl$timestamp_utc))) %>%
  rename(dissolved_oxygen_percent_saturation_converted = dissolved_oxygen_percent_saturation) %>%
  data.frame()

names(fi_both_dat)[5] <- "dissolved_oxygen_percent_saturation_avg"

#minutes <- minutes(7)
#seconds <- seconds(30)

for(i in 1:length(fi_both_dat$timestamp_utc)){
  curr_time <- fi_both_dat$timestamp_utc[i]
  curr_time_lb <- curr_time - minutes - seconds
  curr_time_ub <- curr_time + minutes + seconds
  
  temp_dat <- fi_sat %>%
    filter(timestamp_utc >= curr_time_lb & timestamp_utc < curr_time_ub)
  
  #sat_avg = mean(temp_dat$dissolved_oxygen_percent_saturation)
  
  fi_both_dat$dissolved_oxygen_percent_saturation_avg[i] <- mean(temp_dat$dissolved_oxygen_percent_saturation)
}

fi_4 <- ggplot(fi_both_dat, aes(timestamp_utc)) +
  geom_line(aes(y=dissolved_oxygen_percent_saturation_converted), color="black") +
  geom_line(aes(y=dissolved_oxygen_percent_saturation_avg), color="red")

fi_4
ggplotly(fi_4)

fi_both_dat <- fi_both_dat %>%
  mutate(abs_difference = abs(dissolved_oxygen_percent_saturation_avg - dissolved_oxygen_percent_saturation_converted))

mean(fi_both_dat$abs_difference)
median(fi_both_dat$abs_difference)
max(fi_both_dat$abs_difference)
min(fi_both_dat$abs_difference)

hist(fi_both_dat$abs_difference)


# Little Rafuse Island, 2021-11-21 --------------------------------------------

depl_lri <- dat_raw %>%
  select(-contains("flag")) %>%
  filter(county == "Lunenburg" & 
           station == "Little Rafuse Island" & 
           deployment_range == "2021-Nov-21 to 2022-May-24" & 
           sensor_depth_at_low_tide_m == 5)

depl_lri_dat_sat <- depl_lri %>%
  select(-salinity_psu) %>%
  filter(!is.na(dissolved_oxygen_percent_saturation))


depl_lri_dat_mgl <- depl_lri %>%
  select(-salinity_psu) %>%
  filter(!is.na(dissolved_oxygen_uncorrected_mg_per_l)) %>%
  do_salinity_correction(sal = 35) %>%
  do_solubility(p_atm = 1, return_factors = TRUE) %>%
  mutate(
    dissolved_oxygen_mg_per_l = F_s * dissolved_oxygen_uncorrected_mg_per_l,
    dissolved_oxygen_percent_saturation = 100 * dissolved_oxygen_mg_per_l / C_p
  ) %>%
  select(
    sensor_type:F_s, temperature, C_p,
    dissolved_oxygen_mg_per_l, dissolved_oxygen_percent_saturation
  )

lri_1 <- ggplot(depl_lri_dat_sat, aes(x=timestamp_utc, y=dissolved_oxygen_percent_saturation)) + 
  geom_line()

lri_1

lri_2 <- ggplot(depl_lri_dat_mgl, aes(x=timestamp_utc, y=dissolved_oxygen_percent_saturation)) + 
  geom_line()

lri_2 


lri_mgl <- depl_lri_dat_mgl %>%
  select("sensor_type", "timestamp_utc", "dissolved_oxygen_percent_saturation", "dissolved_oxygen_uncorrected_mg_per_l")

lri_sat <- depl_lri_dat_sat %>%
  select("sensor_type", "timestamp_utc", "dissolved_oxygen_percent_saturation", "dissolved_oxygen_uncorrected_mg_per_l")

new_lri_dat <- rbind(lri_mgl, lri_sat) %>%
  arrange(timestamp_utc)

lri <- ggplot(new_lri_dat, aes(x=timestamp_utc, y=dissolved_oxygen_percent_saturation)) +
  geom_line(aes(color = sensor_type)) +
  ggtitle("Little Rafuse Island, 2021-11-21")

lri
ggplotly(lri)


## Same timestamps ------------------------------------------------------------

lri_both_dat <- cbind(lri_mgl, rep(NA, length(lri_mgl$timestamp_utc))) %>%
  rename(dissolved_oxygen_percent_saturation_converted = dissolved_oxygen_percent_saturation) %>%
  data.frame()

names(lri_both_dat)[5] <- "dissolved_oxygen_percent_saturation_avg"

#minutes <- minutes(7)
#seconds <- seconds(30)

for(i in 1:length(lri_both_dat$timestamp_utc)){
  curr_time <- lri_both_dat$timestamp_utc[i]
  curr_time_lb <- curr_time - minutes - seconds
  curr_time_ub <- curr_time + minutes + seconds
  
  temp_dat <- lri_sat %>%
    filter(timestamp_utc >= curr_time_lb & timestamp_utc < curr_time_ub)
  
  #sat_avg = mean(temp_dat$dissolved_oxygen_percent_saturation)
  
  lri_both_dat$dissolved_oxygen_percent_saturation_avg[i] <- mean(temp_dat$dissolved_oxygen_percent_saturation)
}

lri_4 <- ggplot(lri_both_dat, aes(timestamp_utc)) +
  geom_line(aes(y=dissolved_oxygen_percent_saturation_converted), color="black") +
  geom_line(aes(y=dissolved_oxygen_percent_saturation_avg), color="red")

lri_4
ggplotly(lri_4)

lri_both_dat <- lri_both_dat %>%
  mutate(abs_difference = abs(dissolved_oxygen_percent_saturation_avg - dissolved_oxygen_percent_saturation_converted))

mean(lri_both_dat$abs_difference)
median(lri_both_dat$abs_difference)
max(lri_both_dat$abs_difference)
min(lri_both_dat$abs_difference)

hist(lri_both_dat$abs_difference)


# Little Rafuse Island, 2022-11-19 --------------------------------------------

depl_lri_2 <- dat_raw %>%
  select(-contains("flag")) %>%
  filter(county == "Lunenburg" & 
           station == "Little Rafuse Island" & 
           deployment_range == "2022-Nov-19 to 2023-May-09" & 
           sensor_depth_at_low_tide_m == 5)

depl_lri_2_dat_sat <- depl_lri_2 %>%
  select(-salinity_psu) %>%
  filter(!is.na(dissolved_oxygen_percent_saturation))


depl_lri_2_dat_mgl <- depl_lri_2 %>%
  select(-salinity_psu) %>%
  filter(!is.na(dissolved_oxygen_uncorrected_mg_per_l)) %>%
  do_salinity_correction(sal = 35) %>%
  do_solubility(p_atm = 1, return_factors = TRUE) %>%
  mutate(
    dissolved_oxygen_mg_per_l = F_s * dissolved_oxygen_uncorrected_mg_per_l,
    dissolved_oxygen_percent_saturation = 100 * dissolved_oxygen_mg_per_l / C_p
  ) %>%
  select(
    sensor_type:F_s, temperature, C_p,
    dissolved_oxygen_mg_per_l, dissolved_oxygen_percent_saturation
  )

lri_2_1 <- ggplot(depl_lri_2_dat_sat, aes(x=timestamp_utc, y=dissolved_oxygen_percent_saturation)) + 
  geom_line()

lri_2_1

lri_2_2 <- ggplot(depl_lri_2_dat_mgl, aes(x=timestamp_utc, y=dissolved_oxygen_percent_saturation)) + 
  geom_line()

lri_2_2 


lri_2_mgl <- depl_lri_2_dat_mgl %>%
  select("sensor_type", "timestamp_utc", "dissolved_oxygen_percent_saturation", "dissolved_oxygen_uncorrected_mg_per_l")

lri_2_sat <- depl_lri_2_dat_sat %>%
  select("sensor_type", "timestamp_utc", "dissolved_oxygen_percent_saturation", "dissolved_oxygen_uncorrected_mg_per_l")

new_lri_2_dat <- rbind(lri_2_mgl, lri_2_sat) %>%
  arrange(timestamp_utc)

lri_2 <- ggplot(new_lri_2_dat, aes(x=timestamp_utc, y=dissolved_oxygen_percent_saturation)) +
  geom_line(aes(color = sensor_type)) +
  ggtitle("Little Rafuse Island, 2022-11-19")

lri_2
ggplotly(lri_2)


## Same timestamps ------------------------------------------------------------

lri_2_both_dat <- cbind(lri_2_mgl, rep(NA, length(lri_2_mgl$timestamp_utc))) %>%
  rename(dissolved_oxygen_percent_saturation_converted = dissolved_oxygen_percent_saturation) %>%
  data.frame()

names(lri_2_both_dat)[5] <- "dissolved_oxygen_percent_saturation_avg"

#minutes <- minutes(7)
#seconds <- seconds(30)

for(i in 1:length(lri_2_both_dat$timestamp_utc)){
  curr_time <- lri_2_both_dat$timestamp_utc[i]
  curr_time_lb <- curr_time - minutes - seconds
  curr_time_ub <- curr_time + minutes + seconds
  
  temp_dat <- lri_2_sat %>%
    filter(timestamp_utc >= curr_time_lb & timestamp_utc < curr_time_ub)
  
  #sat_avg = mean(temp_dat$dissolved_oxygen_percent_saturation)
  
  lri_2_both_dat$dissolved_oxygen_percent_saturation_avg[i] <- mean(temp_dat$dissolved_oxygen_percent_saturation)
}

lri_2_4 <- ggplot(lri_2_both_dat, aes(timestamp_utc)) +
  geom_line(aes(y=dissolved_oxygen_percent_saturation_converted), color="black") +
  geom_line(aes(y=dissolved_oxygen_percent_saturation_avg), color="red")

lri_2_4
ggplotly(lri_2_4)

lri_2_both_dat <- lri_2_both_dat %>%
  mutate(abs_difference = abs(dissolved_oxygen_percent_saturation_avg - dissolved_oxygen_percent_saturation_converted))

mean(lri_2_both_dat$abs_difference)
median(lri_2_both_dat$abs_difference)
max(lri_2_both_dat$abs_difference)
min(lri_2_both_dat$abs_difference)

hist(lri_2_both_dat$abs_difference)

# Checking Linearity -----------------------------------------------------------

# Plotting Converted units versus avg (observed) - looking for a linear plot
ggplot(bh_both_dat, aes(x = dissolved_oxygen_percent_saturation_avg, y = dissolved_oxygen_percent_saturation_converted)) +
  geom_line() +
  ggtitle("Birchy Head, 2022-11-03")

ggplot(fi_both_dat, aes(x = dissolved_oxygen_percent_saturation_avg, y = dissolved_oxygen_percent_saturation_converted)) +
  geom_line() +
  ggtitle("Flat Island, 2022-11-19")

ggplot(lri_both_dat, aes(x = dissolved_oxygen_percent_saturation_avg, y = dissolved_oxygen_percent_saturation_converted)) +
  geom_line() +
  ggtitle("Little Rafuse Island, 2021-11-21")

ggplot(lri_2_both_dat, aes(x = dissolved_oxygen_percent_saturation_avg, y = dissolved_oxygen_percent_saturation_converted)) +
  geom_line() +
  ggtitle("Little Rafuse Island, 2022-11-19")

# Getting correlation coefficient for the three deployments
cor(x = bh_both_dat$dissolved_oxygen_percent_saturation_avg, y = bh_both_dat$dissolved_oxygen_percent_saturation_converted)
# 0.9346567

cor(x = fi_both_dat$dissolved_oxygen_percent_saturation_avg, y = fi_both_dat$dissolved_oxygen_percent_saturation_converted)
# 0.5908978

cor(x = lri_both_dat$dissolved_oxygen_percent_saturation_avg, y = lri_both_dat$dissolved_oxygen_percent_saturation_converted)
# 0.2749025

cor(x = lri_2_both_dat$dissolved_oxygen_percent_saturation_avg, y = lri_2_both_dat$dissolved_oxygen_percent_saturation_converted)
# 0.6685581
