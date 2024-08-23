# Liam Bennett, 2024-08-12
# Final updates: 2024-08-16


#Load Libraries
library(docalcs)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(here)
library(lubridate)
library(qaqcmar)
library(sensorstrings)
library(plotly)

# Import Data
dat_raw <- ss_import_data(county = c("Halifax", "Lunenburg"))

# Identifying deployments with different DO sensors at same depth
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

# All deployments with different DO sensors at the same depth
key_depl <- inner_join(dat, dat, join_by(station, deployment_range, sensor_depth_at_low_tide_m), relationship = "many-to-many") %>%
  filter(variable.x != variable.y & sensor_type.x == "aquameasure")
# Kay depl has only 4 rows (at time of writing code)


# Birchy Head, 2022-11-03 ------------------------------------------------------

# Filter down to deployment data at desired depth
depl_bh <- dat_raw %>%
  #select(-contains("flag")) %>%
  filter(station == "Birchy Head" & 
           deployment_range == "2022-Nov-03 to 2023-May-15" & 
           sensor_depth_at_low_tide_m == 5 &
           (qc_flag_dissolved_oxygen_percent_saturation == 1 | qc_flag_dissolved_oxygen_uncorrected_mg_per_l == 1)) #Filter down to observations with a flag of 1

# Separate two DO units (and apply conversion to mg/L)
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

# Plot %sat from both datasets

p_1 <- ggplot(depl_bh_dat_sat, aes(x=timestamp_utc, y=dissolved_oxygen_percent_saturation)) + 
  geom_point()

p_1

p_2 <- ggplot(depl_bh_dat_mgl, aes(x=timestamp_utc, y=dissolved_oxygen_percent_saturation)) + 
  geom_point()

p_2 

ggplotly(p_2)


## Plotting both on one plot ------------------------------------
# Combine all observations into a single dataframe and arrange by timestamp (when plotting, color by sensor type to separate)

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
# %sat has more observations, thus a direct comparison cannot be made. We must average those values in bins around the timestamp of mg/L observations

# Initialize a destination dataframe for the averaged values
bh_both_dat <- data.frame(bh_mgl, dissolved_oxygen_percent_saturation_avg = rep(NA, length(bh_mgl$timestamp_utc))) %>%
  rename(dissolved_oxygen_percent_saturation_converted = dissolved_oxygen_percent_saturation)

# Setting bin radius
minutes <- minutes(7)
seconds <- seconds(30)

# Loop through new dataframe to enter average value in the required column
for(i in 1:length(bh_both_dat$timestamp_utc)){
  curr_time <- bh_both_dat$timestamp_utc[i]
  curr_time_lb <- curr_time - minutes - seconds
  curr_time_ub <- curr_time + minutes + seconds
  
  temp_dat <- bh_sat %>%
    filter(timestamp_utc >= curr_time_lb & timestamp_utc < curr_time_ub)
  
  bh_both_dat$dissolved_oxygen_percent_saturation_avg[i] <- mean(temp_dat$dissolved_oxygen_percent_saturation)
}

# To plot, we must remove missing values in the average column (When we remove the flag >1 observations are removed from bh_mgl, so we can only plot timestamps that have observations of both the observed and average)
bh_both_dat <- bh_both_dat %>%
  filter(!is.na(dissolved_oxygen_percent_saturation_avg)) %>%
  mutate(abs_difference = abs(dissolved_oxygen_percent_saturation_avg - dissolved_oxygen_percent_saturation_converted))

# Plot both lines together
bh_2 <- ggplot(bh_both_dat, aes(timestamp_utc)) +
  geom_line(aes(y=dissolved_oxygen_percent_saturation_converted), color="black") +
  geom_line(aes(y=dissolved_oxygen_percent_saturation_avg), color="red")

bh_2
ggplotly(bh_2)

# Print summary statistics 
mean(bh_both_dat$abs_difference) 
median(bh_both_dat$abs_difference)
max(bh_both_dat$abs_difference)
min(bh_both_dat$abs_difference)

hist(bh_both_dat$abs_difference)

# Plot converted value vs avg value (looking for linearity)
ggplot(bh_both_dat, aes(x = dissolved_oxygen_percent_saturation_avg, y = dissolved_oxygen_percent_saturation_converted)) +
  geom_line() +
  ggtitle("Birchy Head, 2022-11-03")

# Find the correlation coefficient (close to 1 is linear)
cor(x = bh_both_dat$dissolved_oxygen_percent_saturation_avg, y = bh_both_dat$dissolved_oxygen_percent_saturation_converted)
# 0.9691272


# Flat Island, 2022-11-19 --------------------------------------------

depl_fi <- dat_raw %>%
  #select(-contains("flag")) %>%
  filter(county == "Lunenburg" & 
           station == "Flat Island" & 
           deployment_range == "2022-Nov-19 to 2023-Jan-10" & 
           sensor_depth_at_low_tide_m == 5 &
           (qc_flag_dissolved_oxygen_percent_saturation == 1 | qc_flag_dissolved_oxygen_uncorrected_mg_per_l == 1))

# *NOTE*
# No observations from above chunk!
# This entire deployment is flagged as FAIL (4)
# The remainder of the script for this deployment is omitted


# Little Rafuse Island, 2021-11-21 --------------------------------------------

# Filter down to deployment data at desired depth
depl_lri <- dat_raw %>%
  filter(station == "Little Rafuse Island" & 
           deployment_range == "2021-Nov-21 to 2022-May-24" & 
           sensor_depth_at_low_tide_m == 5 &
           (qc_flag_dissolved_oxygen_percent_saturation == 1 | qc_flag_dissolved_oxygen_uncorrected_mg_per_l == 1)) #Filter down to observations with a flag of 1

# Separate two DO units (and apply conversion to mg/L)
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

# Plot %sat from both datasets
lri_1 <- ggplot(depl_lri_dat_sat, aes(x=timestamp_utc, y=dissolved_oxygen_percent_saturation)) + 
  geom_line()

lri_1

lri_2 <- ggplot(depl_lri_dat_mgl, aes(x=timestamp_utc, y=dissolved_oxygen_percent_saturation)) + 
  geom_line()

lri_2 

# Combine all observations into a single dataframe and arrange by timestamp (when plotting, color by sensor type to separate)
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
# %sat has more observations, thus a direct comparison cannot be made. We must average those values in bins around the timestamp of mg/L observations

# Initialize a destination dataframe for the averaged values

lri_both_dat <- data.frame(lri_mgl, dissolved_oxygen_percent_saturation_avg = rep(NA, length(lri_mgl$timestamp_utc))) %>%
  rename(dissolved_oxygen_percent_saturation_converted = dissolved_oxygen_percent_saturation)

# Minutes and seconds previous initialized
#minutes <- minutes(7)
#seconds <- seconds(30)

# Loop through new dataframe to enter average value in the required column
for(i in 1:length(lri_both_dat$timestamp_utc)){
  curr_time <- lri_both_dat$timestamp_utc[i]
  curr_time_lb <- curr_time - minutes - seconds
  curr_time_ub <- curr_time + minutes + seconds
  
  temp_dat <- lri_sat %>%
    filter(timestamp_utc >= curr_time_lb & timestamp_utc < curr_time_ub)
  
  lri_both_dat$dissolved_oxygen_percent_saturation_avg[i] <- mean(temp_dat$dissolved_oxygen_percent_saturation)
}

# To plot, we must remove missing values in the average column (When we remove the flag >1 observations are removed from bh_mgl, so we can only plot timestamps that have observations of both the observed and average)
lri_both_dat <- lri_both_dat %>%
  filter(!is.na(dissolved_oxygen_percent_saturation_avg)) %>%
  mutate(abs_difference = abs(dissolved_oxygen_percent_saturation_avg - dissolved_oxygen_percent_saturation_converted))

# Plot both lines together
lri_4 <- ggplot(lri_both_dat, aes(timestamp_utc)) +
  geom_line(aes(y=dissolved_oxygen_percent_saturation_converted), color="black") +
  geom_line(aes(y=dissolved_oxygen_percent_saturation_avg), color="red")

lri_4
ggplotly(lri_4)

# Print summary statistics
mean(lri_both_dat$abs_difference)
median(lri_both_dat$abs_difference)
max(lri_both_dat$abs_difference)
min(lri_both_dat$abs_difference)

hist(lri_both_dat$abs_difference)

# Plot converted value vs avg value (looking for linearity)
ggplot(lri_both_dat, aes(x = dissolved_oxygen_percent_saturation_avg, y = dissolved_oxygen_percent_saturation_converted)) +
  geom_line() +
  ggtitle("Little Rafuse Island, 2021-11-21")

# Find the correlation coefficient (close to 1 is linear)
cor(x = lri_both_dat$dissolved_oxygen_percent_saturation_avg, y = lri_both_dat$dissolved_oxygen_percent_saturation_converted)
# 0.9535064

# Little Rafuse Island, 2022-11-19 --------------------------------------------

# Filter down to deployment data at desired depth
depl_lri_2 <- dat_raw %>%
  filter(station == "Little Rafuse Island" & 
           deployment_range == "2022-Nov-19 to 2023-May-09" & 
           sensor_depth_at_low_tide_m == 5 &
           (qc_flag_dissolved_oxygen_percent_saturation == 1 | qc_flag_dissolved_oxygen_uncorrected_mg_per_l == 1))

# Separate two DO units (and apply conversion to mg/L)
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

# Plot %sat from both datasets
lri_2_1 <- ggplot(depl_lri_2_dat_sat, aes(x=timestamp_utc, y=dissolved_oxygen_percent_saturation)) + 
  geom_line()

lri_2_1

lri_2_2 <- ggplot(depl_lri_2_dat_mgl, aes(x=timestamp_utc, y=dissolved_oxygen_percent_saturation)) + 
  geom_line()

lri_2_2 

# Combine all observations into a single dataframe and arrange by timestamp (when plotting, color by sensor type to separate)
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
# %sat has more observations, thus a direct comparison cannot be made. We must average those values in bins around the timestamp of mg/L observations

# Initialize a destination dataframe for the averaged values

lri_2_both_dat <- data.frame(lri_2_mgl, dissolved_oxygen_percent_saturation_avg = rep(NA, length(lri_2_mgl$timestamp_utc))) %>%
  rename(dissolved_oxygen_percent_saturation_converted = dissolved_oxygen_percent_saturation)

# Minutes and seconds previous initialized
#minutes <- minutes(7)
#seconds <- seconds(30)

# Loop through new dataframe to enter average value in the required column
for(i in 1:length(lri_2_both_dat$timestamp_utc)){
  curr_time <- lri_2_both_dat$timestamp_utc[i]
  curr_time_lb <- curr_time - minutes - seconds
  curr_time_ub <- curr_time + minutes + seconds
  
  temp_dat <- lri_2_sat %>%
    filter(timestamp_utc >= curr_time_lb & timestamp_utc < curr_time_ub)
  
  #sat_avg = mean(temp_dat$dissolved_oxygen_percent_saturation)
  
  lri_2_both_dat$dissolved_oxygen_percent_saturation_avg[i] <- mean(temp_dat$dissolved_oxygen_percent_saturation)
}

# To plot, we must remove missing values in the average column (When we remove the flag >1 observations are removed from bh_mgl, so we can only plot timestamps that have observations of both the observed and average)
lri_2_both_dat <- lri_2_both_dat %>%
  filter(!is.na(dissolved_oxygen_percent_saturation_avg)) %>%
  mutate(abs_difference = abs(dissolved_oxygen_percent_saturation_avg - dissolved_oxygen_percent_saturation_converted))

# Plot both lines together
lri_2_4 <- ggplot(lri_2_both_dat, aes(timestamp_utc)) +
  geom_line(aes(y=dissolved_oxygen_percent_saturation_converted), color="black") +
  geom_line(aes(y=dissolved_oxygen_percent_saturation_avg), color="red")

lri_2_4
ggplotly(lri_2_4)

# Print summary statistics
mean(lri_2_both_dat$abs_difference)
median(lri_2_both_dat$abs_difference)
max(lri_2_both_dat$abs_difference)
min(lri_2_both_dat$abs_difference)

hist(lri_2_both_dat$abs_difference)

# Plot converted value vs avg value (looking for linearity)
ggplot(lri_2_both_dat, aes(x = dissolved_oxygen_percent_saturation_avg, y = dissolved_oxygen_percent_saturation_converted)) +
  geom_line() +
  ggtitle("Little Rafuse Island, 2022-11-19")

# Find the correlation coefficient (close to 1 is linear)
cor(x = lri_2_both_dat$dissolved_oxygen_percent_saturation_avg, y = lri_2_both_dat$dissolved_oxygen_percent_saturation_converted)
# 0.5445049

# Update salinity function -----------------------------------------------------

update_salinity <- function(station_name, depl_range, salinity = 35){
  
  depl <- dat_raw %>%
    filter(station == station_name & 
           deployment_range == depl_range & 
           sensor_depth_at_low_tide_m == 5 &
           (qc_flag_dissolved_oxygen_percent_saturation == 1 | qc_flag_dissolved_oxygen_uncorrected_mg_per_l == 1))
  
  depl_dat_sat <- depl %>%
    select(-salinity_psu) %>%
    filter(!is.na(dissolved_oxygen_percent_saturation)) %>%
    select("sensor_type", "timestamp_utc", "dissolved_oxygen_percent_saturation", "dissolved_oxygen_uncorrected_mg_per_l")
  
  depl_dat_mgl <- depl %>%
    select(-salinity_psu) %>%
    filter(!is.na(dissolved_oxygen_uncorrected_mg_per_l)) %>%
    do_salinity_correction(sal = salinity) %>%
    do_solubility(p_atm = 1, return_factors = TRUE) %>%
    mutate(
      dissolved_oxygen_mg_per_l = F_s * dissolved_oxygen_uncorrected_mg_per_l,
      dissolved_oxygen_percent_saturation = 100 * dissolved_oxygen_mg_per_l / C_p
    ) %>%
    select(
      "sensor_type", "timestamp_utc", "dissolved_oxygen_percent_saturation", "dissolved_oxygen_uncorrected_mg_per_l"
    )
  
  both_dat <- data.frame(depl_dat_mgl, dissolved_oxygen_percent_saturation_avg = rep(NA, length(depl_dat_mgl$timestamp_utc))) %>%
    rename(dissolved_oxygen_percent_saturation_converted = dissolved_oxygen_percent_saturation)
  
  minutes <- minutes(7)
  seconds <- seconds(30)
  
  for(i in 1:length(both_dat$timestamp_utc)){
    curr_time <- both_dat$timestamp_utc[i]
    curr_time_lb <- curr_time - minutes - seconds
    curr_time_ub <- curr_time + minutes + seconds
    
    temp_dat <- depl_dat_sat %>%
      filter(timestamp_utc >= curr_time_lb & timestamp_utc < curr_time_ub)
    
    both_dat$dissolved_oxygen_percent_saturation_avg[i] <- mean(temp_dat$dissolved_oxygen_percent_saturation)
  }
  
  both_dat <- both_dat %>%
    filter(!is.na(dissolved_oxygen_percent_saturation_avg)) %>%
    mutate(abs_difference = abs(dissolved_oxygen_percent_saturation_avg - dissolved_oxygen_percent_saturation_converted))
  
  return(cor(x = both_dat$dissolved_oxygen_percent_saturation_avg, y = both_dat$dissolved_oxygen_percent_saturation_converted))
  
}

update_salinity("Birchy Head", "2022-Nov-03 to 2023-May-15", salinity = 35)
update_salinity("Birchy Head", "2022-Nov-03 to 2023-May-15", salinity = 34)
update_salinity("Birchy Head", "2022-Nov-03 to 2023-May-15", salinity = 33) # Gives the highest linearity between converted units vs non-converted
update_salinity("Birchy Head", "2022-Nov-03 to 2023-May-15", salinity = 32)
update_salinity("Birchy Head", "2022-Nov-03 to 2023-May-15", salinity = 31)
update_salinity("Birchy Head", "2022-Nov-03 to 2023-May-15", salinity = 30)

update_salinity("Little Rafuse Island", "2021-Nov-21 to 2022-May-24", salinity = 35)
update_salinity("Little Rafuse Island", "2021-Nov-21 to 2022-May-24", salinity = 34)
update_salinity("Little Rafuse Island", "2021-Nov-21 to 2022-May-24", salinity = 33)
update_salinity("Little Rafuse Island", "2021-Nov-21 to 2022-May-24", salinity = 32) # Gives the highest linearity between converted units vs non-converted
update_salinity("Little Rafuse Island", "2021-Nov-21 to 2022-May-24", salinity = 31)
update_salinity("Little Rafuse Island", "2021-Nov-21 to 2022-May-24", salinity = 30)

update_salinity("Little Rafuse Island", "2022-Nov-19 to 2023-May-09", salinity = 35)
update_salinity("Little Rafuse Island", "2022-Nov-19 to 2023-May-09", salinity = 34)
update_salinity("Little Rafuse Island", "2022-Nov-19 to 2023-May-09", salinity = 33)# Gives the highest linearity between converted units vs non-converted
update_salinity("Little Rafuse Island", "2022-Nov-19 to 2023-May-09", salinity = 32)
update_salinity("Little Rafuse Island", "2022-Nov-19 to 2023-May-09", salinity = 31)
update_salinity("Little Rafuse Island", "2022-Nov-19 to 2023-May-09", salinity = 30)

# Salinity Plot ----------------------------------------------------------------

salinity_plot_values <- data.frame(salinity = c(30:35), 
                                   birchy_head = rep(NA, 6), 
                                   little_rafuse_2021 = rep(NA, 6),
                                   little_rafuse_2022 = rep(NA, 6)) 

for(i in 1:6){
  salinity_plot_values$birchy_head[i] = update_salinity("Birchy Head", "2022-Nov-03 to 2023-May-15", salinity = (i+29))
  salinity_plot_values$little_rafuse_2021[i] = update_salinity("Little Rafuse Island", "2021-Nov-21 to 2022-May-24", salinity = (i+29))
  salinity_plot_values$little_rafuse_2022[i] = update_salinity("Little Rafuse Island", "2022-Nov-19 to 2023-May-09", salinity = (i+29))
}

ggplot(salinity_plot_values, aes(x = salinity)) +
  geom_line(aes(y=birchy_head, color="red")) +
  geom_line(aes(y=little_rafuse_2021, color="blue")) +
  geom_line(aes(y=little_rafuse_2022, color="black")) +
  ggtitle("Linearity of Converted Units vs Observed by salinity")
  

# Plot with 1:1 Line
ggplot(bh_both_dat, aes(x = dissolved_oxygen_percent_saturation_avg, y = dissolved_oxygen_percent_saturation_converted, color = timestamp_utc)) +
  geom_point(alpha=0.5) +
  geom_abline(slope = 1, intercept = 0) +
  geom_abline(slope = 1, intercept = 5, linetype = 'dashed') +
  geom_abline(slope = 1, intercept = -5, linetype = 'dashed') +
  ggtitle("Dissolved Oxygen Converted units versus Observed units", subtitle = "Birchy Head, 2022-11-03") +
  xlab("Observed") +
  ylab("Converted") +
  #guides(color = guide_legend(title = "Observation Date")) +
  labs(color = "Observation Date")
  theme_light()

x = cor(x = bh_both_dat$dissolved_oxygen_percent_saturation_avg, y = bh_both_dat$dissolved_oxygen_percent_saturation_converted)
x^2
