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


dat_do <- dat_raw %>%
  inner_join(dat,
             by = join_by(
               county, station, deployment_range, sensor_type, sensor_serial_number,
               sensor_depth_at_low_tide_m)
  )


dat_test <- dat_raw %>%
  filter(
    station == "Shut-In Island",
    timestamp_utc >= as_datetime("2018-04-25 18:24:00")
  ) %>%
  qc_pivot_longer(qc_tests = "qc") %>%
  qc_filter_summary_flags() %>%
  select(sensor_type:sensor_depth_at_low_tide_m, variable, value) %>%
  rename(Date = timestamp_utc) %>%
  filter(variable %in% c(
    "temperature_degree_c",
    "dissolved_oxygen_percent_saturation",
    "dissolved_oxygen_uncorrected_mg_per_l"
  )) %>%
  ss_pivot_wider()


# correct and convert do --------------------------------------------------

###
dat_other <- dat %>%
    filter(is.na(dissolved_oxygen_uncorrected_mg_per_l))
####


dat_do <- dat %>%
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

#
# ggplot(dat_do, aes(Date, dissolved_oxygen_percent_saturation)) +
#   geom_point()


dat_out <- dat_do %>%
  select(
    Date,
    sensor_type, sensor_serial_number, sensor_depth_at_low_tide_m,
    temperature_degree_c = temperature,
    dissolved_oxygen_percent_saturation
  ) %>%
  bind_rows(dat_other) %>%
  select(-dissolved_oxygen_uncorrected_mg_per_l) %>%
  ss_pivot_longer() %>%
  ss_convert_depth_to_ordered_factor()

#ss_ggplot_variables(dat_out)

# plot and export --------------------------------------------------------

p <- ss_ggplot_variables(
  dat_out,
  axis_label_newline = FALSE,
  point_size = 0.05,
  legend_position = "bottom"
) +
  scale_x_datetime(
    "Date",
    limits = c(as_datetime("2018-01-01"), max(dat_out$Date))
  ) +
  theme(
    text = element_text(size = 6),
    axis.title.y = element_blank(),
    strip.text = element_text(size = 6),
    legend.text = element_text(size = 6),
    plot.margin = unit(c(0.5, 0.5, 0, 0.5), 'lines'),
    #legend.position = "bottom",
    # legend.box = "horizontal",
    #legend.justification.bottom = "left",
    legend.box.spacing = unit(0, "lines")
  )

ggsave(
  p,
  filename = here("figures/figure3_wq_facet_8.62.png"),
  device = "png",
  width = 8.8, height = 8.62, units = "cm",
  dpi = 600
)

