# Create a movement indicator from Vodafone trip and subscriber counts
library(tidyverse)

trips <- read_csv(here::here("data/mobility/vodafone/trips_combined.csv"))
subs <- read_csv(here::here("data/mobility/vodafone/subs_combined.csv"))

# Normalise # of outbound trips by total subscribers
trips_norm <- trips %>%
  group_by(date, pcod2_from) %>%
  summarise(outbound_trips = sum(value, na.rm = T), .groups = 'drop') %>%
  left_join(subs, by = c("date", "pcod2_from" = "pcod2")) %>%
  rename(subs = value) %>%
  mutate(year = lubridate::year(date),
         outbound_trips_norm = outbound_trips / subs) %>%
  mutate(wday = lubridate::wday(date))


# Calculate 2019 baseline
baseline <- trips_norm %>%
  filter(year == 2019) %>%
  group_by(pcod2_from, wday) %>%
  summarise(baseline_outbound_trips_norm = median(outbound_trips_norm, na.rm = T),
            .groups = 'drop')

trips_norm_bl <- trips_norm %>%
  filter(year == 2020) %>%
  filter(date <= as.Date("2020-09-01")) %>% 
  left_join(baseline, by = c("pcod2_from", "wday")) %>%
  mutate(change_from_baseline = ((outbound_trips_norm - baseline_outbound_trips_norm) / baseline_outbound_trips_norm) * 100)

# Figure of different metrics
if(interactive()){
  trips_norm_pivot <- trips_norm_bl %>%
    filter(pcod2_from == "fid005") %>%
    select(date, outbound_trips, subs, outbound_trips_norm, change_from_baseline) %>%
    pivot_longer(c(outbound_trips, subs, outbound_trips_norm, change_from_baseline))
  
  make_variable_factor <- function(v){
    return(factor(v,
                  levels = c("outbound_trips", "subs", "outbound_trips_norm", "change_from_baseline"),
                  labels = c("Outbound Trips", "Total Subscribers",
                             "Outbound Trips Normalised (Outbound Trips / Total Subscribers)",
                             "Change from Baseline ((Outbound Trips Normalised - Baseline) / Baseline) * 100"))
    )
  }
  
  trips_norm_pivot$name <- make_variable_factor(trips_norm_pivot$name)
  
  p_norm_example <- trips_norm_pivot %>%
    ggplot() +
    geom_path(aes(x = date, y = value), size = 0.4) +
    facet_wrap(~name, scales = "free_y") +
    theme_classic() +
    labs(y = "Value", x = NULL)
  
  
  ggutils::ggsave_png_pdf(p_norm_example,
                          here::here("output/figs/vf_mob_variable_example.png"),
                          10, 5)
  
  p_bl_region <- trips_norm_bl %>%
    filter(pcod2_from %in% unique(pcod2_from)[1:6]) %>%
    ggplot() +
    geom_hline(aes(yintercept = 0), size = 0.2, color = "black", linetype = "dashed") +
    geom_path(aes(x = date, y = change_from_baseline)) +
    facet_wrap(~pcod2_from) +
    theme_classic() +
    labs(y = "Value", x = NULL)
  
  ggutils::ggsave_png_pdf(p_bl_region,
                          here::here("output/figs/vf_mob_region.png"),
                          10, 10)
  
}

vf_mob_ind <- trips_norm_bl %>%
  select(date, pcod2_from, change_from_baseline)

# Interpolate data for missing dates (linear interpolation)
completed_dates <- seq.Date(min(trips_norm_bl$date), max(trips_norm_bl$date), by = "day")

vf_data <- lapply(unique(trips_norm_bl$pcod2_from), function(x){return(tibble(date = completed_dates, pcod2 = x))})

vf_data <- vf_data %>%
  do.call(rbind, .) %>%
  full_join(trips_norm_bl, by = c("date", "pcod2" = "pcod2_from")) %>%
  select(date, pcod2, change_from_baseline) %>%
  rename(vf = change_from_baseline) %>%
  group_by(pcod2) %>%
  mutate(vf_int = zoo::na.approx(vf, na.rm=FALSE)) %>%
  ungroup() %>% 
  select(-vf) %>% 
  rename(vf = vf_int)

write_csv(vf_data, here::here("data/mobility/vodafone/processed/vodafone_mobility.csv"))

# Number of dates missing data relative to entire time series
if(interactive()){
  missing_dates <- completed_dates[!completed_dates %in% vf_mob_ind$date]
  
  round((length(missing_dates) / length(completed_dates)) * 100, 1)
  
  # Plot of the missing dates that were interpolated
  p_missing_vf <- vf_data %>% 
    filter(pcod2 == "AMA",
           date <= as.Date("2020-09-01")) %>% 
    ggplot() + 
    geom_path(aes(x = date, y = vf), size = 0.3) + 
    geom_vline(xintercept = missing_dates, color = "red", alpha = 0.4) + 
    theme_classic() + 
    labs(x = NULL, y = "Change from Baseline (%)")
  
  ggutils::ggsave_png_pdf(p_missing_vf, 
                          "output/figs/vf_missingness.png", 
                          10, 4)  
}
