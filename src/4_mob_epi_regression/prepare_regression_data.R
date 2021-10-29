# Combine regression data in a single `.rds` object for analysis.
library(sf)
library(tidyverse)

adm <- st_read("data/geo/processed/adm_mun_adjusted.geojson") %>% 
  st_drop_geometry() %>% 
  as_tibble()

rt <- read_csv("data/rt_estimates/processed/rt_regional.csv") %>% 
  filter(variable == "R") %>% 
  filter(date <= as.Date ("2020-09-01")) %>% 
  dplyr::select(date, name2, median) %>% 
  rename(R = median)

name2_include <- rt %>% 
  group_by(name2) %>% 
  summarise(min_date = min(date)) %>% 
  filter(min_date <= as.Date("2020-03-30")) %>% 
  pull(name2)

rt <- rt %>% 
  filter(name2 %in% name2_include)

vf <- read_csv("data/mobility/vodafone/processed/vodafone_mobility.csv") %>% 
  rename(mob = vf) %>% 
  left_join(adm, by = c("pcod2")) %>% 
  dplyr::select(date, name2, mob) %>% 
  filter(name2 %in% name2_include) %>% 
  rename(mob_vf = mob) %>% 
  filter(date <= as.Date ("2020-09-01"))

goog <- read_csv("data/mobility/google/processed/google_mobility.csv") %>% 
  dplyr::select(date, name2, mob) %>% 
  filter(name2 %in% name2_include) %>% 
  rename(mob_goog = mob) %>% 
  filter(date <= as.Date ("2020-09-01")) %>% 
  mutate(mob_goog = mob_goog * -1)

mob <- vf %>% 
  left_join(goog, by = c("date", "name2"))

npi <- read_csv("data/interventions/processed/si_index.csv") %>% 
  mutate(npi_si_custom = npi_si_custom * -1,
         npi_si_ox = npi_si_ox * -1)

event_dates <- c(
  "New Year's Day" = as.Date("2020-01-01"),
  "Constitution Day" = as.Date("2020-01-07"),
  "Independence Day" = as.Date("2020-03-06"),
  "Good Friday" = as.Date("2020-04-10"),
  "Easter Monday" = as.Date("2020-04-13"),  
  "Worker's Day" = as.Date("2020-05-01"),  
  "Eid al-Fitr" = as.Date("2020-05-25"),  
  "Eid al-Adha" = as.Date("2020-07-31"),  
  "Founder's Day" = as.Date("2020-08-04"),
  "Kwame Nkrumah Memorial Day" = as.Date("2020-08-04"),
  "Election Day" = as.Date("2020-12-07"),
  "Christmas" = as.Date("2020-12-25"),
  "Boxing Day" = as.Date("2020-12-26")
)

event_dates <- tibble(date = unique(rt$date)) %>% 
  mutate(events = as.integer(date %in% event_dates))

r_data <- list()

r_data[["rt"]] <- rt
r_data[["mob"]] <- mob
r_data[["npi"]] <- npi
r_data[["events"]] <- event_dates

write_rds(r_data, 
          "data/regression/processed/regression_data.rds")

