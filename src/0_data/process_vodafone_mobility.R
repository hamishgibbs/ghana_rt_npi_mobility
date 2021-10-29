# Script to combine Vodafone mobility into a single file

library(tidyverse)
library(sf)

a2 <- read_csv(here::here("data/geo/processed/admin2_mun_lu.csv"))

mob_files <- list.files("mobility_external",
                        ".csv", full.names = T)

trips_files <- mob_files[lapply(mob_files, stringr::str_detect, pattern = "trips_per_day_admin2") %>%
  unlist()]

subs_files <- mob_files[lapply(mob_files, stringr::str_detect, pattern = "total_subscribers_per_day_admin2") %>%
                     unlist()]

read_file_date <- function(fn){

  file_date <- stringr::str_extract(fn, "\\.(.*?)\\.csv")
  file_date <- stringr::str_replace(file_date, ".", "")
  file_date <- stringr::str_replace(file_date, ".csv", "")

  data <- read_csv(fn, col_types = cols()) %>%
    mutate(date = file_date)

  return(data)

}

print("Reading Trips Files...")
trips <- lapply(trips_files, read_file_date) %>%
  do.call(rbind, .)

# Assign to adjusted municipal geometry
print("Adjusting Trips Geometry...")
trips_mun <- trips %>%
  left_join(a2 %>% select(pcod2, pcod2_mun), by = c("pcod_from" = "pcod2")) %>%
  rename(pcod2_from = pcod_from, pcod2_mun_from = pcod2_mun) %>%
  left_join(a2 %>% select(pcod2, pcod2_mun), by = c("pcod_to" = "pcod2")) %>%
  rename(pcod2_to = pcod_to, pcod2_mun_to = pcod2_mun)

# Filter out trips now within combined districts
trips_mun <- trips_mun %>%
  filter(pcod2_mun_from != pcod2_mun_to) %>% 
  select(-pcod2_from, -pcod2_to) %>% 
  rename(pcod2_from = pcod2_mun_from, pcod2_to = pcod2_mun_to)

print("Writing Adjusted Trips.")
write_csv(trips_mun, here::here("data/mobility/vodafone/trips_combined.csv"))

print("Reading Subs Files...")
subs <- lapply(subs_files, read_file_date) %>%
  do.call(rbind, .)

# Assign & combine to adjusted municipal geometry
print("Adjusting Subs Geometry...")
subs_mun <- subs %>%
  left_join(a2 %>% select(pcod2, pcod2_mun), by = c("pcod" = "pcod2")) %>%
  group_by(date, pcod2_mun) %>%
  summarise(value = sum(value, na.rm = T), .groups = "drop") %>% 
  rename(pcod2 = pcod2_mun)

print("Writing Adjusted Subs.")
write_csv(subs_mun, here::here("data/mobility/vodafone/subs_combined.csv"))
