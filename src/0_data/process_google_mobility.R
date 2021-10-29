# Convert Google mobility data to known districts
library(tidyverse)
library(sf)

# Google mobility is referenced to custom (Google) administrative polygons
# Need to combine these with known administrative reference
location_lookup <- list(
  "ChIJx9baqM1a1w8RNtKT0DDy11Q" = "Ghana",
  "ChIJ6fNExKqb3w8RGqR8UDG1n5Q" = "Accra Metropolitan Area",
  "ChIJ05lJ3faW2w8RUO6U-21oRA8" = "Kumasi Metropolitan Area"
)

get_region <- function(place_id, location_lookup){

  return(location_lookup[[place_id]])

}

get_place_id <- function(region, location_lookup){

  location_lookup2 <- as.list(names(location_lookup))
  names(location_lookup2) <- unlist(location_lookup)

  return(location_lookup2[[region]])

}

# Create a look up between Google place IDs and Admin Districts
mob <- read_csv("data/mobility/google/Global_Mobility_Report_Ghana.csv")

gha_mob <- mob %>%
  filter(country_region == "Ghana")

gha_mob$region <- lapply(gha_mob$place_id, get_region, location_lookup) %>% unlist()

# plot google mobility in all settings
if (interactive()){
  
  p_settings <- gha_mob %>% 
    filter(! region %in% c("Ghana")) %>% 
    select(-c(country_region_code, country_region, sub_region_1, sub_region_2, metro_area, iso_3166_2_code, census_fips_code, place_id)) %>% 
    pivot_longer(!c(date, region), names_to = "setting", values_to = "mob") %>% 
    filter(date <= as.Date("2020-09-01")) %>% 
    mutate(setting = gsub("_percent_change_from_baseline", "", setting),
           setting = gsub("_", " ", setting),
           setting = stringr::str_to_title(setting)) %>%
    ggplot() + 
    geom_path(aes(x = date, y = mob, color = setting), size = 0.3) + 
    geom_hline(aes(yintercept = 0), color = "black", size = 0.3, linetype="dashed") + 
    facet_wrap(~region) + 
    theme_classic() + 
    labs(y = "% Change from Baseline", x = NULL, color = NULL)
  
  ggutils::ggsave_png_pdf(p_settings, 
                          "output/figs/google_mob_settings.png",
                          10, 5)
  
}

google_regions <- st_read("data/geo/google_mobility_areas.kml")

google_regions$place_id <- lapply(google_regions$Name, get_place_id, location_lookup) %>% unlist()

a2 <- st_read("data/geo/processed/adm_mun_adjusted.geojson") %>%
  mutate(area = as.numeric(units::set_units(st_area(geometry), "km^2")))

a2_poly <- a2 %>% group_by(pcod2) %>% group_split()

find_intersect <- function(x, y){
  return(st_intersects(x, y, sparse = F)[1,1])
}

ama_int <- st_intersection(a2, google_regions %>% filter(Name == "Accra Metropolitan Area")) %>% 
  select(place_id, Name, pcod1, name1, pcod2, name2)

kma_int <- st_intersection(a2, google_regions %>% filter(Name == "Kumasi Metropolitan Area")) %>% 
  select(place_id, Name, pcod1, name1, pcod2, name2)

total_intersection <- rbind(ama_int, kma_int)

# DEV: Sanity plot - this will go wrong with sf 1.0.0 for some reason
# "Go wrong" means districts will be mixed up and out of order. This 
# seems like a legitimate bug with sf 1.0.0
if(interactive()){
  plot(total_intersection[, "Name"]) 
}

max_overlap <- total_intersection %>%
  mutate(name2 = as.character(name2)) %>% 
  mutate(area = as.numeric(units::set_units(st_area(geometry), "km^2"))) %>%
  left_join(a2 %>% st_drop_geometry(), by = c("pcod1", "name1", "pcod2", "name2")) %>%
  mutate(area_thresh = area.y * 0.5,
         max_overlap = area.x > area_thresh) %>%
  filter(max_overlap)

# Another sanity plot (see above) - districts should be filtered and not mixed up
if(interactive()){
  plot(max_overlap[, "Name"])
}

total_intersection_a2 <- a2 %>% filter(name2 %in% total_intersection$name2)
max_overlap_a2 <- a2 %>% filter(name2 %in% max_overlap$name2)

max_overlap_lu <- max_overlap %>%
  select(Name, place_id, pcod1, name1, pcod2, name2) %>%
  st_drop_geometry()

write_csv(max_overlap_lu, "data/geo/processed/google_mobility_lu.csv")

# Create a data frame of only Google mobility in "residential" setting
gha_mob_region <- gha_mob %>% 
  select(date, region, residential_percent_change_from_baseline) %>% 
  rename(mob = residential_percent_change_from_baseline)

make_date_df <- function(region, start, end){
  return(
    tibble(pcod2 = region, date = seq.Date(start, end, by = "day"))
  )
}

region_dates <- lapply(max_overlap_lu$pcod2, make_date_df, min(gha_mob_region$date), max(gha_mob_region$date)) %>% 
  do.call(rbind, .)

gha_mob_adj_long <- region_dates %>% 
  left_join(max_overlap_lu, by = c("pcod2")) %>% 
  left_join(gha_mob_region, by = c("Name" = "region", "date")) %>% 
  select(pcod1, name1, pcod2, name2, date, mob)

write_csv(gha_mob_adj_long, "data/mobility/google/processed/google_mobility.csv")

 