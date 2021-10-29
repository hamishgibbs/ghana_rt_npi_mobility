# Combines two admin data sets to create a 
# standard admin reference Districts (Level 2) 
# And their corresponding regions (Level 1)
require(sf)
require(tidyverse)

adm1 <- st_read("data/geo/admin1.geojson") %>% 
  select(-centroid)

adm2 <- st_read("data/geo/admin2.geojson") %>% 
  select(-centroid)

adm1_pcod <- read.csv("data/geo/pcods_admin1.csv", header = F) %>% 
  rename(pcod = V1, name = V2) %>% as_tibble()

adm2_pcod <- read.csv("data/geo/pcods_admin2.csv", header = F) %>% 
  rename(pcod = V1, name = V2) %>% as_tibble()

# Join names to geography
adm1 <- adm1 %>% 
  left_join(adm1_pcod, by = c("pcod")) %>% 
  mutate(geometry = st_make_valid(geometry))

adm2 <- adm2 %>% 
  left_join(adm2_pcod, by = c("pcod")) %>% 
  mutate(geometry = st_make_valid(geometry))

# Compute intersection of Districts and Regions
spatial_intersection <- st_intersection(adm2, adm1)

# Select the region corresponding to each district
adm_combined <- spatial_intersection %>% 
  mutate(area = as.numeric(units::set_units(st_area(geometry), "km^2"))) %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  group_by(pcod, name) %>% 
  top_n(n=1, wt = area)

adm_combined <- adm_combined %>% 
  select(-area) %>% 
  rename(pcod2 = pcod, name2 = name, pcod1 = pcod.1, name1 = name.1)

adm_combined_geo <- adm2 %>% 
  left_join(adm_combined, by = c("pcod" = "pcod2", "name" = "name2")) %>% 
  rename(pcod2 = pcod, name2 = name)

st_write(adm_combined_geo, "data/geo/processed/adm_combined.geojson", delete_dsn = T)

if(interactive()){
  p_a2 <- adm_combined_geo %>% 
    st_simplify(preserveTopology = T, dTolerance = 0.001) %>% 
    ggplot() + 
    geom_sf(aes(fill = name1)) + 
    theme(legend.position = "none")
  
  p_a1 <- adm1 %>% 
    st_simplify(preserveTopology = T, dTolerance = 0.001) %>% 
    ggplot() + 
    geom_sf(aes(fill = name)) + 
    theme(legend.position = "none")
  
  p <- cowplot::plot_grid(p_a2, p_a1)
}