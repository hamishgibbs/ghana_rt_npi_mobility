# Script to adjust administrative reference for municipal areas
library(sf)
library(tidyverse)

# Tema (Prefix: "TMA - ") -> "TMA", "TEMA METROPOLITAN ASSEMBLY"
# Sekondi-Takoradi (Prefix: "STMA-") -> "STMA", "SEKONDI-TAKORADI METROPOLITAN ASSEMBLY"
# Tamale (Prefix: "TM-") -> "TM-", "TAMALE METROPOLITAN"
# Kumasi (Prefix: "KMA-") -> "KMA", "KUMASI METROPOLITAN ASSEMBLY"
# Accra (Prefix: "AMA-") -> "AMA", "ACCRA METROPOLITAN ASSEMBLY"

adm <- st_read("data/geo/processed/adm_combined.geojson")

tma <- stringr::str_detect(adm$name2, "TMA - ")
stma <- stringr::str_detect(adm$name2, "STMA-")
tm <- stringr::str_detect(adm$name2, "TM-")
kma <- stringr::str_detect(adm$name2, "KMA-")
ama <- stringr::str_detect(adm$name2, "AMA-")

adm_mun <- adm %>% 
  mutate(pcod2_mun = NA, 
         name2_mun = NA,
         pcod2_mun = ifelse(tma, "TMA", pcod2),
         name2_mun = ifelse(tma, "TEMA METROPOLITAN ASSEMBLY", name2)) %>% 
  mutate(pcod2_mun = ifelse(stma, "STMA", pcod2_mun),
         name2_mun = ifelse(stma, "SEKONDI-TAKORADI METROPOLITAN ASSEMBLY", name2_mun)) %>% 
  mutate(pcod2_mun = ifelse(tm, "TM", pcod2_mun),
         name2_mun = ifelse(tm, "TAMALE METROPOLITAN", name2_mun)) %>% 
  mutate(pcod2_mun = ifelse(kma, "KMA", pcod2_mun),
         name2_mun = ifelse(kma, "KUMASI METROPOLITAN ASSEMBLY", name2_mun)) %>% 
  mutate(pcod2_mun = ifelse(ama, "AMA", pcod2_mun),
         name2_mun = ifelse(ama, "ACCRA METROPOLITAN ASSEMBLY", name2_mun)) 

adm_mun_lu <- adm_mun %>% st_drop_geometry()

write_csv(adm_mun_lu, "data/geo/processed/admin2_mun_lu.csv")

adm_mun <- adm_mun %>% 
  group_by(pcod2_mun, name2_mun, pcod1, name1) %>% 
  summarise(.groups = "drop") %>% 
  rename(pcod2 = pcod2_mun,
         name2 = name2_mun)

st_write(adm_mun, "data/geo/processed/adm_mun_adjusted.geojson", delete_dsn=T)