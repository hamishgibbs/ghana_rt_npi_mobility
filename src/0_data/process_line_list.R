require(tidyverse)
source("src/utilities/epi_week.R")

# Location of manually cleaned / extracted line lists
indir <- "data/line_list/intermediate_08_2021"

# Directory of spatial and epi_week lookups (coded to known values)
adm_ludir <- "config/cases/spatial_lookups"

# Output directory
outdir <- "data/line_list/processed"

regions <- list.files(indir, pattern = ".csv")

regions <- gsub(".csv", "", regions)

# Function to update region names from spatial lookups
clean_adm_regions <- function(in_data, adm_lu){
  
  processed <- in_data %>% 
    mutate(district = stringr::str_to_upper(district)) %>% 
    left_join(adm_lu, by = c("region", "district"))
  
  if(length(processed %>% filter(is.na(pcod2)) %>% pull(date)) >0){
    print(processed %>% filter(is.na(pcod2)) %>% 
            group_by(region, district) %>% 
            summarise(n = n()))
    stop("Some districts are not referenced")
  }
  
  return(processed)
}

# Function to combine all processing steps per region 
# And output final line lists
process_region <- function(region){
  
  print(region)
  
  in_fn <- paste0(indir, "/", region, ".csv")
  adm_lu_fn <- paste0(adm_ludir, "/", region, "_spatial_lookup_2021_07_26_140436.csv")
  in_data <- read_csv(in_fn, col_types = cols())
  adm_lu <- read_csv(adm_lu_fn, col_types = cols())
  
  processed <- clean_adm_regions(in_data, adm_lu)
  
  # Cleaned
  processed <- processed %>% 
    select(date, pcod2, name2, pcod1, name1) %>% 
    mutate(date = lubridate::dmy(date))
  
  # Aggregated & Cleaned
  processed_agg <- processed %>% 
    group_by(date, pcod2, name2, pcod1, name1) %>% 
    summarise(confirm = n(), .groups = "drop")
  
  out_fn <- paste0(outdir, "/", region, ".csv")
  out_fn_agg <- paste0(outdir, "/", region, "_aggregated.csv")
  
  write_csv(processed, out_fn)
  write_csv(processed_agg, out_fn_agg)
  
  return(processed_agg)

}

res <- lapply(regions, process_region)

# Write a combined aggregate from all line lists
do.call(rbind, res) %>% 
  write_csv(paste0(outdir, "/ALL.csv"))

