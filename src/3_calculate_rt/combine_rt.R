# Script to combine Rt estimates
require(tidyverse)

exp_regions <- read_rds("data/rt_estimates/expected_regions.rds")

rt_folder <- "data/rt_estimates/processed/regional"

# Get data for filtered regions
# DEV: calculate_rt.R outputs a list of regions that should be expected. 
# Construct expected file names from these (will error if names are not present)
estimate_files <- paste0("data/rt_estimates/processed/regional/", exp_regions, "_r_estimate.csv")
sample_files <- paste0("data/rt_estimates/processed/regional/", exp_regions, "_r_samples.csv")

combine_estimates <- function(files){
  
  data_list <- list()
  
  for (fn in files){
    data_list[[fn]] <- read_csv(fn, col_types = cols()) %>% 
      filter(variable == "R")
  }
  
  return(do.call(rbind, data_list))
  
}

estimates <- combine_estimates(estimate_files) %>% 
  group_by(name2) %>% 
  filter(min(date) <= as.Date("2020-03-30"))

# DEV: check that expected regions are all included in the data
setdiff(exp_regions, estimates$name2 %>% unique())
setdiff(estimates$name2 %>% unique(), exp_regions)

write_csv(estimates, "data/rt_estimates/processed/rt_regional.csv")


