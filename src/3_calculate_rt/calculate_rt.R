# Compute Rt estimates from cleaned & filtered data
require(EpiNow2)
require(tidyverse)
require(data.table)

target_folder <- "data/rt_estimates/processed/regional"
cases <- read_csv("data/rt_estimates/cases/rt_filtered_cases.csv") %>% 
  rename(region = name2) %>% 
  select(date, confirm, region)

# Vector of names that have already been calculated
# (optimisation in case run is interrupted)
processed_names <- stringr::str_replace(list.files(target_folder, ".csv"), "_r_estimate.csv", "")

generation_time <- readRDS("data/rt_estimates/params/generation_time.rds")

# Function to calculate rt estimates for single regions
calculate_rt <- function(data){
  
  res <- epinow(reported_cases = data,
                generation_time = generation_time,
                delays = delay_opts(),
                rt = rt_opts(gp_on = "R0"),
                obs = obs_opts(week_length=7, week_effect=F),
                stan = stan_opts(samples = 2000, warmup = 250, chains = 4,
                                 control = list(adapt_delta = 0.95),
                                 cores=2), 
                output = c("timing", "samples"))
  return(res)  
  
}

regions <- cases %>% pull(region) %>% unique()

write_rds(regions, "data/rt_estimates/expected_regions.rds")

regions <- regions[!regions %in% processed_names]

for (area in regions){
  area <- gsub("/", " ", area)
  data <- cases %>% filter(region == area)
  res <- calculate_rt(data)
  summarised <- res$estimates$summarised
  summarised$name2 <- area
  
  r_samples <- res$estimates$samples %>% 
    filter(parameter == "R",
           sample %in% sample(1:2000, 1000))
  r_samples$name2 <- area
  
  write.csv(summarised, paste0(target_folder, "/", area, "_r_estimate.csv"))
  write.csv(r_samples, paste0(target_folder, "/", area, "_r_samples.csv"))
  rm(res)
  gc()

} 


