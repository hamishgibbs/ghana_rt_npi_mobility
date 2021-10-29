# Normalize NPI data sets (OxCGRT, GHS) to indices
library(tidyverse)

ox <- read_csv("https://github.com/OxCGRT/covid-policy-tracker/raw/master/data/OxCGRT_latest.csv",
               col_types = cols(Date = col_character(),
                                RegionName = col_character(),
                                RegionCode = col_character()))

ox <- ox %>%
  filter(CountryName == "Ghana" & is.na(RegionName)) %>%
  mutate(Date = lubridate::ymd(Date))

interventions_clean <- read_csv("data/interventions/ppt_intervention_data.csv") %>%
  mutate(date = lubridate::dmy(date))

# Process custom GHS NPI data
int_wide <- interventions_clean %>%
  select(-comments, -link) %>%
  filter(type == "measure") %>%
  pivot_wider(names_from = stage, values_from = date)

int_list <- int_wide %>%
  group_by(label) %>%
  group_split()

get_date_seq <- function(li){
  return(
    tibble(date = seq.Date(from = li$start, to = li$end, by = "day"),
           label = li$label,
           value = 1)
  )
  
}

custom_si_data <- lapply(int_list, get_date_seq) %>%
  do.call(rbind, .)

custom_si <- custom_si_data %>% 
  group_by(date) %>%
  summarise(value = sum(value, na.rm = T), .groups = "drop") %>%
  mutate(type = "Custom",
         value = (value / 8) * 100)

write_csv(custom_si, "data/interventions/processed/si_index_custom.csv")


if(interactive()){

  p_int <- int_wide %>%
    ggplot() +
    geom_segment(aes(x = start, xend = end, y = label, yend = label, color = label),
                 size = 2) +
    xlim(c(min(int_wide$start), max(int_wide$end))) +
    theme_classic() +
    theme(legend.position = "none") +
    labs(y = NULL, x = NULL)
  
  ggutils::ggsave_png_pdf(p_int,
                          here::here("output/figs/si_timeline.png"),
                          10, 5)
  
}

ox_si <- ox %>%
  select(Date, StringencyIndex) %>%
  mutate(type = "OxCGRT") %>%
  rename(date = Date,
         value = StringencyIndex)


# Output OxCGRT data
write_csv(ox_si, "data/interventions/processed/oxcgrt_si.csv")

si_full <- custom_si %>% 
  rename(npi_si_custom = value) %>% 
  select(-type) %>% 
  left_join(ox_si, by = "date") %>% 
  rename(npi_si_ox = value) %>% 
  select(-type)

write_csv(si_full, "data/interventions/processed/si_index.csv")

si <- rbind(ox_si, custom_si)

get_first_max_date <- function(si){
  return(si %>%
           filter(value == max(value, na.rm = T)) %>%
           filter(date == min(date, na.rm = T))
  )
}

first_dates <- rbind(get_first_max_date(custom_si),
                     get_first_max_date(ox_si))

if(interactive()){
  p_si_comparison <- si %>%
    ggplot() +
    geom_path(aes(x = date, y = value, color = type)) +
    geom_vline(data = first_dates, aes(xintercept = date, color = type),
               linetype = "dashed") +
    xlim(c(min(custom_si$date), max(custom_si$date))) +
    scale_color_manual(values = c("OxCGRT" = "black", "Custom" = "red")) +
    theme_classic() +
    theme(legend.position = c(0.88, 0.88)) +
    labs(color = "Index", y = "Severity Index", x = NULL)  
  
  ggutils::ggsave_png_pdf(p_si_comparison,
                          here::here("output/figs/si_comparison.png"),
                          10, 5)
  
  write_rds(p_si_comparison, here::here("output/figs/si_comparison.rds"))
  
}

write_csv(si, here::here("data/interventions/processed/si_index.csv"))

