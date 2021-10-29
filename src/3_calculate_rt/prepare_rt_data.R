# Filter regions by number of reported cases and days cases were reported
require(tidyverse)
require(sf)

# reading in case data with outliers manually removed
cases <- read_csv("data/line_list/ALL_outliers_removed.csv") %>% 
  filter(date <= as.Date("2020-09-01"))

name2_filtered <- cases %>% 
  group_by(name2) %>% 
  summarise(total_confirm = sum(confirm, na.rm = T),
            total_days = length(unique(date)),
            first_report = min(date),
            .groups = "drop") %>% 
  filter(total_confirm > 100 & first_report <= as.Date("2020-03-30"))

p_thresh <- cases %>% 
  filter(name2 %in% name2_filtered$name2) %>% 
  ggplot() + 
  geom_bar(aes(x = date, y = confirm), stat = "identity") + 
  facet_wrap(~name2, scales = "free_y") + 
  theme_classic()

ggutils::ggsave_png_pdf(p_thresh,
                        "output/figs/rt_thresh.png",
                        10, 10)

cases_filtered <- cases %>% 
  left_join(name2_filtered, by = c("name2")) %>% 
  arrange(-total_confirm) %>% 
  drop_na(total_confirm) %>% 
  select(-total_confirm, -first_report, -total_days)

write_csv(cases_filtered, "data/rt_estimates/cases/rt_filtered_cases.csv")

# summary of cases per district
cases_filtered$name2 %>% unique() %>% length()

cases_sum <- cases_filtered %>% 
  group_by(name2) %>% 
  summarise(n_dates = length(unique(date)),
            confirm_min = min(confirm),
            confirm_max = max(confirm),
            confirm_mean = mean(confirm),
            confirm_sd = sd(confirm))

max(cases_sum$confirm_max)
mean(cases_sum$n_dates)
as.Date("2020-03-12") - as.Date("2020-09-01")
