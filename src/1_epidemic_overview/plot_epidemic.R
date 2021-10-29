# Plot reported cases in all districts and annotate with timing of NPIs
require(sf)
require(tidyverse)

source("src/utilities/plot_ghana.R")

.args <-  c("data/geo/processed/adm_mun_adjusted.geojson",
            "data/line_list/ALL_outliers_removed.csv",
            "data/interventions/processed/npi_custom_wide.csv",
            "output")

adm2 <- st_read(.args[1])

geo_lu <- adm2 %>% 
  st_drop_geometry()

# Some cases are recorded before the first known case arrival
line_list <- read_csv(.args[2]) %>% 
  filter(lubridate::year(date) <= 2020 & 
           date >= as.Date("2020-03-12")) %>% 
  filter(date <= as.Date("2020-09-01"))

npi <- read_csv(.args[3])

name_order <- line_list %>% group_by(name2) %>% summarise(confirm = sum(confirm, na.rm = T)) %>% arrange(-confirm) %>% pull(name2)

# plot of epidemic in individual regions (supplement)
cutoff <- line_list %>% pull(name2) %>% unique() %>% length() / 2

group1 <- name_order[1:cutoff]
group2 <- name_order[cutoff + 1:cutoff]

p_region_facet <- line_list %>% 
  filter(name2 %in% group1) %>% 
  mutate(name2 = factor(name2, levels = name_order)) %>% 
  ggplot() + 
  geom_bar(aes(x = date, y = confirm), stat="identity", color = "grey", alpha = 0.2) + 
  facet_wrap(~name2, scales = "free_y") + 
  theme_classic() + 
  scale_x_date() + 
  theme(axis.text.x = element_text(angle = -35, vjust = 0.5, hjust=0,
                                   size = 8)) + 
  labs(x = NULL, y = "Cases")

ggutils::ggsave_png_pdf(p_region_facet,
                        "output/figs/region_cases_overview1.png",
                        20, 20)

# Plot of spreading of epidemic through time
arrival_dates <- line_list %>% 
  group_by(name2) %>%
  summarise(date = min(date),
            confirm = sum(confirm, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(days_from = as.numeric(date - min(date)))

arrival_data <- adm2 %>% 
  left_join(arrival_dates, by = c("name2"))

fill_scale_arrival <- colorspace::scale_fill_continuous_sequential("Mint", na.value = "grey")
fill_scale_total <- colorspace::scale_fill_continuous_sequential("Blues", na.value = "grey",
                                                                 trans = "log10")

p_arrival <- plot_ghana(arrival_data, "days_from", fill_scale_arrival, adm2, fill_title = "Days\nfrom\nIntroduction")
p_total <- plot_ghana(arrival_data, "confirm", fill_scale_total, adm2, fill_title = "Total\ncases")

p_map <- cowplot::plot_grid(p_arrival, p_total, nrow = 1)

ggutils::ggsave_png_pdf(p_map,
                        "output/figs/region_arrival.png",
                        10, 6)

line_list <- read_csv("data/rt_estimates/cases/rt_filtered_cases.csv")

# Plot of national epidemic with interventions overlayed
total_cases_date <- line_list %>% 
  group_by(date) %>% 
  summarise(confirm = sum(confirm, na.rm = T))

npi_y_index <- seq(max(total_cases_date$confirm), min(total_cases_date$confirm), length.out = nrow(npi) + 2)

npi_plot <- npi %>% 
  mutate(start = lubridate::dmy(start),
         end = lubridate::dmy(end)) %>% 
  mutate(npi_len = as.numeric(end - start)) %>% 
  arrange(npi_len) %>% 
  mutate(y_index = npi_y_index[1:nrow(npi) + 1],
         mid_date = end - (npi_len / 2))

p_cases_si <- total_cases_date %>% 
  ggplot() + 
  geom_bar(aes(x = date, y = confirm), stat = "identity", fill = "grey", alpha = 0.8) + 
  geom_segment(data = npi_plot, aes(x = start, xend = end, y = y_index, yend = y_index, color = label), linetype = "dashed") + 
  geom_text(data = npi_plot, aes(x = mid_date, y = y_index + 20, label = label), size = 2.8) + 
  theme_classic() + 
  labs(x = NULL, y = "Confirmed Cases") + 
  theme(legend.position = "none")

ggutils::ggsave_png_pdf(p_cases_si,
                        "output/figs/cases_si.png", 
                        8, 3.5)
