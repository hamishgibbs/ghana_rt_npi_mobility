# Compare Google and Vodafone mobility indices
require(tidyverse)

source("src/utilities/plot_ghana.R")

if(interactive()){
  .args <-  c("data/geo/processed/adm_mun_adjusted.geojson",
              "data/mobility/vodafone/processed/vodafone_mobility.csv",
              "data/mobility/google/processed/google_mobility.csv",
              "data/line_list/processed/ALL.csv",
              "output")
} else {
  .args <- commandArgs(trailingOnly = T)
}

adm2 <- st_read(.args[1])

geo_lu <- adm2 %>% st_drop_geometry() %>% 
  select(-pcod1, -name1)

rt <- read_csv("data/rt_estimates/processed/rt_regional.csv")

vf <- read_csv(.args[2]) %>% 
  left_join(geo_lu, by = c("pcod2" = "pcod2")) %>% 
  filter(date <= as.Date("2020-09-01")) %>% 
  filter(name2 %in% unique(rt$name2))

goog <- read_csv(.args[3]) %>% 
  filter(date <= as.Date("2020-09-01")) %>% 
  mutate(mob = mob * -1)

line_list <- read_csv(.args[4])

# Supplementary plot of mobility data for individual regions (1 for Google, 1 for VF)
p_vf_mob <- vf %>% 
  ggplot() + 
  geom_path(aes(x = date, y = vf), size = 0.3) +
  geom_hline(aes(yintercept = 0), linetype = "dashed", size = 0.2) + 
  facet_wrap(~name2) + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = -35, vjust = 0.5, hjust=0,
                                   size = 8)) + 
  labs(y = "Percent Change from Baseline", x = NULL)

ggutils::ggsave_png_pdf(p_vf_mob,
                        here::here("output/figs/vf_mob.png"),
                        10, 8)

p_google_mob <- goog %>%
  filter(pcod2 %in% c("AMA", "KMA")) %>%
  ggplot() +
  geom_path(aes(x = date, y = mob), size = 0.3) +
  geom_hline(aes(yintercept = 0), size = 0.2, linetype = "dashed") +
  facet_wrap(~name2) +
  theme_classic() +
  labs(x = NULL, y = "Change from baseline (%)", color = NULL) + 
  scale_x_date(date_breaks = "1 month", 
               date_labels = "%B")

ggutils::ggsave_png_pdf(p_google_mob,
                        here::here("output/figs/google_mob.png"),
                        10, 4)

# Plot of the places that have both google and vodafone vs just vf
data_coverage <- adm2 %>% 
  mutate(coverage = NA,
         coverage = ifelse(pcod2 %in% unique(vf$pcod2), "Vodafone\nOnly", coverage),
         coverage = ifelse(pcod2 %in% unique(vf$pcod2) & pcod2 %in% unique(goog$pcod2), "Both", coverage)) %>% 
  drop_na(coverage)

fill_scale <- scale_fill_manual(values = c("Both" = "blue", "Vodafone\nOnly" = "lightgreen"), na.value = "#EFEFEF")

p_coverage <- plot_ghana(data_coverage, "coverage", fill_scale, adm2, fill_title = "Mobility\ncoverage") + 
  theme(legend.position = c(0.94, 0.8))

ggutils::ggsave_png_pdf(p_coverage,
                        here::here("output/figs/mobility_coverage_mob.png"),
                        8, 5.5)
 
cor_data <- vf %>% 
  left_join(goog, by = c("date", "pcod2", "name2")) %>% 
  drop_na(name2)

plot_cor <- function(data, ncol = NULL){
  
  p_cor <- data %>% 
    ggplot() + 
    geom_jitter(aes(x = vf, y = mob), size = 0.3) + 
    geom_smooth(aes(x = vf, y = mob), method = "lm", color = "black", size = 0.3) + 
    geom_abline(linetype = "dashed", color = "blue", size = 0.3) + 
    facet_wrap(~name2, ncol = ncol) + 
    ggpubr::stat_cor(aes(x = vf, y = mob, group = name2), label.y = 8, size = 3) + 
    labs(x = "Vodafone Mobility", y = "Google Mobility (Inverse)", title = "b") + 
    theme_classic() + 
    theme(axis.title.x = element_text(margin = margin(0, 0, 25, 0)))
  
  return(p_cor)
  
}

p_cor_all <- plot_cor(cor_data %>% filter(name2 %in% c(goog$name2)), NULL)
p_cor <- plot_cor(cor_data %>% filter(pcod2 %in% c("AMA", "KMA")), 1)

# Plot mobility for Accra specifically (aggregated metric of Google and 1 for Vodafone)
p_ama_kma <- cor_data %>% 
  select(-pcod1, -name1) %>% 
  filter(pcod2 %in% c("AMA", "KMA")) %>% 
  rename(goog = mob) %>% 
  pivot_longer(!c(date, pcod2, name2), names_to = "source", values_to = "value") %>% 
  ggplot() + 
  geom_hline(aes(yintercept = 0), size = 0.1, linetype = "dashed") +
  geom_path(aes(x = date, y = value, color = source), size = 0.35) + 
  scale_color_manual(values = c("goog" = "darkgreen", "vf" = "black"), labels = c("Google", "Vodafone")) + 
  facet_wrap(~name2, ncol = 1) + 
  theme_classic() + 
  theme(legend.position = "bottom") + 
  labs(color = "Source", title= "a", y = "Change from Baseline (%)", x = NULL) + 
  scale_x_date(date_breaks = "1 month", 
               date_labels = "%B")

p <- cowplot::plot_grid(p_ama_kma, p_cor, rel_widths = c(0.6, 0.4))

ggutils::ggsave_png_pdf(p,
                        here::here("output/figs/google_vf_compare.png"),
                         10, 5)

ggutils::ggsave_png_pdf(p_cor_all,
                        here::here("output/figs/google_vf_cor_all.png"),
                        9, 8)


# Difference change for both indicators before & after partial lockdown
lockdown_start <- as.Date("2020-03-30")

goog %>% 
  filter(pcod2 %in% c("AMA", "KMA")) %>% 
  filter(date %in% c(lockdown_start - 7, lockdown_start + 7)) %>% 
  mutate(mob = mob * -1) %>% 
  select(name2, date, mob) %>% 
  pivot_wider(id_cols = c(name2), names_from = date, values_from = mob) %>% 
  mutate(perc_change = `2020-04-06` - `2020-03-23`)

vf %>% 
  filter(pcod2 %in% c("AMA", "KMA")) %>% 
  filter(date %in% c(lockdown_start - 7, lockdown_start + 7)) %>% 
  select(name2, date, vf) %>% 
  pivot_wider(id_cols = c(name2), names_from = date, values_from = vf) %>% 
  mutate(perc_change = `2020-04-06` - `2020-03-23`)

