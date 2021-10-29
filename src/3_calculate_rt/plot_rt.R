# Plot cases and Rt in individual districts + map of districts in context
require(tidyverse)
require(sf)

source("src/utilities/plot_ghana.R")

epochs <- read_rds("data/regression/processed/epochs.rds")

adm2 <- st_read("data/geo/processed/adm_mun_adjusted.geojson")

rt <- read_csv("data/rt_estimates/processed/rt_regional.csv")

name2_include <- rt %>% 
  group_by(name2) %>% 
  summarise(min_date = min(date)) %>% 
  filter(min_date <= as.Date("2020-03-30")) %>% 
  pull(name2)

rt <- rt %>% 
  filter(name2 %in% name2_include) %>% 
  filter(date <= as.Date("2020-09-01"))

cases <- read_csv("data/line_list/ALL_outliers_removed.csv") %>% 
  filter(name2 %in% name2_include) %>% 
  filter(date <= as.Date("2020-09-01"))

plot_ci_ribbons <- function(data, alpha = 0.2){
  
  geom <- list(
    geom_ribbon(aes(x = date, ymin = lower_90, ymax = upper_90), size = 0, fill="red", alpha = alpha),
    geom_ribbon(aes(x = date, ymin = lower_50, ymax = upper_50), size = 0, fill="red", alpha = alpha),
    geom_ribbon(aes(x = date, ymin = lower_20, ymax = upper_20), size = 0, fill="red", alpha = alpha),
    geom_hline(aes(yintercept = 1), color = "black", size = 0.3, linetype = "dashed"),
    labs(y = expression(R[t]), x = NULL)
  )  
  
  return(geom)
  
}

# plot of rt in individual regions
rt_regional <- rt %>% 
  left_join(adm2 %>% st_drop_geometry(), by = c("name2")) %>% 
  ggplot() + 
  plot_ci_ribbons() + 
  theme_classic() + 
  facet_wrap(~name2) + 
  theme(strip.background = element_blank(),
        axis.text.x = element_text(angle = -20, vjust = 0.5, hjust=0)) + 
  labs(title = "d")

ggutils::ggsave_png_pdf(rt_regional, "output/figs/rt_regional.png", 
                        10, 9)

# Combined pallette for rt name2 values
rt_name2 <- unique(rt$name2)
name2_pal <- sample(c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#ffff99','#b15928',
               '#8dd3c7','#ffffb3','#bebada','#fb8072','#80b1d3','#fdb462','#b3de69','#fccde5','#d9d9d9','#bc80bd','#ccebc5','#ffed6f',
               '#fdbf6f', '#ffffb3'))
names(name2_pal) <- rt_name2

# plot regions included in Rt estimates
rt_regions <- adm2 %>% filter(name2 %in% rt$name2) %>% 
  mutate(included = "yes")

fill_scale <- scale_fill_manual(values = name2_pal)

# new map for rt regions
rt_regions_split <- adm2 %>% filter(name2 %in% rt$name2) %>% 
  group_by(name1) %>% 
  group_split()

plot_district_subset <- function(index, rt_regions_split){
  
  p <- rt_regions_split[[index]] %>% 
    ggplot() + 
    geom_sf(data = adm2, size = 0.2, color = "black", fill = "white") + 
    geom_sf(aes(fill = name2), size = 0.2, color = "black") + 
    fill_scale + 
    theme_void() + 
    ggutils::geo_lims(rt_regions_split[[index]]) + 
    theme(legend.position = "none", 
          plot.title = element_text(hjust = 0.5)) + 
    labs(title = index)
  
  return(p)
}

district_subsets <- lapply(1:3, plot_district_subset, rt_regions_split = rt_regions_split)

p_row <- cowplot::plot_grid(district_subsets[[1]], district_subsets[[2]], 
                            rel_widths = c(0.3, 0.7))
p_district_close <- cowplot::plot_grid(p_row, district_subsets[[3]], 
                                       nrow = 2, 
                                       rel_heights = c(0.65, 0.35))

district_bbox <- lapply(rt_regions_split, function(x){return(st_as_sfc(st_bbox(x)))}) %>% 
  do.call(rbind, .) %>% 
  st_as_sfc() %>% 
  st_set_crs(4326)
  
bbox_coords <- st_coordinates(st_centroid(district_bbox))

label_coords <- tibble(lat = bbox_coords[,2] + c(0.8, 0.65, 0.55), 
                       lon = bbox_coords[,1]) %>% 
  mutate(id = row_number())

p_district_inset <- district_bbox %>% 
  ggplot() + 
  geom_sf(data = adm2, size = 0.1, color = "black", fill = "white") + 
  geom_sf(color = "red", fill = "transparent") + 
  geom_label(data = label_coords, aes(x = lon, y = lat, label = id), 
            color = "red") + 
  theme_void()



plot_max_i_annotation <- function(line_y, text_y, line_size, text_size){
  # add an annotation with the maximum period of interventions
  
  max_i_p <- list(
    start = as.Date("2020-04-01"),
    end = as.Date("2020-04-28")
  )
  
  return (
    list(
      geom_segment(aes(x = max_i_p$start, xend = max_i_p$end, y = line_y, yend = line_y),
                   color = "blue", size = line_size),
      annotate(geom = "text", x = max_i_p$start + (max_i_p$end - max_i_p$start) / 2, y = text_y, label = "Maximum NPI Stringency",
               size = text_size)
    )
  )
}


# plot rt all regions
p_median <- rt %>%
  ggplot() + 
  geom_path(aes(x = date, y = median, group = name2, color = name2), size = 0.3) + 
  geom_hline(aes(yintercept = 1), color = "black", size = 0.4, linetype = "dashed") + 
  scale_color_manual(values = name2_pal) + 
  theme_classic() + 
  ylim(0, 3) + 
  labs(title = "b", y = "Median", x = NULL) + 
  theme(legend.position = "none") + 
  plot_max_i_annotation(2.8, 3.0, 1, 2.5)

# plot total cases per admin area through time
p_cases <- cases %>% 
  filter(name2 %in% rt_name2) %>% 
  ggplot() + 
  geom_bar(aes(x = date, y = confirm, fill = name2), stat = "identity") + 
  scale_fill_manual(values = name2_pal) + 
  theme_classic() + 
  xlim(min(rt$date), max(rt$date)) + 
  theme(legend.position = "none") + 
  labs(title = "a", y = "Reported Cases", x = NULL) + 
  plot_max_i_annotation(400, 435, 1, 2.5)

p_col <- cowplot::plot_grid(p_cases, 
                            p_median + theme(axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0.7, unit = 'cm'))), 
                            ncol = 1)

p_map <- cowplot::plot_grid(ggplot() + theme_void(),
                            p_district_close, 
                            p_district_inset, 
                            ncol = 1,
                            rel_heights = c(0.05, 0.5, 0.5))

p <- cowplot::plot_grid(p_col, p_map, 
                        nrow = 1, rel_widths = c(0.7, 0.3),
                        labels = c("", "c"),
                        label_fontface = "plain",
                        label_x = 0.05)

ggutils::ggsave_png_pdf(p, "output/figs/rt_overview.png", 
                        10, 5)

i_date <- as.Date("2020-04-01")

# Extract changes in Rt before and after lockdowns
rt %>% 
  filter(date %in% c(i_date - 7, i_date + 7)) %>% 
  select(date, median, name2) %>% 
  pivot_wider(names_from = date, values_from = "median") %>% 
  mutate(difference = `2020-04-08` - `2020-03-25`) %>% 
  mutate(difference_lt0 = difference < 0) %>% pull(difference_lt0) %>% is.na() %>% sum()#sum(na.rm=T)
  #filter(is.na(difference_lt0))
  

