plot_ghana <- function(data_layer, variable, fill_scale, base_country, 
                       title = NULL, subtitle = NULL, fill_title = NULL){
  
  base <- ggutils::basemap() %>% filter(sovereignt != "Ghana")
  
  base$geometry <- base$geometry + c(0.005, 0)
  
  base <- st_set_crs(base, 4326)
  
  p <- data_layer %>% 
    ggplot() + 
    geom_sf(data = base, size = 0.05, color = "black", fill = "white") +
    geom_sf(data = base_country, size = 0.05, color = "black", fill = "white") +
    geom_sf(aes(fill = !! sym(variable)), size = 0.1, color = "black") + 
    ggutils::geo_lims(data_layer) + 
    fill_scale + 
    theme_void() + 
    theme(panel.background = element_rect(fill = "light blue", size = 0)) + 
    labs(title = title, subtitle = subtitle, fill = fill_title)
  
  return(p)
  
}
