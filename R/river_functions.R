plot_graph <- function(g, station_info){
  tbl <- graph_to_tibble(g, station_info)
  plot_tibble(tbl)
}

plot_tibble <- function(tbl){
  station_info <- tibble(Lon = unique(c(tbl$Lon_from, tbl$Lon_to)),
                         Lat = unique(c(tbl$Lat_from, tbl$Lat_to)))
  ggplot() +
    geom_point(data = station_info, aes(x = Lon, y = Lat), 
               color = "black", size = 3, shape = 21, 
               fill = my_palette_vibrant$blue, stroke = .5) +
    geom_curve(data = tbl, 
               aes(x = Lon_from, y = Lat_from, xend = Lon_to, yend = Lat_to),
               arrow = arrow(length = unit(0.1, "inches"), type = "open"),  
               alpha = 0.75, curvature = 0.2, linewidth = 0.8, 
               lineend = "butt")
  
}

graph_to_tibble <- function(g, station_info){
  as.data.frame(as_edgelist(g)) %>% 
    rename(from = V1, to = V2) %>% 
    left_join(station_info %>% rename(from = id_name), by = "from") %>% 
    rename(Lat_from = Lat, Lon_from = Lon) %>% 
    left_join(station_info %>% rename(to = id_name), by = "to") %>% 
    rename(Lat_to = Lat, Lon_to = Lon)
}
