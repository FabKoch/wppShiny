
data_map_EU_bar <- map_centroid_EU %>% 
  select(
    contains("-")) 


leaflet(map_polygon_EU) %>% 
  addTiles(tilesURL) %>% 
  addPolygons() %>% 
  addMinicharts(
    map_centroid_EU$x, map_centroid_EU$y,
    chartdata = data_map_EU_bar,
    colorPalette = d3.schemeCategory10,
    width = 45, height = 45
  )
