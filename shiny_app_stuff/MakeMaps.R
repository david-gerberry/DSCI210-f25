

shiny.map <- function(shpFile){
  
  mapDict = c(
    "CPS" = "banana",
    "R" = "Z"
  )
  
  #map.title = "Municipal District 4 in Hamilton County"
  
  leaflet() %>% 
    # BASE MAP
    addProviderTiles(providers$CartoDB.Positron) %>%
    
    # COLOR IN
    addPolygons(
      data = interpolated.results,
      weight = 1,
      fillOpacity = .35,
      opacity = .375
    ) %>% 
    
    addPolygons(
      data = entireCountyGeometry,
      weight = 2,
      fillOpacity = 0.1,
      opacity = 1
    )
  
}