require("tidyverse")
require("tidycensus")
require("leaflet")
require("sf")


shiny.map <- function(shpFile = "CIT"){
  
  mapDict = c(
    "CPS" = "cps_precincts.shp", #cincinatti public school
    "MUN" = "judicial_precincts.shp", #municipal court
    "CIT" = "cincy_precincts.shp" #city council!
  )
  
  ourMap = mapDict[shpFile] %>% #prep our shapefile
    st_read() %>% 
    st_zm() %>% 
    st_as_sf(4269)
  
  ourData = read_csv("data/Acs_Data.csv")
  
  
  
  leaflet() %>% 
    # BASE MAP
    addProviderTiles(providers$CartoDB.Positron) %>%
    
    # COLOR IN
    addPolygons(
      data = ourMap,
      weight = 1,
      fillOpacity = .35,
      opacity = .375
    )
  
}
