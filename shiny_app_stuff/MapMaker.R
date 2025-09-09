require("tidyverse")
require("tidycensus")
require("leaflet")
require("sf")

#' Creates a map
#' @param shpFile what map we're going to use
#'    "CPS" = cincinatti public school
#'    "MUN" = municipal court 4
#'    "CIT" = cincinatti city council
#'
shiny.map <- function(shpFile = "CIT"){
  
  mapDict = c(
    "CPS" = "shapefiles/cps_precincts.shp", #cincinatti public school
    "MUN" = "shapefiles/judicial_precincts.shp", #municipal court
    "CIT" = "shapefiles/cincy_precincts.shp" #city council!
  )
  
  ourMap = mapDict[shpFile] %>% #prep our shapefile
    st_read() %>% 
    st_zm() %>% 
    st_as_sf(4269)
  
  
 # ourData = read_csv("data/Acs_Data.csv") %>% #prep our acs data
 #   mutate(GEOID = toString(GEOID)) %>% 

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
