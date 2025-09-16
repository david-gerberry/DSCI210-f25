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
#' @param colName the name of the column that we want to draw data from
#'
shiny.map <- function(shpFile = "CIT",colName = "pop_totalE"){
  
  mapDict = c(
    "CPS" = "acs_interp_cps", #cincinatti public school
    "MUN" = "acs_interp_judicial", #municipal court
    "CIT" = "acs_interp_cincy" #city council!
  )
  
  #ourMap = mapDict[shpFile] %>% #prep our shapefile
  #  st_zm() %>% 
  #  st_as_sf(4269)
  ourMap = get( mapDict[shpFile] ) %>% 
    st_zm() %>% 
    st_as_sf(4269)
    
  
  palette <- colorNumeric(
    palette = c("yellow", "orange", "red"), # Yellow-Orange-Red color scale
    domain = ourMap[[colName]]
  )
  
  ourClr = ~palette( ourMap[[colName]] )

  leaflet() %>% 
    # BASE MAP
    addProviderTiles(providers$CartoDB.Positron) %>%
    
    # COLOR IN
    addPolygons(
      data = ourMap,
      weight = 1,
      fillOpacity = .35,
      opacity = .375,
      color = ourClr,
      label = ~paste( colName ,": ", round(ourMap[[colName]])),
    layerId = ~ourMap$PRECINCT
    ) %>% 
    
    addControl(
      html = paste0(
        "<div style='
            font-size: 12px;
            font-weight:600;
            color:#000;
            background:rgba(255,255,255,0.9);
            padding:4px 10px;
            border-radius:6px;'>
          ", shpFile , " | ", colName ,"
         </div>"),
      position = "topright"   # still required, but we override with CSS
    )

}
