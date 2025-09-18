require("tidyverse")
require("tidycensus")
require("leaflet")
require("sf")

#' Creates a map
#' @param shpFile what map we're going to use ( all caps! )
#'    "CPS" = cincinatti public school
#'    "MUN" = municipal court 4
#'    "CIT" = cincinatti city council
#'    
#' @param colType what columns we are going to look at ( all lowercase! )
#'    "age" =
#'    "income" =
#'    "race" =
#' @note make sure you load acs_data.RData in DSCI210-f25/data
#' 
shiny.map <- function(shpFile = "CIT",colType = "age"){
  
  # LETS GET OUR DATA READY
  mapDict = c(
    "CPS" = "acs_interp_cps", #cincinatti public school
    "MUN" = "acs_interp_judicial", #municipal court
    "CIT" = "acs_interp_cincy" #city council!
  )
  columnId = c(
    "age" = "median_ageE", #cincinatti public school
    "income" = "med_incomeE", #municipal court
    "race" = "pop_totalE" #city council!
  )
  
  colName = columnId[colType] # get our actual column
  
  ourMap = get( mapDict[shpFile] ) %>%  # get our dataset and set it up for maps
    st_zm() %>% 
    st_as_sf(4269)
    
  
  # PREPARE VISUALS FOR THE GRAPH
  palette <- colorNumeric(
    palette = c("yellow", "orange", "red"), # Yellow-Orange-Red color scale
    domain = ourMap[[colName]]
  )
  
  ourClr = ~palette( ourMap[[colName]] ) # get our colors

  
  mapFullName = c( # get the full name of the place we're looking for
    "CPS" = "Cincinatti Public Schools",
    "MUN" = "Municipal Court District 4", 
    "CIT" = "City Council" 
  )[shpFile]
  
  columnFullName = c( # get the full name for the variable we're showing
    "age" = "Median Age", 
    "income" = "Median Income", 
    "race" = "Total Population"
  )[colType]
  
  # MAKE THE MAP!
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
      label = ~paste( columnFullName ,": ", round(ourMap[[colName]])),
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
          ", mapFullName , " | ", columnFullName ,"
         </div>"),
      position = "topright"
    )

}
