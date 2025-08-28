library(tidyverse)
library(tidycensus)
library(ggplot2)
library(sf) #for getting our custom shapes (made by the amazing Dr. Gerberry) to work 
##My Special Key dc98f1c12e3b8ede55889807d8b33f71cc962dc9 (it opens the tomb of cuthulu)
##census_api_key("dc98f1c12e3b8ede55889807d8b33f71cc962dc9", install = TRUE)

##making Vars
new_acs = 2023 #normaly writing the number is cool but due to it changing soon im saving it in a var so i can change it more easily

#not need more than once but this should let you explore the ACS 5 year in R. also will need a rerun come September.
Acs5.vars <- load_variables(new_acs, "acs5", cache = TRUE)
## B11003_003 married couples with kids; B11003_010 single dads; B11003_016 single moms


##finaly pulling the census data in 
demographicData = get_acs(
 geography = "tract",
 variables = c(married_parents = "B11003_003",
               single_dads = "B11003_010",
               single_moms = "B11003_016",
               population = "B11001_001"
 ),#end of c
 year = new_acs,
 state = "OH",
 county = "Hamilton",
 geometry = TRUE
)


##Geting the block total to make mhy map better
block.total <- get_decennial(geography = "block",
  state = "Ohio",
  county = "Hamilton",
  variables = "P1_001N",
  year = 2020,
  sumfile = "dhc",
  geometry = TRUE) %>% 
  select(total.pop = value)
  
schoolPrecincts <- st_read("shapefiles/cps_precincts.shp")
schoolBoundry <- st_read("shapefiles/cps_boundary.shp")

schoolBoundry <- st_set_crs(schoolBoundry, 4269)
schoolPrecincts <- st_set_crs(schoolPrecincts, 4269)




##make the data wide
the_wiiide_school = demographicData %>% 
  select(GEOID, NAME, variable, estimate) %>% 
  pivot_wider(names_from = variable, values_from = estimate) 

the_wiiide_school <- demographicData %>%
  select(GEOID, NAME, variable, estimate, geometry) %>%
  pivot_wider(names_from = variable, values_from = estimate) %>%
  mutate(total_parent_household = married_parents + single_dads + single_moms) %>% 
  mutate(parent_porp = ((total_parent_household/ population) *100))

# Read custom school precinct shapefile
#schoolPrecincts <- st_read("shapefiles/cps_precincts.shp")
#schoolPrecincts <- st_set_crs(schoolPrecincts, 4269)


interpolated_demographic_data <- interpolate_pw(
  from = st_make_valid(the_wiiide_school),
  to = st_make_valid(schoolBoundry),
  extensive = FALSE,
  weights = st_make_valid(block.total),
  weight_column = "total.pop",
  crs = 4269
)

precinct_interpolated_demographic_data <- interpolate_pw(
  from = st_make_valid(the_wiiide_school),
  to = st_make_valid(schoolPrecincts),
  extensive = FALSE,
  weights = st_make_valid(block.total),
  weight_column = "total.pop",
  crs = 4269
)

mx4_parent <- function(){
  #credit  to the funk master
  palette <- colorNumeric(
    palette = viridisLite::turbo(256), # Yellow-Orange-Red color scale, 
    domain = precinct_interpolated_demographic_data$parent_porp,
    na.color = "transparent"
  )
  
  leaflet() %>% 
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(
      data = precinct_interpolated_demographic_data,
      weight = 1,
      color = "black",
      fillColor = ~palette(parent_porp),
      fillOpacity = .5,
      opacity = .4
    ) %>% 
    addLegend(
      pal = palette,
      values = precinct_interpolated_demographic_data$parent_porp,
      title = "PublicPorp",
      position = "bottomright"
      
    )
}

precinct_interpolated_demographic_data %>% 
  ggplot(aes(fill = parent_porp)) +
  geom_sf()+
  scale_fill_viridis_c(option = "turbo") +
  labs(title = "Percent of Population who are Parents Living with Kids") +
  labs(fill = "Percent of Parents") 