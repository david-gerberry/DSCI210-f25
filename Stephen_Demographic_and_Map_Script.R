library(tidyverse)
library(tidycensus)
library(ggplot2)
library(sf)



#### Pulling Census Data ####

census_api_key("4951cbe6bdec5269e91df7656ba86ee58a29cff7", install = TRUE)

Census_Data <- get_acs(
  geography = "block group",
  variables = c(
    med_household_income = "B19013_001",
    #need to add more things will figure this out tomrrow 
  ), 
  state = "OH",
  county= "Hamilton",
  year = 2023,
  geometry = TRUE
)



#### Map Making ####



block.total <- get_decennial(geography = "block", 
                             state = "Ohio",
                             county = "Hamilton",
                             variables = "P1_001N", 
                             year = 2020,
                             sumfile = "dhc",
                             geometry = TRUE) %>% 
  select(total.pop = value) 




cincy_precincts <- st_read("shapefiles/cincy_precincts.shp")
cincy_boundary <- st_read("shapefiles/cincy_boundary.shp")

cincy_boundary <- st_set_crs(cincy_boundary, 4269)
cincy_precincts <- st_set_crs(cincy_precincts, 4269)


household_income_interpolated <- interpolate_pw(
  from = st_make_valid(Census_Data),
  to = st_make_valid(cincy_boundary),
  extensive = FALSE,                 ## because you don't want to add the median ages,
  weights =st_make_valid(block.total),
  weight_column = "total.pop",
  crs = 4269
) 

precinct_household_income_interpolated <- interpolate_pw(
  from = st_make_valid(Census_Data),
  to = st_make_valid(cincy_precincts),
  extensive = FALSE,                 ## because you don't want to add the median ages,
  weights =st_make_valid(block.total),
  weight_column = "total.pop",
  crs = 4269
) 


precinct_household_income_interpolated %>%
  ggplot(aes(fill = estimate))+
  geom_sf()+
  scale_fill_viridis_c(option = "turbo") +
  labs(fill= "Median Household Income")
