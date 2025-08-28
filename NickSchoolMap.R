library(tidyverse)
library(tidycensus)
library(ggplot2)
library(leaflet)
library(sf) #for getting our custom shapes (made by the amazing Dr. Gerberry) to work 
##My Special Key dc98f1c12e3b8ede55889807d8b33f71cc962dc9 (it opens the tomb of cuthulu)
##census_api_key("dc98f1c12e3b8ede55889807d8b33f71cc962dc9", install = TRUE)

##making Vars
new_acs = 2023 #normaly writing the number is cool but due to it changing soon im saving it in a var so i can change it more easily

#not need more than once but this should let you explore the ACS 5 year in R. also will need a rerun come september.
Acs5.vars <- load_variables(new_acs, "acs5", cache = TRUE)
## B11003_003 married couples with kids; B11003_010 single dads; B11003_016 single moms


##finaly pulling the census data in 

PublicVSPrivate <- get_acs(
  geography = "tract",
  variables = c(MPu0 = "B14002_008",
                MPi0 = "B14002_009",
                MPu14 = "B14002_011",
                MPi14 = "B14002_012",
                MPu58 = "B14002_014",
                MPi58 = "B14002_015",
                MPu912 = "B14002_017",
                MPi912 = "B14002_018",
                FPu0 = "B14002_032",
                FPi0 = "B14002_033",
                FPu14 = "B14002_035",
                FPi14 = "B14002_036",
                FPu58 = "B14002_038",
                FPi58 = "B14002_039",
                FPu912 = "B14002_041",
                FPi912 = "B14002_042"
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
the_wiiider_school <- PublicVSPrivate %>% 
  select(GEOID, NAME, variable, estimate) %>% 
  pivot_wider(names_from = variable, values_from = estimate) 

the_wiiider_school <- PublicVSPrivate %>%
  select(GEOID, NAME, variable, estimate, geometry) %>%
  pivot_wider(names_from = variable, values_from = estimate) %>%
  mutate(totalPublic = MPu0 + MPu14 + MPu58 + MPu912 + FPu0 + FPu14 + FPu58 + FPu912) %>% 
  mutate(totalPrivate = MPi0 + MPi14 + MPi58 + MPi912 + FPi0 + FPi14 + FPi58 + FPi912) %>% 
  mutate(totalK12 = MPu0 + MPu14 + MPu58 + MPu912 + FPu0 + FPu14 + FPu58 + FPu912 + 
           MPi0 + MPi14 + MPi58 + MPi912 + FPi0 + FPi14 + FPi58 + FPi912) %>% 
  mutate(PublicPorp = ((totalPublic/totalK12) *100)) %>% 
  mutate(PrivatePorp = ((totalPrivate/totalK12) *100))
  


interpolated_PublicVSPrivate <- interpolate_pw(
  from = st_make_valid(the_wiiider_school),
  to = st_make_valid(schoolBoundry),
  extensive = FALSE,
  weights = st_make_valid(block.total),
  weight_column = "total.pop",
  crs = 4269
)

precinct_interpolated_PublicVSPrivate <- interpolate_pw(
  from = st_make_valid(the_wiiider_school),
  to = st_make_valid(schoolPrecincts),
  extensive = FALSE,
  weights = st_make_valid(block.total),
  weight_column = "total.pop",
  crs = 4269
)



mx4_public <- function(){
  #credit  to the funk master
  palette <- colorNumeric(
    palette = viridisLite::turbo(256), # Yellow-Orange-Red color scale, 
    domain = precinct_interpolated_PublicVSPrivate$PublicPorp,
    na.color = "transparent"
  )
  
  leaflet() %>% 
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(
      data = precinct_interpolated_PublicVSPrivate,
      weight = 1,
      color = "black",
      fillColor = ~palette(PublicPorp),
      fillOpacity = .5,
      opacity = .4
    ) %>% 
    addLegend(
      pal = palette,
      values = precinct_interpolated_PublicVSPrivate$PublicPorp,
      title = "PublicPorp",
      position = "bottomright"
      
    )
}

precinct_interpolated_PublicVSPrivate %>% 
  ggplot(aes(fill = PublicPorp)) +
  geom_sf()+
  scale_fill_viridis_c(option = "turbo") +
  labs(title = "Percent of Students K-12 going to CPS")
  labs(fill = "Percent of K-12 Public School")
