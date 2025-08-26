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


##make the data wide
the_wiiide_school = demographicData %>% 
  select(GEOID, NAME, variable, estimate) %>% 
  pivot_wider(names_from = variable, values_from = estimate) 

demographicData_wide <- demographicData %>%
  select(GEOID, NAME, variable, estimate, geometry) %>%
  pivot_wider(names_from = variable, values_from = estimate) %>%
  mutate(total_parent_household = married_parents + single_dads + single_moms) %>% 
  mutate(parent_porp = total_parent_household/ population)

# Read custom school precinct shapefile
schoolPrecincts <- st_read("shapefiles/cps_precincts.shp")
schoolPrecincts <- st_set_crs(schoolPrecincts, 4269)

# Align CRS
schoolPrecincts <- st_transform(schoolPrecincts, st_crs(demographicData_wide))

# Spatial join: attach ACS tracts to school precincts
acs_joined <- st_join(demographicData_wide, schoolPrecincts, join = st_intersects)

# Aggregate to one row per precinct
acs_by_precinct <- acs_joined %>%
  group_by(geometry) %>%  # Replace with your shapefile's precinct name column
  summarise(
    married_parents = sum(married_parents, na.rm = TRUE),
    single_dads = sum(single_dads, na.rm = TRUE),
    single_moms = sum(single_moms, na.rm = TRUE),
    total_parent_household = sum(total_parent_household, na.rm = TRUE),
    total_population = sum(population, na.rm = TRUE)   # sum total households/population
  ) %>% 
  mutate(parent_per = (total_parent_household/ total_population)*100 )

# Map example


ggplot(acs_by_precinct) +
  geom_sf(aes(fill = parent_per)) +
  scale_fill_viridis_c(trans = "sqrt") +
  theme_minimal() +
  labs(title = "Percent of households with parents by Precinct")



acs_clipped <- st_intersection(acs_by_precinct, schoolPrecincts)

ggplot(acs_clipped) +
  geom_sf(aes(fill = parent_per)) +
  scale_fill_viridis_c(labels = scales::percent_format(accuracy = 1, trans = "sqrt")) +
  theme_minimal() +
  labs(title = "Percent of households with parents by Precinct")