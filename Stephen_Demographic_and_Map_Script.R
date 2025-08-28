library(tidyverse)
library(tidycensus)
library(ggplot2)
library(sf)



#### Pulling Census Data ####

census_api_key("4951cbe6bdec5269e91df7656ba86ee58a29cff7")

Census_Data <- get_acs(
  geography = "block group",
  variables = c(
    med_household_income = "B19013_001"#,
    #median_age_pop = "B01002_001",
    #median_age_fam_hh = "B11007_002",      
    #median_age_nonfam_hh = "B11007_003" 
  ), 
  state = "OH",
  county= "Hamilton",
  year = 2023,
  geometry = TRUE
)

Census_Data_2 <- get_acs(
  geography = "block group",
  variables = c(
    #med_household_income = "B19013_001"#,
    median_age_pop = "B01002_001",
    median_age_fam_hh = "B11007_002",      
    median_age_nonfam_hh = "B11007_003" 
  ), 
  state = "OH",
  county= "Hamilton",
  year = 2023,
  geometry = TRUE
)


#### Map Making ####



households <- get_acs(geography = "block group", 
                             state = "Ohio",
                             county = "Hamilton",
                             variables = "B11001_001",
                             year = 2023,
                             geometry = TRUE) %>% 
  select(GEOID, households = estimate) 


population <- get_acs(geography = "block group",
                                state = "OH",
                                county = "Hamilton",
                                variables = "B01003_001",   # Total population
                                year = 2023,
                                geometry = TRUE) %>%
  select(GEOID, total.pop = estimate)



cincy_precincts <- st_read("shapefiles/cincy_precincts.shp")
cincy_boundary <- st_read("shapefiles/cincy_boundary.shp")

cincy_boundary <- st_set_crs(cincy_boundary, 4269)
cincy_precincts <- st_set_crs(cincy_precincts, 4269)



#### Interpolation ####


precinct_household_income_interpolated <- interpolate_pw(
  from = st_make_valid(Census_Data),
  to = st_make_valid(cincy_precincts),
  extensive = FALSE,                 ## because you don't want to add the median ages,
  weights =st_make_valid(households),
  weight_column = "households",
  crs = 4269
) %>% rename(med_household_income = estimate)



#Interpolate median ages
precinct_age_fam_interpolated <- interpolate_pw(
  from = st_make_valid(Census_Data),
  to = st_make_valid(cincy_precincts),
  extensive = FALSE,
  weights = st_make_valid(population),     # Use population weights
  weight_column = "total.pop",
  crs = 4269
) %>% rename(median_age_fam_hh = estimate)

precinct_age_nonfam_interpolated <- interpolate_pw(
  from = st_make_valid(Census_Data),
  to = st_make_valid(cincy_precincts),
  extensive = FALSE,
  weights = st_make_valid(population),     # Use population weights
  weight_column = "total.pop",
  crs = 4269
) %>% rename(median_age_nonfam_hh = estimate)


#### Plotting Maps ####


# Family householders this is all fucked
precinct_age_fam_interpolated %>%
  ggplot(aes(fill = median_age_fam_hh)) +
  geom_sf(color = NA) +
  scale_fill_viridis_c(option = "magma") +
  labs(fill = "Median Age (Family HH)") +
  theme_minimal()

# Non-family householders this is all fucked 
precinct_age_nonfam_interpolated %>%
  ggplot(aes(fill = median_age_nonfam_hh)) +
  geom_sf(color = NA) +
  scale_fill_viridis_c(option = "turbo") +
  labs(fill = "Median Age (Nonfamily HH)") +
  theme_minimal()


#household Income only thing that works
precinct_household_income_interpolated %>%
  ggplot(aes(fill = med_household_income)) +
  geom_sf(color = NA) +
  scale_fill_viridis_c(option = "turbo") +
  labs(fill = "Median Household Income ($)") +
  theme_minimal()

