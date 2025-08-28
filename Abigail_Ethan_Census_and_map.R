# Install if not already installed
# install.packages(c("sf", "tidycensus", "ggplot2", "dplyr"))

library(sf)
library(ggplot2)
library(dplyr)
library(tidycensus)
library(tidyverse)

# 1. Read shapefiles (replace paths with your actual files)
judicial_boundaries <- st_read("shapefiles/judicial_boundary.shp")
judicial_precincts  <- st_read("shapefiles/judicial_precincts.shp")

# make sure CRS is the same
judicial_boundaries <- st_set_crs(judicial_boundaries, 4269)
judicial_precincts  <- st_set_crs(judicial_precincts, 4269)

# 3. Get ACS census data (example: median household income by census tract)
# You need to set your Census API key once: census_api_key("YOUR_KEY", install = TRUE)
acs_data1 <- get_acs(
  geography = "block group",
  variables = c(
    pop_total = "B01003_001",
    white = "B02001_002",
    black = "B02001_003",
    asian = "B02001_005",
    hispanic = "B03003_003"
  ),
  state = "OH",         # change state if needed
  year = 2023,
  geometry = TRUE,
  output = "wide"       # keeps each variable in its own column
)

acs_data2 <- get_acs(
  geography = "block group",
  variables = c(
    median_age = "B01002_001",
    med_income = "B19013_001"
  ),
  state = "OH",         # change state if needed
  year = 2023,
  geometry = TRUE,
  output = "wide"       # keeps each variable in its own column
)


block.total <-get_decennial(geography = "block",
                            state = "Ohio",
                            county = "Hamilton",
                            variables = "P1_001N",
                            year = 2020,
                            sumfile = "dhc",
                            geometry = TRUE) %>%
    select(pop_totalE = value)

acs_data1 <- acs_data1[!st_is_empty(acs_data1), ]
acs_data2 <- acs_data2[!st_is_empty(acs_data2), ]
judicial_boundaries <- judicial_boundaries[!st_is_empty(judicial_boundaries), ]
block.total <- block.total[!st_is_empty(block.total), ]

acs_data1_interpolated <- interpolate_pw(
  from = st_make_valid(acs_data1),
  to = st_make_valid(judicial_precincts),
  extensive = TRUE,
  weights = st_make_valid(block.total),
  weight_column = "pop_totalE",
  crs = 4269
) 

acs_data2_interpolated <- interpolate_pw(
  from = st_make_valid(acs_data2),
  to = st_make_valid(judicial_precincts),
  extensive = FALSE,
  weights = st_make_valid(block.total),
  weight_column = "pop_totalE",
  crs = 4269
) 

acs_data_interpolated <- acs_data1_interpolated %>%
  left_join(st_drop_geometry(acs_data2_interpolated), by = "id")


# 5. Plot precincts with ACS data
acs_data_MHI <- acs_data_interpolated %>%
  ggplot(aes(fill = med_incomeE))+
  geom_sf()+
  scale_fill_viridis_c(option = "turbo")+
  labs(fill = "Median Household Income")

acs_data_total_pop <- acs_data_interpolated %>%
  ggplot(aes(fill = pop_totalE))+
  geom_sf()+
  scale_fill_viridis_c(option = "turbo")+
  labs(fill = "Total Population")


total_pop <- sum(acs_data1_interpolated$pop_totalE)

total_pop











