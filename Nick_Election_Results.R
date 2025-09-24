library(readxl)
library(tidyverse)
library(sf)
library(RColorBrewer)
FUXL <- read_excel("Data/election results/G23_Official_Canvass.xlsx", 
                                   sheet = 'Boards of Education', skip = 2)
view(FUXL)

schoolPrecincts <- st_read("shapefiles/cps_precincts.shp")
schoolBoundry <- st_read("shapefiles/cps_boundary.shp")

schoolBoundry <- st_set_crs(schoolBoundry, 4269)
schoolPrecincts <- st_set_crs(schoolPrecincts, 4269)

schoolPrecincts %>% 
  ggplot(aes()) +
  geom_sf()

MapWResults <- left_join(schoolPrecincts, FUXL, by = c("PRECINCT" = "PRECINCT"))

MapWResults <- MapWResults %>% 
  mutate(NEW_PERCENT = PERCENT * 100)

MapWResults %>% 
  ggplot(aes(fill = NEW_PERCENT)) +
  geom_sf() +
  scale_fill_viridis_c(option = "turbo") +
  labs(title = "Voter Turn out By Precinct for 2023 Cincinnati School Board") +
  labs(fill = "Voter Turn out") 