library(readxl)
library(tidyverse)
library(sf)
library(RColorBrewer)
FUXL23 <- read_excel("Data/election results/G23_Official_Canvass.xlsx", 
                                   sheet = 'Boards of Education', skip = 2)
view(FUXL23)

FUXL19 <- read_excel("Data/election results/G19_Official_Canvass.xlsx", 
                   sheet = 'Boards of Education', skip = 2)
view(FUXL19)



schoolPrecincts <- st_read("shapefiles/cps_precincts.shp")
schoolBoundry <- st_read("shapefiles/cps_boundary.shp")

schoolBoundry <- st_set_crs(schoolBoundry, 4269)
schoolPrecincts <- st_set_crs(schoolPrecincts, 4269)

schoolPrecincts %>% 
  ggplot(aes()) +
  geom_sf()

MapWResults <- left_join(schoolPrecincts, FUXL23, by = c("PRECINCT" = "PRECINCT"))

#TIME FOR DATA WRANGLE
SmallMap <- MapWResults %>% 
  mutate(NEW_PERCENT = `BALLOTS CAST TOTAL`/ `REGISTERED VOTERS TOTAL`) %>% 
  select(PRECINCT, `REGISTERED VOTERS TOTAL`, `BALLOTS CAST TOTAL`, NEW_PERCENT, 
         `Eve           Bolton`, `Bryan        Cannon`, `Ben             Lindy`,
         `Kendra        Mapp`, `Paul         Schiele`) %>% 
  mutate(TOT_SCHOOL_BOARD = `Eve           Bolton` + `Bryan        Cannon` + 
           `Ben             Lindy` + `Kendra        Mapp` + `Paul         Schiele`) %>% 
  filter(TOT_SCHOOL_BOARD != 0) 
  

 SmallMap %>% 
  ggplot(aes(fill = NEW_PERCENT)) +
  geom_sf() +
  scale_fill_viridis_c(option = "turbo") +
  labs(title = "Voter Turn out By Precinct for 2023 Cincinnati School Board") +
  labs(fill = "Voter Turn out") 
  
  SmallMap <- SmallMap %>% 
    mutate(PrecentPorxy = `Kendra        Mapp`/`BALLOTS CAST TOTAL`)
  
  SmallMap %>% 
    ggplot(aes(fill = PrecentPorxy)) +
    geom_sf() +
    scale_fill_viridis_c(option = "turbo") +
    labs(title = "Votes for Mapp") +
    labs(fill = "Percent of Vote") 
  
  SmallMap %>%
    mutate(MappBaseSwing = cut(PrecentPorxy, breaks = c(-0.001, .35, .50, 1), label = c('Residual', 'Swing', 'Base'))) %>% 
    ggplot(aes(fill = MappBaseSwing)) +
    geom_sf() +
    scale_fill_manual(
      values = c(
        "Residual" = "red",   
        "Base" = "blue",   
        "Swing" = "gold"   
      )
    )
    