library(readxl)
library(tidyverse)
library(sf)
library(RColorBrewer)
FUXL23 <- read_excel("Data/election results/G23_Official_Canvass.xlsx", 
                                   sheet = 'Boards of Education', skip = 2, n_max = 562)
view(FUXL23)

FUXL21 <- read_excel("Data/election results/G21_Official_Canvass.xlsx", 
                     sheet = 'Boards of Education', skip = 2, n_max = 564)
view(FUXL21)

FUXL19 <- read_excel("Data/election results/G19_Official_Canvass.xlsx", 
                   sheet = 'Boards of Education', skip = 2, n_max=563)
view(FUXL19)

##I love the new shape files so much here is some data cleaning to make them useable
FUXL23$PRECINCT <- substr(FUXL23$PRECINCT, 1, nchar(FUXL23$PRECINCT) - 8)
FUXL21$PRECINCT <- substr(FUXL21$PRECINCT, 1, nchar(FUXL21$PRECINCT) - 8)
FUXL19$PRECINCT <- substr(FUXL19$PRECINCT, 1, nchar(FUXL19$PRECINCT) - 8)

schoolPrecincts <- st_read("shapefiles/cps_precincts.shp")
schoolBoundry <- st_read("shapefiles/cps_boundary.shp")

schoolBoundry <- st_set_crs(schoolBoundry, 4269)
schoolPrecincts <- st_set_crs(schoolPrecincts, 4269)

schoolPrecincts %>% 
  ggplot(aes()) +
  geom_sf()

MapWResults23 <- left_join(schoolPrecincts, FUXL23, by = c("PRECINCT" = "PRECINCT"))
MapWResults21 <- left_join(schoolPrecincts, FUXL21, by = c("PRECINCT" = "PRECINCT"))
MapWResults19 <- left_join(schoolPrecincts, FUXL19, by = c("PRECINCT" = "PRECINCT"))

#TIME FOR DATA WRANGLE
SmallMap <- MapWResults23 %>% 
  mutate(NEW_PERCENT = `BALLOTS CAST TOTAL`/ `REGISTERED VOTERS TOTAL`) %>% 
  select(PRECINCT, `REGISTERED VOTERS TOTAL`, `BALLOTS CAST TOTAL`, NEW_PERCENT, 
         `Eve           Bolton`, `Bryan        Cannon`, `Ben             Lindy`,
         `Kendra        Mapp`, `Paul         Schiele`) %>% 
  mutate(TOT_SCHOOL_BOARD =  `Eve           Bolton` + `Bryan        Cannon` + 
           `Ben             Lindy` + `Kendra        Mapp` + `Paul         Schiele`)  
   

SmallMap21 <- MapWResults21 %>% 
  mutate(NEW_PERCENT = `BALLOTS CAST TOTAL`/ `REGISTERED VOTERS TOTAL`) %>% 
  select(PRECINCT, `REGISTERED VOTERS TOTAL`, PRECINCT, `REGISTERED VOTERS TOTAL`, `BALLOTS CAST TOTAL`, NEW_PERCENT, geometry,
         `Pamela F. Bowers`, `Brandon Craig`, `Gary      Favors`, `Kareem T. Moffett`, `Mike    Moroski`, `Mary Wineberg`) %>% 
  mutate(TOT_SCHOOL_BOARD = `Pamela F. Bowers`+ `Brandon Craig`+ `Gary      Favors`+ `Kareem T. Moffett`+ `Mike    Moroski`+ `Mary Wineberg`)


SmallMap19 <- MapWResults19 %>% 
  mutate(NEW_PERCENT = `BALLOTS CAST TOTAL`/ `REGISTERED VOTERS TOTAL`) %>% 
  select(PRECINCT, `REGISTERED VOTERS TOTAL`, PRECINCT, `REGISTERED VOTERS TOTAL`, `BALLOTS CAST TOTAL`, NEW_PERCENT, geometry,
        `Eve Bolton`, `Marlena Brookfield`, `Heather M. Couch`, `Ozie Davis III (Write-In)`, `Carolyn Jones`, `Ben Lindy`) %>% 
  mutate(TOT_SCHOOL_BOARD = `Eve Bolton` + `Marlena Brookfield` + `Heather M. Couch` + `Ozie Davis III (Write-In)` + `Carolyn Jones` + `Ben Lindy`)

  
BallotSum23 = sum(FUXL23$`BALLOTS CAST TOTAL`, na.rm = T)
BallotSum21 = sum(FUXL21$`BALLOTS CAST TOTAL`, na.rm = T)
BallotSum19 = sum(FUXL19$`BALLOTS CAST TOTAL`, na.rm = T)
#drop off calculation
SmallMap21 <- SmallMap21 %>% 
  mutate(dropOffestimate = (`BALLOTS CAST TOTAL` - (TOT_SCHOOL_BOARD/4))/`BALLOTS CAST TOTAL`)
SmallMap19 <- SmallMap19 %>% 
  mutate(dropOffestimate = (`BALLOTS CAST TOTAL` - (TOT_SCHOOL_BOARD/4))/`BALLOTS CAST TOTAL`)

EDrop21 <-  (sum(SmallMap21$`BALLOTS CAST TOTAL`) - (sum(SmallMap21$TOT_SCHOOL_BOARD)/4))/sum(SmallMap21$`BALLOTS CAST TOTAL`)
EDrop19 <-  (sum(SmallMap19$`BALLOTS CAST TOTAL`) - (sum(SmallMap19$TOT_SCHOOL_BOARD)/4))/sum(SmallMap19$`BALLOTS CAST TOTAL`)
####graphs####
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
