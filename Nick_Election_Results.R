library(readxl)
library(tidyverse)
library(sf)
library(RColorBrewer)


FUXL23 <- read_excel("Data/election results/G23_Official_Canvass.xlsx", 
                                   sheet = 'Boards of Education', skip = 2, n_max = 562)


FUXL21 <- read_excel("Data/election results/G21_Official_Canvass.xlsx", 
                     sheet = 'Boards of Education', skip = 2, n_max = 564)


FUXL19 <- read_excel("Data/election results/G19_Official_Canvass.xlsx", 
                   sheet = 'Boards of Education', skip = 2, n_max=563)





schoolPrecincts <- st_read("shapefiles/cps_precincts.shp")
schoolBoundry <- st_read("shapefiles/cps_boundary.shp")

##I love the new shape files so much here is some data cleaning to make them useable
FUXL23$PRECINCT <- substr(FUXL23$PRECINCT, 1, 4)
FUXL21$PRECINCT <- substr(FUXL21$PRECINCT, 1, 4)
FUXL19$PRECINCT <- substr(FUXL19$PRECINCT, 1, 4)

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
           `Ben             Lindy` + `Kendra        Mapp` + `Paul         Schiele`) %>%  
   filter(TOT_SCHOOL_BOARD != 0)

SmallMap21 <- MapWResults21 %>% 
  mutate(NEW_PERCENT = `BALLOTS CAST TOTAL`/ `REGISTERED VOTERS TOTAL`) %>% 
  select(PRECINCT, `REGISTERED VOTERS TOTAL`, PRECINCT, `REGISTERED VOTERS TOTAL`, `BALLOTS CAST TOTAL`, NEW_PERCENT, geometry,
         `Pamela F. Bowers`, `Brandon Craig`, `Gary      Favors`, `Kareem T. Moffett`, `Mike    Moroski`, `Mary Wineberg`) %>% 
  mutate(TOT_SCHOOL_BOARD = `Pamela F. Bowers`+ `Brandon Craig`+ `Gary      Favors`+ `Kareem T. Moffett`+ `Mike    Moroski`+ `Mary Wineberg`) %>%  
  filter(TOT_SCHOOL_BOARD != 0)


SmallMap19 <- MapWResults19 %>% 
  mutate(NEW_PERCENT = `BALLOTS CAST TOTAL`/ `REGISTERED VOTERS TOTAL`) %>% 
  select(PRECINCT, `REGISTERED VOTERS TOTAL`, PRECINCT, `REGISTERED VOTERS TOTAL`, `BALLOTS CAST TOTAL`, NEW_PERCENT, geometry,
        `Eve Bolton`, `Marlena Brookfield`, `Heather M. Couch`, `Ozie Davis III (Write-In)`, `Carolyn Jones`, `Ben Lindy`) %>% 
  mutate(TOT_SCHOOL_BOARD = `Eve Bolton` + `Marlena Brookfield` + `Heather M. Couch` + `Ozie Davis III (Write-In)` + `Carolyn Jones` + `Ben Lindy`) %>%  
  filter(TOT_SCHOOL_BOARD != 0)

  
BallotSum23 = sum(FUXL23$`BALLOTS CAST TOTAL`, na.rm = T)
BallotSum21 = sum(FUXL21$`BALLOTS CAST TOTAL`, na.rm = T)
BallotSum19 = sum(FUXL19$`BALLOTS CAST TOTAL`, na.rm = T)

VoteSum21 = sum(FUXL21$`REGISTERED VOTERS TOTAL`)
VoteSum19 = sum(FUXL19$`REGISTERED VOTERS TOTAL`)
#drop off calculation
SmallMap21 <- SmallMap21 %>% 
  mutate(dropOffestimate = (`BALLOTS CAST TOTAL` - (TOT_SCHOOL_BOARD/4))/`BALLOTS CAST TOTAL`)
SmallMap19 <- SmallMap19 %>% 
  mutate(dropOffestimate = (`BALLOTS CAST TOTAL` - (TOT_SCHOOL_BOARD/4))/`BALLOTS CAST TOTAL`)

EDrop21 <-  (sum(SmallMap21$`BALLOTS CAST TOTAL`) - (sum(SmallMap21$TOT_SCHOOL_BOARD)/4))/sum(SmallMap21$`BALLOTS CAST TOTAL`)
EDrop19 <-  (sum(SmallMap19$`BALLOTS CAST TOTAL`) - (sum(SmallMap19$TOT_SCHOOL_BOARD)/4))/sum(SmallMap19$`BALLOTS CAST TOTAL`)


##getting precent proxy
  SmallMap <- SmallMap %>% 
    mutate(PrecentPorxy = `Kendra        Mapp`/`BALLOTS CAST TOTAL`)
  
  
  SmallMap19 <- SmallMap19 %>% 
    mutate(PrecentPorxy = `Ben Lindy`/`BALLOTS CAST TOTAL`)
  
  AvgMap <- SmallMap %>%
    select(PRECINCT, PrecentPorxy) %>% 
    mutate(PrecentPorxy23 = PrecentPorxy) %>% 
    select(PRECINCT, PrecentPorxy23)
  
  
  tempMap <- SmallMap19 %>%
    st_set_geometry(NULL) %>%   
    select(PRECINCT, PrecentPorxy, dropOffestimate, NEW_PERCENT) %>% 
    mutate(PrecentPorxy19 = PrecentPorxy) %>% 
    mutate(dropOffestimate19 = dropOffestimate) %>% 
    mutate(turnout19 = NEW_PERCENT) %>% 
    select(PRECINCT, PrecentPorxy19, dropOffestimate19, turnout19, NEW_PERCENT)
  
  tempMap21 <- SmallMap21 %>% 
    st_set_geometry(NULL) %>%
    select(dropOffestimate, PRECINCT, NEW_PERCENT) %>% 
    mutate(dropOffestimate21 = dropOffestimate) %>% 
    mutate(turnout21 = NEW_PERCENT) %>% 
    select(PRECINCT, dropOffestimate21, turnout21)
    
  
  # Join on PRECINCT, geometry is preserved
  AvgMap <- AvgMap %>%
    left_join(tempMap, by = "PRECINCT")
  AvgMap <- AvgMap %>%
    left_join(tempMap21, by = "PRECINCT")
  
  AvgMap <- AvgMap %>% 
    mutate(AvgProx = (PrecentPorxy23 + PrecentPorxy19)/2) %>% 
    mutate(AvgDrop = (dropOffestimate21 + dropOffestimate19)/2) %>% 
    mutate(AvgTurn = ((turnout19 + turnout21)/2)*100)
  
  #turn out
  AvgMap %>% 
    replace(is.na(.), 0) %>% 
    ggplot(aes(fill = (turnout19 + turnout21)/2 )) +
    geom_sf() +
    scale_fill_viridis_c(option = "turbo") +
    labs(title = "Avrage Voter Turn out By Precinct for Cincinnati School Board") +
    labs(fill = "Voter Turn out") +
    theme(
      legend.text = element_text(size = 14),   # bigger legend labels
      legend.title = element_text(size = 16)   # bigger legend title
    )
  #dropoff
  AvgMap %>% 
    ggplot(aes(fill = (dropOffestimate21 + dropOffestimate19)/2)) +
    geom_sf() +
    scale_fill_viridis_c(option = "turbo") +
    labs(title = "Avrage Voter Turn out By Precinct for Cincinnati School Board") +
    labs(fill = "Voter Turn out") +
    theme(
      legend.text = element_text(size = 14),   # bigger legend labels
      legend.title = element_text(size = 16)   # bigger legend title
    )
  
  
  
  ####graphs####
  SmallMap %>% 
    ggplot(aes(fill = NEW_PERCENT)) +
    geom_sf() +
    scale_fill_viridis_c(option = "turbo") +
    labs(title = "Voter Turn out By Precinct for 2023 Cincinnati School Board") +
    labs(fill = "Voter Turn out") 
  
  SmallMap19 %>% 
    ggplot(aes(fill = NEW_PERCENT)) +
    geom_sf() +
    scale_fill_viridis_c(option = "turbo") +
    labs(title = "Voter Turn out By Precinct for 2019 Cincinnati School Board") +
    labs(fill = "Voter Turn out") 
  
  SmallMap19 %>% 
    ggplot(aes(fill = dropOffestimate)) +
    geom_sf() +
    scale_fill_viridis_c(option = "turbo") +
    labs(title = "drop off By Precinct for 2019 Cincinnati School Board") +
    labs(fill = "drop off") 
  
  
  
  

