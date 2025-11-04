library(tidyverse)
library(sf)
library(readxl)
library(tidygeocoder)
library(ggtext)

voterDFClean = read_csv("data/voter_df_clean.csv")
#### DELIVERABLES ####

#* 1: What is our demographic? 
#* 2: Can we use this demographic to get an ACTUAL list of voters?
  #* 2.1: Map this list onto a map, darker = more voters


#### DEMOGRAPHIC ####

  #* Strong Democrats who havent voted in a bit (might not know)
  #* Strong Democrats in swing areas
  #* Norwood and Anderson


#### LIST ####

# get our weirdly named data
baseVoterList = read_csv("data/VoterListMunicipalCourt4.csv")

results2023 <- read_excel("data/election results/G23_Official_Canvass.xlsx", 
                          sheet = "Judicial",skip=2)
mapANDresults2023 <-
  left_join(acs_interp_judicial, results2023, by = c("PRECINCT" = "PRECINCT"))

mapANDresults2023 %>% 
  mutate(DemProp = `Samantha Silverstein`/( `Samantha Silverstein`+ `Curt           Kissinger`)) %>%
  mutate(DemBaseSwing = cut(DemProp, breaks = c(-0.001, 0.2,.40,.45,.55, 0.60, 0.8,1),labels = c("Very Residual","Residual","Leaning Residual", "Swing", "Leaning Base", "Base","Very Base"))) %>% 
  select(PRECINCT,DemProp,DemBaseSwing)


cadeVoterList = baseVoterList %>% 
  # in our race and isn't affiliated republican
  filter(PartyCode != "R") %>%
  # voted (EITHER 2024/2023) AND (voted 2021)
  filter(!is.na(`2024 General Election`) | !is.na(`2023 General Election`) ) %>%
  filter(!is.na(GENERAL_NOV_2021))  %>% 
  # ONLY Anderson and Norwood
  filter( grepl("NORWOOD|ANDERSON",PrecinctName ) ) %>% 
  # bonus column :)
  mutate(FullAddress = paste(AddressNumber,AddressStreet,AddressSuffix)) 


cadeVoterList23 = baseVoterList %>% 
  # in our race and isn't affiliated republican
  filter(PartyCode != "R") %>%
  # voted (EITHER 2024/2023) AND (voted 2021)
  filter(!is.na(`2023 General Election`) ) %>%
  # ONLY Anderson and Norwood
  filter( grepl("NORWOOD|ANDERSON",PrecinctName ) ) %>% 
  # bonus column :)
  mutate(FullAddress = paste(AddressNumber,AddressStreet,AddressSuffix)) 

# CREATE GEOMETRY
judicialGeometry = acs_interp_judicial %>% 
  select(PRECINCT,geometry)

# CREATE PRECINCT MAPS
cadeVoterList = cadeVoterList %>% 
  mutate(
    PrecinctNumber = str_pad(PrecinctNum, 4, pad = "0"),
  
    AbbrevName = ifelse( str_sub(PrecinctName, 1, 4) == "NORW",
                  str_to_upper(str_sub(PrecinctName, 1, 4)), # is norwood
                  str_to_upper(str_sub(PrecinctName, 1, 3)) # isnt norwood
    ),
    Suffix = str_trim(str_remove(PrecinctName, "^[A-Za-z]+\\s+")),
    FullPrecinct = str_trim(paste(PrecinctNumber, AbbrevName, Suffix))
 )
cadeVoterList23 = cadeVoterList23 %>% 
  mutate(
    PrecinctNumber = str_pad(PrecinctNum, 4, pad = "0"),
    
    AbbrevName = ifelse( str_sub(PrecinctName, 1, 4) == "NORW",
                         str_to_upper(str_sub(PrecinctName, 1, 4)), # is norwood
                         str_to_upper(str_sub(PrecinctName, 1, 3)) # isnt norwood
    ),
    Suffix = str_trim(str_remove(PrecinctName, "^[A-Za-z]+\\s+")),
    FullPrecinct = str_trim(paste(PrecinctNumber, AbbrevName, Suffix))
  )

# count the precincts
countedPrecincts = cadeVoterList %>% 
  count(FullPrecinct, name = "VoterAmount")
countedPrecincts23 = cadeVoterList23 %>% 
  count(FullPrecinct, name = "VoterAmount")
# add them to a new geometry dataset
countedGeometry = judicialGeometry %>%
  left_join(countedPrecincts, by = c("PRECINCT" = "FullPrecinct")) %>% 
  mutate(VoterAmount = replace_na(VoterAmount, 0))
countedGeometry23 = judicialGeometry %>%
  left_join(countedPrecincts23, by = c("PRECINCT" = "FullPrecinct")) %>% 
  mutate(VoterAmount = replace_na(VoterAmount, 0))

### MAP 1!!!!!!!!!
countedGeometry %>% 
  ggplot() +
  geom_sf(aes(fill = VoterAmount), color = "grey", size = 0.1) +
  scale_fill_gradient( 
    low = "#C7DDFF",
    high = "darkblue",
    breaks = c(400,350,300,250,200,150,100,50,0),
    labels = c("400", "350","300","250","200","150","100","50","0")
  ) +
  geom_sf_text(
    data = subset(countedGeometry, VoterAmount >= 225),  # 👈 only show where >= 100
    aes(label = PRECINCT),
    size = 3,
    color = "white",
    stroke = 0.4
  ) +labs(
    title = "Voters we can expect to come out",
    fill = "Voter Amount"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "transparent"),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )
### MAP 2!!!!!!!!!
countedGeometry23 %>% 
  ggplot() +
  geom_sf(aes(fill = VoterAmount), color = "grey", size = 0.1) +
  scale_fill_gradient( 
    low = "#C7DDFF",
    high = "darkblue",
    breaks = c(800,700,600,500,400,300,200,100,0),
    labels = c("800", "700","600","500","400","300","200","100","0")
  ) +
  labs(
    title = "Voter Distribution by Precinct (2023)",
    fill = "Voter Amount"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "transparent"),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )



# 0101 CIN 1-A
# 2619 + CINCINNATI 26-S

install.packages("shadowtext")   # only once
library(shadowtext)


# SUPER GRAPH 1
countedGeometry <- countedGeometry %>%
  mutate(centroid = st_point_on_surface(geometry)) %>%  # point guaranteed inside polygon
  mutate(
    lon = st_coordinates(centroid)[,1],
    lat = st_coordinates(centroid)[,2]
  )
ggplot() +
  geom_sf(data = countedGeometry,
          aes(fill = VoterAmount),
          color = "grey", size = 0.1) +
  scale_fill_gradient(
    low = "#C7DDFF",
    high = "darkblue",
    breaks = c(400, 350, 300, 250, 200, 150, 100, 50, 0),
    labels = c("400", "350", "300", "250", "200", "150", "100", "50", "0")
  ) +
  geom_shadowtext(
    data = subset(countedGeometry, VoterAmount >= 300),  # only label higher turnout precincts
    aes(x = lon, y = lat, label = PRECINCT),
    size = 2.5,
    color = "white",        # text color
    bg.color = "black",     # outline color
    bg.r = 0.15             # stroke thickness
  ) +
  labs(
    title = "Voters We Can Expect to Come Out",
    fill  = "Voter Amount"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "transparent"),
    axis.text  = element_blank(),
    axis.ticks = element_blank()
  ) +
  coord_sf()

# SUPER GRAPH 2
countedGeometry23 <- countedGeometry23 %>%
  mutate(centroid = st_point_on_surface(geometry)) %>%  # point guaranteed inside polygon
  mutate(
    lon = st_coordinates(centroid)[,1],
    lat = st_coordinates(centroid)[,2]
  )
ggplot() +
  geom_sf(data = countedGeometry23,
          aes(fill = VoterAmount),
          color = "grey", size = 0.1) +
  scale_fill_gradient( 
    low = "#C7DDFF",
    high = "darkblue",
    breaks = c(800,700,600,500,400,300,200,100,0),
    labels = c("800", "700","600","500","400","300","200","100","0")
  ) +
  geom_shadowtext(
    data = subset(countedGeometry23, VoterAmount >= 625),  # only label higher turnout precincts
    aes(x = lon, y = lat, label = PRECINCT),
    size = 2.5,
    color = "white",        # text color
    bg.color = "black",     # outline color
    bg.r = 0.15             # stroke thickness
  ) +
  labs(
    title = "Voters We Want to Get to Come Out",
    fill  = "Voter Amount"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "transparent"),
    axis.text  = element_blank(),
    axis.ticks = element_blank()
  ) +
  coord_sf()
