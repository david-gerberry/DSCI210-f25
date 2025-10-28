library(tidyverse)
library(sf)
library(readxl)
library(tidygeocoder)

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
baseVoterList = read_csv("data/VoterListExport-20250911-CICINC-CI-pn.csv")

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
  filter(PartyCode != "R" & Judicial == "JDMC04") %>%
  # voted EITHER 2024/2023 AND voted 2021
  filter(!is.na(`2024 General Election`) | !is.na(`2023 General Election`) ) %>%
  filter(!is.na(GENERAL_NOV_2021)) 
  # ONLY anderson and noorwood
  mutate(FullAddress = paste(AddressNumber,AddressStreet,AddressSuffix)) 
  
# LONG ASS CODE TIME
voterListSample <- cadeVoterList[sample(nrow(cadeVoterList), size = 30), ]

voterListSample = voterListSample %>% 
  #mutate(AddressLocation = geo(address = FullAddress)) 
  mutate(AddressLocation = geo(,))
geo()
  

# 0101 CIN 1-A
# 2619 + CINCINNATI 26-S
