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
defaultVoterList <-  read_csv("data/VoterListCincySchoolBoard.csv")


  Cade2VoterList <- defaultVoterList %>% 
    filter(School == "SCCISD") %>% 
    mutate(Age = 2025 - BirthYear) %>% 
    filter(Age > 27 & Age < 46) %>% 
    filter(PrecinctNum %in% PRE_LIST)
    ##the Big Filter
    
    
  # in our race and isn't affiliated republican1


mutate(FullAddress = paste(AddressNumber,AddressStreet,AddressSuffix)) 



Cade2VoterList %>% 
  count(PrecinctName)



