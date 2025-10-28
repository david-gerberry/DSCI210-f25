library(dplyr)
library(gender)
library(stringr)
library(sf)
library(ggplot2)
library(dplyr)
library(tidycensus)
library(tidyverse)

data <- read.csv("data/HAMILTON.txt")
judicial_precincts  <- st_read("shapefiles/judicial_precincts.shp")


data <- data %>%
  mutate(
    FIRST_NAME = iconv(FIRST_NAME, from = "", to = "UTF-8", sub = ""),  # fix encoding
    FIRST_NAME = gsub("[^A-Za-z]", "", FIRST_NAME),                     # remove symbols/numbers
    FIRST_NAME = trimws(FIRST_NAME),                                    # trim whitespace
    FIRST_NAME = tolower(FIRST_NAME),                                    # make lowercase
    LAST_NAME = iconv(LAST_NAME, from = "", to = "UTF-8", sub = ""),  # fix encoding
    LAST_NAME = gsub("[^A-Za-z]", "", LAST_NAME),                     # remove symbols/numbers
    LAST_NAME = trimws(LAST_NAME),                                    # trim whitespace
    LAST_NAME = tolower(LAST_NAME)  
  )

name_gender <- gender(unique(data$FIRST_NAME), method = "ssa")

data <- data %>%
  left_join(name_gender %>% select(name, gender), by = c("FIRST_NAME" = "name"))

data <- data %>%
  mutate(gender = ifelse(is.na(gender), "unknown", gender))

data <- left_join(judicial_precincts, data, by = c("PRC_NAME" = "PRECINCT_NAME"))

wanted <- c(
  "SOS_VOTERID",
  "FIRST_NAME",
  "LAST_NAME",
  "PARTY_AFFILIATION",
  "DATE_OF_BIRTH",
  "REGISTRATION_DATE",
  "RESIDENTIAL_ADDRESS1",
  "RESIDENTIAL_SECONDARY_ADDR",
  "RESIDENTIAL_CITY",
  "RESIDENTIAL_STATE",
  "RESIDENTIAL_ZIP",
  "CITY.y",
  "MUNICIPAL_COURT_DISTRICT",
  "PRC_NAME"
  )

sm_reach <- data %>%
  select(all_of(wanted))

sm_reach$DATE_OF_BIRTH <- as.numeric(floor((Sys.Date() - as.Date(sm_reach$DATE_OF_BIRTH)) / 365.25))
colnames(sm_reach)[colnames(sm_reach) == "DATE_OF_BIRTH"] <- "AGE"

sm_reach <- sm_reach %>%
  filter(AGE <= 29, CITY.y == "NORWOOD CITY")

write.csv(sm_reach, "sm_reach.csv", row.names = FALSE)
