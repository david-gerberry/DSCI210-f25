
library(dplyr)
library(gender)
library(stringr)


# Read the text file (comma-delimited)
data <- read.csv("data/HAMILTON.txt")

wanted <- c(
  "SOS_VOTERID",
  "FIRST_NAME",
  "LAST_NAME",
  "PARTY_AFFILIATION",
  "DATE_OF_BIRTH",
  "gender",
  "REGISTRATION_DATE",
  "CITY",
  "CITY_SCHOOL_DISTRICT",
  "MUNICIPAL_COURT_DISTRICT",
  "PRECINCT_NAME",
  "TOWNSHIP",
  "VILLAGE",
  "WARD",
  "GENERAL.11.06.2001", "GENERAL.11.04.2003", "PRIMARY.05.03.2005",
  "PRIMARY.09.13.2005", "GENERAL.11.08.2005", "PRIMARY.05.08.2007",
  "PRIMARY.09.11.2007", "GENERAL.11.06.2007", "PRIMARY.05.05.2009",
  "PRIMARY.09.08.2009", "PRIMARY.09.15.2009", "PRIMARY.09.29.2009",
  "GENERAL.11.03.2009", "PRIMARY.05.03.2011", "PRIMARY.09.13.2011",
  "GENERAL.11.08.2011", "PRIMARY.05.07.2013", "PRIMARY.09.10.2013",
  "PRIMARY.10.01.2013", "GENERAL.11.05.2013", "PRIMARY.05.05.2015",
  "PRIMARY.09.15.2015", "GENERAL.11.03.2015",
  "PRIMARY.05.02.2017", "PRIMARY.09.12.2017", "GENERAL.11.07.2017",
  "PRIMARY.05.07.2019", "PRIMARY.09.10.2019", "GENERAL.11.05.2019",
  "PRIMARY.05.04.2021", "PRIMARY.08.03.2021", "GENERAL.11.02.2021",
  "PRIMARY.05.02.2023", "PRIMARY.10.03.2023", "GENERAL.11.07.2023",
  "PRIMARY.05.06.2025", "PRIMARY.09.09.2025" # example name for this year's primary — replace with your exact column name
)

predict_data <- data %>%
  select(all_of(wanted))

data <- data %>%
  mutate(
    FIRST_NAME = iconv(FIRST_NAME, from = "", to = "UTF-8", sub = ""),  # fix encoding
    FIRST_NAME = gsub("[^A-Za-z]", "", FIRST_NAME),                     # remove symbols/numbers
    FIRST_NAME = trimws(FIRST_NAME),                                    # trim whitespace
    FIRST_NAME = tolower(FIRST_NAME)                                    # make lowercase
  )

name_gender <- gender(unique(data$FIRST_NAME), method = "ssa")

data <- data %>%
  left_join(name_gender %>% select(name, gender), by = c("FIRST_NAME" = "name"))

predict_data$will_vote_2025 <- sample(c(0, 1), nrow(predict_data), replace = TRUE, prob = c(0.65, 0.35))
