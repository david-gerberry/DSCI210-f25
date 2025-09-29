#### Libraries ####

library(tidyverse)
library(readxl)
library(scales)

#### Data ####

load("data/acs_data.RData")

rm(acs_interp_cincy)
rm(acs_interp_cps)
rm(acs_interp_ham)

data_24 <- read_excel("data/election results/G24_Official_Canvass.xlsx")
data_23 <- read_excel("data/election results/G23_Official_Canvass.xlsx")
data_22 <- read_excel("data/election results/G22_Official_Canvass.xlsx")
data_21 <- read_excel("data/election results/G21_Official_Canvass.xlsx")
data_20 <- read_excel("data/election results/G20_Official_Canvass.xlsx")
data_19 <- read_excel("data/election results/G19_Official_Canvass.xlsx")
data_18 <- read_excel("data/election results/G18_Official_Amended_Canvass.xlsx")


name_columns <- function(df) {
  colnames(df) <- as.character(unlist(df[1, ]))  
  df <- df[-1, ]                                
  return(df)
}

data_24 <- name_columns(data_24)
data_23 <- name_columns(data_23)
data_23 <- name_columns(data_23)
data_22 <- name_columns(data_22)
data_21 <- name_columns(data_21)
data_21 <- name_columns(data_21)
data_20 <- name_columns(data_20)
data_19 <- name_columns(data_19)
data_19 <- name_columns(data_19)
data_18 <- name_columns(data_18)
data_18 <- name_columns(data_18)

#### Cleaning ####

data_18 <- data_18[ , c(2, 3, 4)]

data_18 <- data_18 %>%
  filter(PRECINCT %in% acs_interp_judicial$PRECINCT)

data_19 <- data_19[ , c(2, 3, 4, 12, 13)]

data_19 <- data_19 %>%
  filter(PRECINCT %in% acs_interp_judicial$PRECINCT)

data_20 <- data_20[ , c(2, 3, 4)]

data_20 <- data_20 %>%
  filter(PRECINCT %in% acs_interp_judicial$PRECINCT)

data_21 <- data_21[ , c(2, 3, 4)]

data_21 <- data_21 %>%
  filter(PRECINCT %in% acs_interp_judicial$PRECINCT)

data_22 <- data_22[ , c(2, 3, 4)]

data_22 <- data_22 %>%
  filter(PRECINCT %in% acs_interp_judicial$PRECINCT)

data_23 <- data_23[ , c(2, 3, 4, 9, 10)]

data_23 <- data_23 %>%
  filter(PRECINCT %in% acs_interp_judicial$PRECINCT)

data_24 <- data_24[ , c(2, 3, 4)]

data_24 <- data_24 %>%
  filter(PRECINCT %in% acs_interp_judicial$PRECINCT)

data_18$`BALLOTS CAST TOTAL` <- as.numeric(data_18$`BALLOTS CAST TOTAL`)
data_18$`REGISTERED VOTERS TOTAL` <- as.numeric(data_18$`REGISTERED VOTERS TOTAL`)

data_19$`BALLOTS CAST TOTAL` <- as.numeric(data_19$`BALLOTS CAST TOTAL`)
data_19$`REGISTERED VOTERS TOTAL` <- as.numeric(data_19$`REGISTERED VOTERS TOTAL`)

data_20$`BALLOTS CAST TOTAL` <- as.numeric(data_20$`BALLOTS CAST TOTAL`)
data_20$`REGISTERED VOTERS TOTAL` <- as.numeric(data_20$`REGISTERED VOTERS TOTAL`)

data_21$`BALLOTS CAST TOTAL` <- as.numeric(data_21$`BALLOTS CAST TOTAL`)
data_21$`REGISTERED VOTERS TOTAL` <- as.numeric(data_21$`REGISTERED VOTERS TOTAL`)

data_22$`BALLOTS CAST TOTAL` <- as.numeric(data_22$`BALLOTS CAST TOTAL`)
data_22$`REGISTERED VOTERS TOTAL` <- as.numeric(data_22$`REGISTERED VOTERS TOTAL`)

data_23$`BALLOTS CAST TOTAL` <- as.numeric(data_23$`BALLOTS CAST TOTAL`)
data_23$`REGISTERED VOTERS TOTAL` <- as.numeric(data_23$`REGISTERED VOTERS TOTAL`)

data_24$`BALLOTS CAST TOTAL` <- as.numeric(data_24$`BALLOTS CAST TOTAL`)
data_24$`REGISTERED VOTERS TOTAL` <- as.numeric(data_24$`REGISTERED VOTERS TOTAL`)


#### Turnout ####

data_18 <- data_18 %>% 
  mutate(Turnout = `BALLOTS CAST TOTAL`/`REGISTERED VOTERS TOTAL`)

data_19 <- data_19 %>% 
  mutate(Turnout = `BALLOTS CAST TOTAL`/`REGISTERED VOTERS TOTAL`)

data_20 <- data_20 %>% 
  mutate(Turnout = `BALLOTS CAST TOTAL`/`REGISTERED VOTERS TOTAL`)

data_21 <- data_21 %>% 
  mutate(Turnout = `BALLOTS CAST TOTAL`/`REGISTERED VOTERS TOTAL`)

data_22 <- data_22 %>% 
  mutate(Turnout = `BALLOTS CAST TOTAL`/`REGISTERED VOTERS TOTAL`)

data_23 <- data_23 %>% 
  mutate(Turnout = `BALLOTS CAST TOTAL`/`REGISTERED VOTERS TOTAL`)

data_24 <- data_24 %>% 
  mutate(Turnout = `BALLOTS CAST TOTAL`/`REGISTERED VOTERS TOTAL`)

turnout <- function(year){
  
  df <- get(paste0("data_", year))
  
  registered <- sum(df$`REGISTERED VOTERS TOTAL`, na.rm = TRUE)
  votes <- sum(df$`BALLOTS CAST TOTAL`, na.rm = TRUE)
  
  turnout <- votes / registered
  
  return(turnout)
  
}

turn_18 <- turnout(18)
turn_19 <- turnout(19)
turn_20 <- turnout(20)
turn_21 <- turnout(21)
turn_22 <- turnout(22)
turn_23 <- turnout(23)
turn_24 <- turnout(24)

# Our Prediction
turn_25 <- .4

#### Turnout Plot ####

turnout_df <- data.frame(
  year = 2018:2025,
  turnout = c(turn_18, turn_19, turn_20, turn_21,
              turn_22, turn_23, turn_24, turn_25)
)

# Election type (leave 2025 blank so it's handled separately)
turnout_df$election_type <- c(
  "Sen/Gov",     # 2018
  "Off-cycle",   # 2019
  "Presidential",# 2020
  "Off-cycle",   # 2021
  "Sen/Gov",     # 2022
  "Off-cycle",   # 2023
  "Presidential",# 2024
  NA             # 2025 -> no type
)

ggplot(turnout_df, aes(x = year, y = turnout, color = election_type, group = election_type)) +
  geom_line(size = 1.2, na.rm = TRUE) +
  geom_point(size = 3, na.rm = TRUE) +
  # Special orange dot for 2025
  geom_point(
    data = subset(turnout_df, year == 2025),
    aes(x = year, y = turnout),
    color = "orange", size = 4,
    inherit.aes = FALSE
  ) +
  # Label for prediction
  geom_text(
    data = subset(turnout_df, year == 2025),
    aes(x = year, y = turnout, label = "Our Prediction"),
    hjust = 1.2, vjust = 0.5,
    fontface = "bold", color = "black",
    inherit.aes = FALSE
  ) +
  # 🔥 Regression line for off-cycle extended to 2025
  geom_smooth(
    data = subset(turnout_df, election_type == "Off-cycle"),
    method = "lm", se = FALSE, color = "black", linetype = "dashed",
    fullrange = TRUE
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(.25, .9)
  ) +
  scale_color_discrete(na.translate = FALSE) +
  labs(
    title = "Voter Turnout by Election Type",
    x = "Year",
    y = "Turnout (%)",
    color = "Election Type"
  ) +
  theme_minimal(base_size = 14)

#### Drop-Off ####



