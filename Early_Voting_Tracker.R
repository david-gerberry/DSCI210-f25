#### Libraries ####

library(tidyverse)

#### Data #### 

hamilton_df <- read_csv("data/AbsenteeListExport-6908fd6184a9c.csv")

hamilton_df_2021 <- read_csv("data/AbsenteeListExport-6908f8972a157.csv")

hamilton_df_2023 <- read_csv("data/AbsenteeListExport-68f96ba1604c8.csv")


#### Actually Good Code ####

hamilton_df <- hamilton_df %>%
  filter(!(`Return Ballot Date` %in% c("2025-10-02", "2025-10-03")))

hamilton_df_2021 <- hamilton_df_2021 %>%
  filter(is.na(`Return Ballot Date`) | !(`Return Ballot Date` > as.Date("2021-11-4"))) %>% 
  filter(`Return Ballot Date` > as.Date("2021-10-01"))

hamilton_df <- hamilton_df %>%
  mutate(days_before = as.numeric(as.Date("2025-11-04") - `Return Ballot Date`))

hamilton_df_2021 <- hamilton_df_2021 %>%
  mutate(days_before = as.numeric(as.Date("2021-11-04") - `Return Ballot Date`))

hamilton_df_2023 <- hamilton_df_2023 %>%
  filter(is.na(`Return Ballot Date`) | !(`Return Ballot Date` > as.Date("2023-11-7")))

hamilton_df_2023 <- hamilton_df_2023 %>%
  mutate(days_before = as.numeric(as.Date("2023-11-07") - `Return Ballot Date`))

hamilton_df <- hamilton_df %>% 
  mutate(in_person = ifelse(`Request Application Date` == `Return Ballot Date`,1,0))

hamilton_df_2023 <- hamilton_df_2023 %>% 
  mutate(in_person = ifelse(`Request Application Date` == `Return Ballot Date`,1,0))

hamilton_df_2021 <- hamilton_df_2021 %>% 
  mutate(in_person = ifelse(`Request Application Date` == `Return Ballot Date`,1,0))

hamilton_df_early <- hamilton_df %>%
  filter(in_person == 1)

hamilton_df_2023_early <- hamilton_df_2023 %>%
  filter(in_person == 1)

hamilton_df_2021_early <- hamilton_df_2021 %>%
  filter(in_person == 1)

real_votes <- hamilton_df %>% 
  filter(!(is.na(`Return Ballot Date`)))
  
real_votes_2023 <- hamilton_df_2023 %>% 
  filter(!(is.na(`Return Ballot Date`)))

hamilton_df <- hamilton_df[-13038, ] 

real_votes <- real_votes %>% 
  mutate(age = 2025 - BirthYear)

real_votes_2023 <- real_votes_2023 %>% 
  mutate(age = 2025 - BirthYear)


#### Functions ####

turnout_by_date_2025 <- function(date, party = NULL) {
  
  if (!is.null(party) && party == "R") {
    df <- hamilton_df %>%
      filter(days_before >= !!date, VoterParty == "R")
  } else if (!is.null(party) && party == "U") {
    df <- hamilton_df %>%
      filter(days_before >= !!date, VoterParty == "U")
  } else if (!is.null(party) && party == "D") {
    df <- hamilton_df %>%
      filter(days_before >= !!date, VoterParty == "D")
  } else {
    df <- hamilton_df %>% 
      filter(days_before >= !!date)
  }
  
  votes_cast <- nrow(df)
  turnout <- votes_cast / nrow(hamilton_df)
  
  return(turnout)
}
turnout_by_date_2023 <- function(date, party = NULL) {
  
  if (!is.null(party) && party == "R") {
    df <- hamilton_df_2023 %>%
      filter(days_before >= !!date, VoterParty == "R")
  } else if (!is.null(party) && party == "U") {
    df <- hamilton_df_2023 %>%
      filter(days_before >= !!date, VoterParty == "U")
  } else if (!is.null(party) && party == "D") {
    df <- hamilton_df_2023 %>%
      filter(days_before >= !!date, VoterParty == "D")
  } else {
    df <- hamilton_df_2023 %>% 
      filter(days_before >= !!date)
  }
  
  votes_cast <- nrow(df)
  turnout <- votes_cast / nrow(hamilton_df_2023)
  
  return(turnout)
}
turnout_by_date_2021 <- function(date, party = NULL) {
  
  if (!is.null(party) && party == "R") {
    df <- hamilton_df_2021 %>%
      filter(days_before >= !!date, VoterParty == "R")
  } else if (!is.null(party) && party == "U") {
    df <- hamilton_df_2021 %>%
      filter(days_before >= !!date, VoterParty == "U")
  } else if (!is.null(party) && party == "D") {
    df <- hamilton_df_2021 %>%
      filter(days_before >= !!date, VoterParty == "D")
  } else {
    df <- hamilton_df_2021 %>% 
      filter(days_before >= !!date)
  }
  
  votes_cast <- nrow(df)
  turnout <- votes_cast / nrow(hamilton_df_2021)
  
  return(turnout)
}
total_votes_2025 <- function(date,party = NULL){
  
  if (!is.null(party) && party == "R") {
    df <- hamilton_df %>%
      filter(days_before >= !!date, VoterParty == "R")
  } else if (!is.null(party) && party == "U") {
    df <- hamilton_df %>%
      filter(days_before >= !!date, VoterParty == "U")
  } else if (!is.null(party) && party == "D") {
    df <- hamilton_df %>%
      filter(days_before >= !!date, VoterParty == "D")
  } else {
    df <- hamilton_df %>% 
      filter(days_before >= !!date)
  }
  
  votes_cast <- nrow(df)
  
  return(votes_cast)
  
}
total_votes_2023 <- function(date,party = NULL){
  
  if (!is.null(party) && party == "R") {
    df <- hamilton_df_2023 %>%
      filter(days_before >= !!date, VoterParty == "R")
  } else if (!is.null(party) && party == "U") {
    df <- hamilton_df_2023 %>%
      filter(days_before >= !!date, VoterParty == "U")
  } else if (!is.null(party) && party == "D") {
    df <- hamilton_df_2023 %>%
      filter(days_before >= !!date, VoterParty == "D")
  } else {
    df <- hamilton_df_2023 %>% 
      filter(days_before >= !!date)
  }
  
  votes_cast <- nrow(df)
  
  return(votes_cast)
  
}
total_votes_2021 <- function(date,party = NULL){
  
  if (!is.null(party) && party == "R") {
    df <- hamilton_df_2021 %>%
      filter(days_before >= !!date, VoterParty == "R")
  } else if (!is.null(party) && party == "U") {
    df <- hamilton_df_2021 %>%
      filter(days_before >= !!date, VoterParty == "U")
  } else if (!is.null(party) && party == "D") {
    df <- hamilton_df_2021 %>%
      filter(days_before >= !!date, VoterParty == "D")
  } else {
    df <- hamilton_df_2021 %>% 
      filter(days_before >= !!date)
  }
  
  votes_cast <- nrow(df)
  
  return(votes_cast)
  
}

turnout_by_date_2025_early <- function(date, party = NULL) {
  
  if (!is.null(party) && party == "R") {
    df <- hamilton_df_early %>%
      filter(days_before >= !!date, VoterParty == "R")
  } else if (!is.null(party) && party == "U") {
    df <- hamilton_df_early %>%
      filter(days_before >= !!date, VoterParty == "U")
  } else if (!is.null(party) && party == "D") {
    df <- hamilton_df_early %>%
      filter(days_before >= !!date, VoterParty == "D")
  } else {
    df <- hamilton_df_early %>% 
      filter(days_before >= !!date)
  }
  
  votes_cast <- nrow(df)
  turnout <- votes_cast / nrow(hamilton_df)
  
  return(turnout)
}
turnout_by_date_2023_early <- function(date, party = NULL) {
  
  if (!is.null(party) && party == "R") {
    df <- hamilton_df_2023_early %>%
      filter(days_before >= !!date, VoterParty == "R")
  } else if (!is.null(party) && party == "U") {
    df <- hamilton_df_2023_early %>%
      filter(days_before >= !!date, VoterParty == "U")
  } else if (!is.null(party) && party == "D") {
    df <- hamilton_df_2023_early %>%
      filter(days_before >= !!date, VoterParty == "D")
  } else {
    df <- hamilton_df_2023_early %>% 
      filter(days_before >= !!date)
  }
  
  votes_cast <- nrow(df)
  turnout <- votes_cast / nrow(hamilton_df_2023)
  
  return(turnout)
}
turnout_by_date_2021_early <- function(date, party = NULL) {
  
  if (!is.null(party) && party == "R") {
    df <- hamilton_df_2021_early %>%
      filter(days_before >= !!date, VoterParty == "R")
  } else if (!is.null(party) && party == "U") {
    df <- hamilton_df_2021_early %>%
      filter(days_before >= !!date, VoterParty == "U")
  } else if (!is.null(party) && party == "D") {
    df <- hamilton_df_2021_early %>%
      filter(days_before >= !!date, VoterParty == "D")
  } else {
    df <- hamilton_df_2021_early %>% 
      filter(days_before >= !!date)
  }
  
  votes_cast <- nrow(df)
  turnout <- votes_cast / nrow(hamilton_df_2021)
  
  return(turnout)
}
total_votes_2025_early <- function(date,party = NULL){
  
  if (!is.null(party) && party == "R") {
    df <- hamilton_df_early %>%
      filter(days_before >= !!date, VoterParty == "R")
  } else if (!is.null(party) && party == "U") {
    df <- hamilton_df_early %>%
      filter(days_before >= !!date, VoterParty == "U")
  } else if (!is.null(party) && party == "D") {
    df <- hamilton_df_early %>%
      filter(days_before >= !!date, VoterParty == "D")
  } else {
    df <- hamilton_df_early %>% 
      filter(days_before >= !!date)
  }
  
  votes_cast <- nrow(df)
  
  return(votes_cast)
  
}
total_votes_2023_early <- function(date,party = NULL){
  
  if (!is.null(party) && party == "R") {
    df <- hamilton_df_2023_early %>%
      filter(days_before >= !!date, VoterParty == "R")
  } else if (!is.null(party) && party == "U") {
    df <- hamilton_df_2023_early %>%
      filter(days_before >= !!date, VoterParty == "U")
  } else if (!is.null(party) && party == "D") {
    df <- hamilton_df_2023_early %>%
      filter(days_before >= !!date, VoterParty == "D")
  } else {
    df <- hamilton_df_2023_early %>% 
      filter(days_before >= !!date)
  }
  
  votes_cast <- nrow(df)
  
  return(votes_cast)
  
}
total_votes_2021_early <- function(date,party = NULL){
  
  if (!is.null(party) && party == "R") {
    df <- hamilton_df_2021_early %>%
      filter(days_before >= !!date, VoterParty == "R")
  } else if (!is.null(party) && party == "U") {
    df <- hamilton_df_2021_early %>%
      filter(days_before >= !!date, VoterParty == "U")
  } else if (!is.null(party) && party == "D") {
    df <- hamilton_df_2021_early %>%
      filter(days_before >= !!date, VoterParty == "D")
  } else {
    df <- hamilton_df_2021_early %>% 
      filter(days_before >= !!date)
  }
  
  votes_cast <- nrow(df)
  
  return(votes_cast)
  
}

#### Making Data Frame ####

days_2025 <- c(2:28)

days_2021 <- c(30:0)

days_2023 <- c(33:0)

dates <- c(
  seq(as.Date("2025-10-07"), as.Date("2025-11-02"), by = "day"),
  seq(as.Date("2023-10-05"), as.Date("2023-11-07"), by = "day"),
  seq(as.Date("2021-10-05"), as.Date("2021-11-04"), by = "day"))

df <- data.frame(
  date = dates,
  turnout = NA_real_,
  turnout_R = NA_real_,
  turnout_D = NA_real_,
  turnout_U = NA_real_,
  total_R = NA_real_,
  total_D = NA_real_,
  total_U = NA_real_
)

rm(dates)

days_2025 <- c(2:28)

days_2021 <- c(30:0)

days_2023 <- c(33:0)

dates <- c(
  seq(as.Date("2025-10-07"), as.Date("2025-11-02"), by = "day"),
  seq(as.Date("2023-10-05"), as.Date("2023-11-07"), by = "day"),
  seq(as.Date("2021-10-05"), as.Date("2021-11-04"), by = "day"))

df_early <- data.frame(
  date = dates,
  turnout = NA_real_,
  turnout_R = NA_real_,
  turnout_D = NA_real_,
  turnout_U = NA_real_,
  total_R = NA_real_,
  total_D = NA_real_,
  total_U = NA_real_
)

rm(dates)

#### Turnout Creator ####

  i <- 27
  for (date in days_2025) {
    df$turnout[i]    <- turnout_by_date_2025(date)
    df$turnout_R[i]  <- turnout_by_date_2025(date, "R")
    df$turnout_D[i]  <- turnout_by_date_2025(date, "D")
    df$turnout_U[i]  <- turnout_by_date_2025(date, "U")
    df$total_R[i]  <- total_votes_2025(date, "R")
    df$total_D[i]  <- total_votes_2025(date, "D")
    df$total_U[i]  <- total_votes_2025(date, "U")
    i <- i - 1
  }
  
  i <- 28
  for (date in days_2023) {
    df$turnout[i]    <- turnout_by_date_2023(date)
    df$turnout_R[i]  <- turnout_by_date_2023(date, "R")
    df$turnout_D[i]  <- turnout_by_date_2023(date, "D")
    df$turnout_U[i]  <- turnout_by_date_2023(date, "U")
    df$total_R[i]  <- total_votes_2023(date, "R")
    df$total_D[i]  <- total_votes_2023(date, "D")
    df$total_U[i]  <- total_votes_2023(date, "U")
    i <- i + 1
  }
  
  i <- 62
  for (date in days_2021) {
    df$turnout[i]    <- turnout_by_date_2021(date)
    df$turnout_R[i]  <- turnout_by_date_2021(date, "R")
    df$turnout_D[i]  <- turnout_by_date_2021(date, "D")
    df$turnout_U[i]  <- turnout_by_date_2021(date, "U")
    df$total_R[i]  <- total_votes_2021(date, "R")
    df$total_D[i]  <- total_votes_2021(date, "D")
    df$total_U[i]  <- total_votes_2021(date, "U")
    i <- i + 1
  }
  
  
  
  
  
  
  i <- 27
  for (date in days_2025) {
    df_early$turnout[i]    <- turnout_by_date_2025_early(date)
    df_early$turnout_R[i]  <- turnout_by_date_2025_early(date, "R")
    df_early$turnout_D[i]  <- turnout_by_date_2025_early(date, "D")
    df_early$turnout_U[i]  <- turnout_by_date_2025_early(date, "U")
    df_early$total_R[i]  <- total_votes_2025_early(date, "R")
    df_early$total_D[i]  <- total_votes_2025_early(date, "D")
    df_early$total_U[i]  <- total_votes_2025_early(date, "U")
    i <- i - 1
  }
  
  i <- 28
  for (date in days_2023) {
    df_early$turnout[i]    <- turnout_by_date_2023_early(date)
    df_early$turnout_R[i]  <- turnout_by_date_2023_early(date, "R")
    df_early$turnout_D[i]  <- turnout_by_date_2023_early(date, "D")
    df_early$turnout_U[i]  <- turnout_by_date_2023_early(date, "U")
    df_early$total_R[i]  <- total_votes_2023_early(date, "R")
    df_early$total_D[i]  <- total_votes_2023_early(date, "D")
    df_early$total_U[i]  <- total_votes_2023_early(date, "U")
    i <- i + 1
  }
  
  i <- 62
  for (date in days_2021) {
    df_early$turnout[i]    <- turnout_by_date_2021_early(date)
    df_early$turnout_R[i]  <- turnout_by_date_2021_early(date, "R")
    df_early$turnout_D[i]  <- turnout_by_date_2021_early(date, "D")
    df_early$turnout_U[i]  <- turnout_by_date_2021_early(date, "U")
    df_early$total_R[i]  <- total_votes_2021_early(date, "R")
    df_early$total_D[i]  <- total_votes_2021_early(date, "D")
    df_early$total_U[i]  <- total_votes_2021_early(date, "U")
    i <- i + 1
  }
  
  
  
  rm(date)
  rm(days_2021)
  rm(days_2023)
  rm(days_2025)
  rm(i)
  


#### Adding Rows ####

df <- df %>%
  mutate(election_year = case_when(
    format(date, "%Y") == "2025" ~ 0,
    format(date, "%Y") == "2023" ~ 1,
    format(date, "%Y") == "2021" ~ 2,
    TRUE ~ NA_real_  # for dates outside those years
  ))

df <- df %>%
  mutate(days_before = case_when(
    format(date, "%Y") == "2025" ~ as.numeric(as.Date("2025-11-04") - date),
    format(date, "%Y") == "2023" ~ as.numeric(as.Date("2023-11-07") - date),
    format(date, "%Y") == "2021" ~ as.numeric(as.Date("2021-11-04") - date),
    TRUE ~ NA_real_  # for any other years
  ))

df <- df %>% 
  mutate(total = total_R + total_D + total_U)






df_early <- df_early %>%
  mutate(election_year = case_when(
    format(date, "%Y") == "2025" ~ 0,
    format(date, "%Y") == "2023" ~ 1,
    format(date, "%Y") == "2021" ~ 2,
    TRUE ~ NA_real_  # for dates outside those years
  ))

df_early <- df_early %>%
  mutate(days_before = case_when(
    format(date, "%Y") == "2025" ~ as.numeric(as.Date("2025-11-04") - date),
    format(date, "%Y") == "2023" ~ as.numeric(as.Date("2023-11-07") - date),
    format(date, "%Y") == "2021" ~ as.numeric(as.Date("2021-11-04") - date),
    TRUE ~ NA_real_  # for any other years
  ))

df_early <- df_early %>% 
  mutate(total = total_R + total_D + total_U)

#### Total Comparison Graph ####

ggplot(df, aes(x = days_before, y = turnout, linetype = factor(election_year))) +
  geom_line(size = 1.2, color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", size = 0.7) +
  annotate("text", x = 0, y = max(df$turnout, na.rm = TRUE) * 1.02,
           label = "Election Day", hjust = 1.1, vjust = 0, color = "gray20", size = 3.5) +
  scale_x_reverse(
    breaks = seq(0, max(df$days_before, na.rm = TRUE), by = 5),
    labels = function(x) ifelse(x == 0, "Election Day", x),
    expand = c(0.01, 0)
  ) +
  # Explicit, named mapping so labels can't get mixed up:
  scale_linetype_manual(
    name = "Election Year",
    values = c("0" = "solid",    # election_year == 0 -> 2025 -> solid
               "1" = "longdash", # election_year == 1 -> 2023 -> longdash
               "2" = "dotted"),  # election_year == 2 -> 2021 -> dotted
    labels = c("2" = "2021", "1" = "2023", "0" = "2025"),  # order labels chronologically
    breaks = c("2", "1", "0")  # ensures legend order: 2021, 2023, 2025
  ) +
  labs(
    x = "Days Before Election",
    y = "Absentee Ballot Return Rate",
    title = "Absentee Ballot Return Rates by Election Year",
    subtitle = "Comparison of daily return rates leading up to Election Day"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray30"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "gray25"),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "gray90"),
    panel.grid.major.y = element_line(color = "gray85"),
    plot.margin = margin(10, 20, 10, 20)
  )

#### All Party Proportion Affiliation Graph ####

ggplot() +
  # 2025 solid
  geom_line(data = subset(df, election_year == 0),
            aes(x = days_before, y = turnout_R, color = "Republican", linetype = "2025"), size = 1.2) +
  geom_line(data = subset(df, election_year == 0),
            aes(x = days_before, y = turnout_D, color = "Democrat", linetype = "2025"), size = 1.2) +
  geom_line(data = subset(df, election_year == 0),
            aes(x = days_before, y = turnout_U, color = "Undecided", linetype = "2025"), size = 1.2) +
  
  # 2023 dashed
  geom_line(data = subset(df, election_year == 1),
            aes(x = days_before, y = turnout_R, color = "Republican", linetype = "2023"), size = 1.2) +
  geom_line(data = subset(df, election_year == 1),
            aes(x = days_before, y = turnout_D, color = "Democrat", linetype = "2023"), size = 1.2) +
  geom_line(data = subset(df, election_year == 1),
            aes(x = days_before, y = turnout_U, color = "Undecided", linetype = "2023"), size = 1.2) +
  
  # 2021 dotted
  geom_line(data = subset(df, election_year == 2),
            aes(x = days_before, y = turnout_R, color = "Republican", linetype = "2021"), size = 1.2) +
  geom_line(data = subset(df, election_year == 2),
            aes(x = days_before, y = turnout_D, color = "Democrat", linetype = "2021"), size = 1.2) +
  geom_line(data = subset(df, election_year == 2),
            aes(x = days_before, y = turnout_U, color = "Undecided", linetype = "2021"), size = 1.2) +
  
  # Reverse x-axis (Election Day on the right)
  scale_x_reverse(
    breaks = seq(0, max(df$days_before, na.rm = TRUE), by = 5),
    labels = function(x) ifelse(x == 0, "Election Day", x)
  ) +
  
  # Custom color palette for parties
  scale_color_manual(
    name = "Party",
    values = c("Republican" = "#D73027", "Democrat" = "#4575B4", "Undecided" = "#1A9850")
  ) +
  
  # Line styles for years
  scale_linetype_manual(
    name = "Election Year",
    values = c("2021" = "dotted", "2023" = "dashed", "2025" = "solid")
  ) +
  
  # Fancy labels and theme
  labs(
    x = "Days Before Election",
    y = "Absentee Ballot Return Rate",
    title = "Absentee Ballot Return Rates by Party and Election Year",
    subtitle = "Comparison across 2021, 2023, and 2025 elections"
  ) +
  theme_minimal(base_family = "Helvetica", base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    plot.subtitle = element_text(hjust = 0.5, size = 13, color = "gray30"),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "gray85", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "gray98", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    axis.text = element_text(color = "gray20"),
    axis.title = element_text(face = "bold", color = "gray20")
  ) +
  guides(
    color = guide_legend(order = 1, override.aes = list(size = 2)),
    linetype = guide_legend(order = 2)
  )

#### Comparing All Parties this Year ####

ggplot() +
  # 2025 solid
  geom_line(data = subset(df, election_year == 0),
            aes(x = days_before, y = turnout_R, color = "Republican", linetype = "2025"), size = 1.2) +
  geom_line(data = subset(df, election_year == 0),
            aes(x = days_before, y = turnout_D, color = "Democrat", linetype = "2025"), size = 1.2) +
  geom_line(data = subset(df, election_year == 0),
            aes(x = days_before, y = turnout_U, color = "Undecided", linetype = "2025"), size = 1.2) +
  
  # Reverse x-axis (Election Day on the right)
  scale_x_reverse(
    breaks = seq(0, max(df$days_before, na.rm = TRUE), by = 5),
    labels = function(x) ifelse(x == 0, "Election Day", x)
  ) +
  
  # Custom color palette for parties
  scale_color_manual(
    name = "Party",
    values = c("Republican" = "#D73027", "Democrat" = "#4575B4", "Undecided" = "#1A9850")
  ) +
  
  # Line styles for years
  scale_linetype_manual(
    name = "Election Year",
    values = c("2021" = "dotted", "2023" = "dashed", "2025" = "solid")
  ) +
  
  # Fancy labels and theme
  labs(
    x = "Days Before Election",
    y = "Absentee Ballot Return Rate",
    title = "Absentee Ballot Return Rates by Party and Election Year",
    subtitle = "Comparison across 2021, 2023, and 2025 elections"
  ) +
  theme_minimal(base_family = "Helvetica", base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    plot.subtitle = element_text(hjust = 0.5, size = 13, color = "gray30"),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "gray85", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "gray98", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    axis.text = element_text(color = "gray20"),
    axis.title = element_text(face = "bold", color = "gray20")
  ) +
  guides(
    color = guide_legend(order = 1, override.aes = list(size = 2)),
    linetype = guide_legend(order = 2)
  )

#### Comparing Single Parties by Year - Republican ####

ggplot() +
  # 2025 solid
  geom_line(data = subset(df, election_year == 0),
            aes(x = days_before, y = turnout_R, color = "Republican", linetype = "2025"), size = 1.2) +
  # 2023 dashed
  geom_line(data = subset(df, election_year == 1),
            aes(x = days_before, y = turnout_R, color = "Republican", linetype = "2023"), size = 1.2) +
  # 2021 dotted
  geom_line(data = subset(df, election_year == 2),
            aes(x = days_before, y = turnout_R, color = "Republican", linetype = "2021"), size = 1.2) +
  # Reverse x-axis (Election Day on the right)
  scale_x_reverse(
    breaks = seq(0, max(df$days_before, na.rm = TRUE), by = 5),
    labels = function(x) ifelse(x == 0, "Election Day", x)
  ) +
  
  # Custom color palette for parties
  scale_color_manual(
    name = "Party",
    values = c("Republican" = "#D73027", "Democrat" = "#4575B4", "Undecided" = "#1A9850")
  ) +
  
  # Line styles for years
  scale_linetype_manual(
    name = "Election Year",
    values = c("2021" = "dotted", "2023" = "dashed", "2025" = "solid")
  ) +
  
  # Fancy labels and theme
  labs(
    x = "Days Before Election",
    y = "Absentee Ballot Return Rate",
    title = "Absentee Ballot Return Rates by Party and Election Year",
    subtitle = "Comparison across 2021, 2023, and 2025 elections"
  ) +
  theme_minimal(base_family = "Helvetica", base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    plot.subtitle = element_text(hjust = 0.5, size = 13, color = "gray30"),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "gray85", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "gray98", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    axis.text = element_text(color = "gray20"),
    axis.title = element_text(face = "bold", color = "gray20")
  ) +
  guides(
    color = guide_legend(order = 1, override.aes = list(size = 2)),
    linetype = guide_legend(order = 2)
  )

#### Comparing Single Parties by Year - Democrat ####

ggplot() +
  # 2025 solid
  geom_line(data = subset(df, election_year == 0),
            aes(x = days_before, y = turnout_D, color = "Democrat", linetype = "2025"), size = 1.2) +
  
  # 2023 dashed
  geom_line(data = subset(df, election_year == 1),
            aes(x = days_before, y = turnout_D, color = "Democrat", linetype = "2023"), size = 1.2) +
  
  # 2021 dotted
  geom_line(data = subset(df, election_year == 2),
            aes(x = days_before, y = turnout_D, color = "Democrat", linetype = "2021"), size = 1.2) +
  
  # Reverse x-axis (Election Day on the right)
  scale_x_reverse(
    breaks = seq(0, max(df$days_before, na.rm = TRUE), by = 5),
    labels = function(x) ifelse(x == 0, "Election Day", x)
  ) +
  
  # Custom color palette for parties
  scale_color_manual(
    name = "Party",
    values = c("Republican" = "#D73027", "Democrat" = "#4575B4", "Undecided" = "#1A9850")
  ) +
  
  # Line styles for years
  scale_linetype_manual(
    name = "Election Year",
    values = c("2021" = "dotted", "2023" = "dashed", "2025" = "solid")
  ) +
  
  # Fancy labels and theme
  labs(
    x = "Days Before Election",
    y = "Absentee Ballot Return Rate",
    title = "Absentee Ballot Return Rates by Party and Election Year",
    subtitle = "Comparison across 2021, 2023, and 2025 elections"
  ) +
  theme_minimal(base_family = "Helvetica", base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    plot.subtitle = element_text(hjust = 0.5, size = 13, color = "gray30"),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "gray85", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "gray98", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    axis.text = element_text(color = "gray20"),
    axis.title = element_text(face = "bold", color = "gray20")
  ) +
  guides(
    color = guide_legend(order = 1, override.aes = list(size = 2)),
    linetype = guide_legend(order = 2)
  )

#### Comparing Single Parties by Year - Unaffiliated ####

ggplot() +
  # 2025 solid
  geom_line(data = subset(df, election_year == 0),
            aes(x = days_before, y = turnout_U, color = "Undecided", linetype = "2025"), size = 1.2) +
  
  # 2023 dashed
  geom_line(data = subset(df, election_year == 1),
            aes(x = days_before, y = turnout_U, color = "Undecided", linetype = "2023"), size = 1.2) +
  
  # 2021 dotted
  geom_line(data = subset(df, election_year == 2),
            aes(x = days_before, y = turnout_U, color = "Undecided", linetype = "2021"), size = 1.2) +
  
  # Reverse x-axis (Election Day on the right)
  scale_x_reverse(
    breaks = seq(0, max(df$days_before, na.rm = TRUE), by = 5),
    labels = function(x) ifelse(x == 0, "Election Day", x)
  ) +
  
  # Custom color palette for parties
  scale_color_manual(
    name = "Party",
    values = c("Republican" = "#D73027", "Democrat" = "#4575B4", "Undecided" = "#1A9850")
  ) +
  
  # Line styles for years
  scale_linetype_manual(
    name = "Election Year",
    values = c("2021" = "dotted", "2023" = "dashed", "2025" = "solid")
  ) +
  
  # Fancy labels and theme
  labs(
    x = "Days Before Election",
    y = "Absentee Ballot Return Rate",
    title = "Absentee Ballot Return Rates by Party and Election Year",
    subtitle = "Comparison across 2021, 2023, and 2025 elections"
  ) +
  theme_minimal(base_family = "Helvetica", base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    plot.subtitle = element_text(hjust = 0.5, size = 13, color = "gray30"),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "gray85", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "gray98", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    axis.text = element_text(color = "gray20"),
    axis.title = element_text(face = "bold", color = "gray20")
  ) +
  guides(
    color = guide_legend(order = 1, override.aes = list(size = 2)),
    linetype = guide_legend(order = 2)
  )

#### Total Vote by Party All Year ####

ggplot() +
  # 2025 solid
  geom_line(data = subset(df, election_year == 0),
            aes(x = days_before, y = total_R, color = "Republican", linetype = "2025"), size = 1.2) +
  geom_line(data = subset(df, election_year == 0),
            aes(x = days_before, y = total_D, color = "Democrat", linetype = "2025"), size = 1.2) +
  
  # 2023 dashed
  geom_line(data = subset(df, election_year == 1),
            aes(x = days_before, y = total_R, color = "Republican", linetype = "2023"), size = 1.2) +
  geom_line(data = subset(df, election_year == 1),
            aes(x = days_before, y = total_D, color = "Democrat", linetype = "2023"), size = 1.2) +
  
  # Reverse x-axis (Election Day on the right)
  scale_x_reverse(
    breaks = seq(0, max(df$days_before, na.rm = TRUE), by = 5),
    labels = function(x) ifelse(x == 0, "Election Day", x)
  ) +
  
  # Custom color palette for parties
  scale_color_manual(
    name = "Party",
    values = c("Republican" = "#D73027", "Democrat" = "#4575B4", "Undecided" = "#1A9850")
  ) +
  
  # Line styles for years
  scale_linetype_manual(
    name = "Election Year",
    values = c("2021" = "dotted", "2023" = "dashed", "2025" = "solid")
  ) +
  
  # Fancy labels and theme
  labs(
    x = "Days Before Election",
    y = "Early Votes Cast",
    title = "Total Early Votes Cast by Party and Election Year",
    subtitle = "Comparison of 2023 and 2025 elections"
  ) +
  theme_minimal(base_family = "Helvetica", base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    plot.subtitle = element_text(hjust = 0.5, size = 13, color = "gray30"),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "gray85", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "gray98", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    axis.text = element_text(color = "gray20"),
    axis.title = element_text(face = "bold", color = "gray20")
  ) +
  guides(
    color = guide_legend(order = 1, override.aes = list(size = 2)),
    linetype = guide_legend(order = 2)
  )

#### Total Vote by Party All Year - Early ####

ggplot() +
  # 2025 solid
  geom_line(data = subset(df_early, election_year == 0),
            aes(x = days_before, y = total_R, color = "Republican", linetype = "2025"), size = 1.2) +
  geom_line(data = subset(df_early, election_year == 0),
            aes(x = days_before, y = total_D, color = "Democrat", linetype = "2025"), size = 1.2) +
  
  # 2023 dashed
  geom_line(data = subset(df_early, election_year == 1),
            aes(x = days_before, y = total_R, color = "Republican", linetype = "2023"), size = 1.2) +
  geom_line(data = subset(df_early, election_year == 1),
            aes(x = days_before, y = total_D, color = "Democrat", linetype = "2023"), size = 1.2) +
  geom_line(data = subset(df_early, election_year == 2),
            aes(x = days_before, y = total_R, color = "Republican", linetype = "2021"), size = 1.2) +
  geom_line(data = subset(df_early, election_year == 2),
            aes(x = days_before, y = total_D, color = "Democrat", linetype = "2021"), size = 1.2) +
  
  # Reverse x-axis (Election Day on the right)
  scale_x_reverse(
    breaks = seq(0, max(df$days_before, na.rm = TRUE), by = 5),
    labels = function(x) ifelse(x == 0, "Election Day", x)
  ) +
  
  # Custom color palette for parties
  scale_color_manual(
    name = "Party",
    values = c("Republican" = "#D73027", "Democrat" = "#4575B4", "Undecided" = "#1A9850")
  ) +
  
  # Line styles for years
  scale_linetype_manual(
    name = "Election Year",
    values = c("2021" = "dotted", "2023" = "dashed", "2025" = "solid")
  ) +
  
  # Fancy labels and theme
  labs(
    x = "Days Before Election",
    y = "Early In-Person Votes Cast",
    title = "Early In-Person Votes Cast by Party and Election Year",
    subtitle = "Comparison of 2021, 2023, and 2025 elections"
  ) +
  theme_minimal(base_family = "Helvetica", base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    plot.subtitle = element_text(hjust = 0.5, size = 13, color = "gray30"),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "gray85", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "gray98", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    axis.text = element_text(color = "gray20"),
    axis.title = element_text(face = "bold", color = "gray20")
  ) +
  guides(
    color = guide_legend(order = 1, override.aes = list(size = 2)),
    linetype = guide_legend(order = 2)
  )

#### Comparing Total Votes - Republican ####

ggplot() +
  # 2025 solid
  geom_line(data = subset(df, election_year == 0),
            aes(x = days_before, y = total_R, color = "Republican", linetype = "2025"), size = 1.2) +
  # 2023 dashed
  geom_line(data = subset(df, election_year == 1),
            aes(x = days_before, y = total_R, color = "Republican", linetype = "2023"), size = 1.2) +
  # 2021 dotted
  geom_line(data = subset(df, election_year == 2),
            aes(x = days_before, y = total_R, color = "Republican", linetype = "2021"), size = 1.2) +
  # Reverse x-axis (Election Day on the right)
  scale_x_reverse(
    breaks = seq(0, max(df$days_before, na.rm = TRUE), by = 5),
    labels = function(x) ifelse(x == 0, "Election Day", x)
  ) +
  
  # Custom color palette for parties
  scale_color_manual(
    name = "Party",
    values = c("Republican" = "#D73027", "Democrat" = "#4575B4", "Undecided" = "#1A9850")
  ) +
  
  # Line styles for years
  scale_linetype_manual(
    name = "Election Year",
    values = c("2021" = "dotted", "2023" = "dashed", "2025" = "solid")
  ) +
  
  # Fancy labels and theme
  labs(
    x = "Days Before Election",
    y = "Absentee Ballot Return Rate",
    title = "Absentee Ballot Return Rates by Party and Election Year",
    subtitle = "Comparison across 2021, 2023, and 2025 elections"
  ) +
  theme_minimal(base_family = "Helvetica", base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    plot.subtitle = element_text(hjust = 0.5, size = 13, color = "gray30"),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "gray85", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "gray98", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    axis.text = element_text(color = "gray20"),
    axis.title = element_text(face = "bold", color = "gray20")
  ) +
  guides(
    color = guide_legend(order = 1, override.aes = list(size = 2)),
    linetype = guide_legend(order = 2)
  )


#### Comparing Total Votes - Democrat ####

ggplot() +
  # 2025 solid
  geom_line(data = subset(df, election_year == 0),
            aes(x = days_before, y = total_D, color = "Democrat", linetype = "2025"), size = 1.2) +
  geom_line(data = subset(df, election_year == 1),
            aes(x = days_before, y = total_D, color = "Democrat", linetype = "2023"), size = 1.2) +
  geom_line(data = subset(df, election_year == 2),
            aes(x = days_before, y = total_D, color = "Democrat", linetype = "2021"), size = 1.2) +
  
  # Reverse x-axis (Election Day on the right)
  scale_x_reverse(
    breaks = seq(0, max(df$days_before, na.rm = TRUE), by = 5),
    labels = function(x) ifelse(x == 0, "Election Day", x)
  ) +
  
  # Custom color palette for parties
  scale_color_manual(
    name = "Party",
    values = c("Republican" = "#D73027", "Democrat" = "#4575B4", "Undecided" = "#1A9850")
  ) +
  
  # Line styles for years
  scale_linetype_manual(
    name = "Election Year",
    values = c("2021" = "dotted", "2023" = "dashed", "2025" = "solid")
  ) +
  
  # Fancy labels and theme
  labs(
    x = "Days Before Election",
    y = "Absentee Ballot Return Rate",
    title = "Absentee Ballot Return Rates by Party and Election Year",
    subtitle = "Comparison across 2021, 2023, and 2025 elections"
  ) +
  theme_minimal(base_family = "Helvetica", base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    plot.subtitle = element_text(hjust = 0.5, size = 13, color = "gray30"),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "gray85", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "gray98", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    axis.text = element_text(color = "gray20"),
    axis.title = element_text(face = "bold", color = "gray20")
  ) +
  guides(
    color = guide_legend(order = 1, override.aes = list(size = 2)),
    linetype = guide_legend(order = 2)
  )

#### Comparing Total Votes - Unaffiliated ####

ggplot() +
  # 2025 solid
  geom_line(data = subset(df, election_year == 0),
            aes(x = days_before, y = total_U, color = "Undecided", linetype = "2025"), size = 1.2) +
  # 2023 dashed
  geom_line(data = subset(df, election_year == 1),
            aes(x = days_before, y = total_U, color = "Undecided", linetype = "2023"), size = 1.2) +
  # 2021 dotted
  geom_line(data = subset(df, election_year == 2),
            aes(x = days_before, y = total_U, color = "Undecided", linetype = "2021"), size = 1.2) +
  
  # Reverse x-axis (Election Day on the right)
  scale_x_reverse(
    breaks = seq(0, max(df$days_before, na.rm = TRUE), by = 5),
    labels = function(x) ifelse(x == 0, "Election Day", x)
  ) +
  
  # Custom color palette for parties
  scale_color_manual(
    name = "Party",
    values = c("Republican" = "#D73027", "Democrat" = "#4575B4", "Undecided" = "#1A9850")
  ) +
  
  # Line styles for years
  scale_linetype_manual(
    name = "Election Year",
    values = c("2021" = "dotted", "2023" = "dashed", "2025" = "solid")
  ) +
  
  # Fancy labels and theme
  labs(
    x = "Days Before Election",
    y = "Absentee Ballot Return Rate",
    title = "Absentee Ballot Return Rates by Party and Election Year",
    subtitle = "Comparison across 2021, 2023, and 2025 elections"
  ) +
  theme_minimal(base_family = "Helvetica", base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    plot.subtitle = element_text(hjust = 0.5, size = 13, color = "gray30"),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "gray85", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "gray98", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    axis.text = element_text(color = "gray20"),
    axis.title = element_text(face = "bold", color = "gray20")
  ) +
  guides(
    color = guide_legend(order = 1, override.aes = list(size = 2)),
    linetype = guide_legend(order = 2)
  )

#### Total Votes by Party 2025 ####

ggplot() +
  # 2025 solid
  geom_line(data = subset(df, election_year == 0),
            aes(x = days_before, y = total_R, color = "Republican", linetype = "2025"), size = 1.2) +
  geom_line(data = subset(df, election_year == 0),
            aes(x = days_before, y = total_D, color = "Democrat", linetype = "2025"), size = 1.2) +
  geom_line(data = subset(df, election_year == 0),
            aes(x = days_before, y = total_U, color = "Undecided", linetype = "2025"), size = 1.2) +
  # Reverse x-axis (Election Day on the right)
  scale_x_reverse(
    breaks = seq(0, max(df$days_before, na.rm = TRUE), by = 5),
    labels = function(x) ifelse(x == 0, "Election Day", x)
  ) +
  
  # Custom color palette for parties
  scale_color_manual(
    name = "Party",
    values = c("Republican" = "#D73027", "Democrat" = "#4575B4", "Undecided" = "#1A9850")
  ) +
  
  # Line styles for years
  scale_linetype_manual(
    name = "Election Year",
    values = c("2021" = "dotted", "2023" = "dashed", "2025" = "solid")
  ) +
  
  # Fancy labels and theme
  labs(
    x = "Days Before Election",
    y = "Absentee Ballot Return Rate",
    title = "Absentee Ballot Return Rates by Party and Election Year",
    subtitle = "Comparison across 2021, 2023, and 2025 elections"
  ) +
  theme_minimal(base_family = "Helvetica", base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    plot.subtitle = element_text(hjust = 0.5, size = 13, color = "gray30"),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "gray85", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "gray98", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    axis.text = element_text(color = "gray20"),
    axis.title = element_text(face = "bold", color = "gray20")
  ) +
  guides(
    color = guide_legend(order = 1, override.aes = list(size = 2)),
    linetype = guide_legend(order = 2)
  )


#### Total Vote by Year ####

ggplot() +
  geom_line(
    data = subset(df, election_year == 0),
    aes(x = days_before, y = total, linetype = "2025"),
    color = "black", size = 1.2
  ) +
  geom_line(
    data = subset(df, election_year == 1),
    aes(x = days_before, y = total, linetype = "2023"),
    color = "black", size = 1.2
  ) +
  
  # Reverse x-axis (Election Day on the right)
  scale_x_reverse(
    breaks = seq(0, max(df$days_before, na.rm = TRUE), by = 5),
    labels = function(x) ifelse(x == 0, "Election Day", x)
  ) +
  
  # Custom line styles for each year
  scale_linetype_manual(
    name = "Election Year",
    values = c("2021" = "dotted", "2023" = "dashed", "2025" = "solid")
  ) +
  
  # Fancy labels and theme
  labs(
    x = "Days Before Election",
    y = "Votes Cast Early",
    title = "Total Early Votes by Election Year",
    subtitle = "Comparison of 2023 and 2025 elections"
  ) +
  theme_minimal(base_family = "Helvetica", base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    plot.subtitle = element_text(hjust = 0.5, size = 13, color = "gray30"),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "gray85", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "gray98", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    axis.text = element_text(color = "gray20"),
    axis.title = element_text(face = "bold", color = "gray20")
  ) +
  guides(
    linetype = guide_legend(order = 1, override.aes = list(size = 1.5))
  )

#### Total Vote by Year - Early ####

ggplot() +
  geom_line(
    data = subset(df_early, election_year == 0),
    aes(x = days_before, y = total, linetype = "2025"),
    color = "black", size = 1.2
  ) +
  geom_line(
    data = subset(df_early, election_year == 1),
    aes(x = days_before, y = total, linetype = "2023"),
    color = "black", size = 1.2
  ) +
  geom_line(
    data = subset(df_early, election_year == 2),
    aes(x = days_before, y = total, linetype = "2021"),
    color = "black", size = 1.2
  ) +
  
  # Reverse x-axis (Election Day on the right)
  scale_x_reverse(
    breaks = seq(0, max(df$days_before, na.rm = TRUE), by = 5),
    labels = function(x) ifelse(x == 0, "Election Day", x)
  ) +
  
  # Custom line styles for each year
  scale_linetype_manual(
    name = "Election Year",
    values = c("2021" = "dotted", "2023" = "dashed", "2025" = "solid")
  ) +
  
  # Fancy labels and theme
  labs(
    x = "Days Before Election",
    y = "Votes Cast Early In-Person",
    title = "Early In-Person Votes by Election Year",
    subtitle = "Comparison of 2021, 2023, and 2025 elections"
  ) +
  theme_minimal(base_family = "Helvetica", base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    plot.subtitle = element_text(hjust = 0.5, size = 13, color = "gray30"),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "gray85", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "gray98", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    axis.text = element_text(color = "gray20"),
    axis.title = element_text(face = "bold", color = "gray20")
  ) +
  guides(
    linetype = guide_legend(order = 1, override.aes = list(size = 1.5))
  )

#### Mailer List ####

cool_df <- hamilton_df %>%
  filter(is.na(`Return Ballot Date`)) %>%
  filter(VoterParty == "R") %>%
  filter(Judicial == "JDMC04") %>%
  filter(Zip %in% c(45255, 45226, 45230, 45244, 45254))

write.csv(cool_df, "mailer_list_berkowitz.csv", row.names = FALSE)


#### Age Graph ####

gpt_2025 <- real_votes %>% mutate(Year = "2025")
gpt_2023 <- real_votes_2023 %>% mutate(Year = "2023")

# Combine datasets
combined_df <- bind_rows(gpt_2025, gpt_2023)

# Define age groups
combined_df <- combined_df %>%
  mutate(AgeGroup = cut(
    age,
    breaks = c(0, 24, 34, 44, 54, 64, Inf),
    labels = c("18–24", "25–34", "35–44", "45–54", "55–64", "65+"),
    right = TRUE
  ))

# Compute percentage of voters in each age group per year
age_pct <- combined_df %>%
  group_by(Year, AgeGroup) %>%
  summarise(Count = n(), .groups = "drop_last") %>%
  mutate(Percentage = Count / sum(Count) * 100)

neutral_colors <- c("2023" = "#A8B5B9", "2025" = "#7C8C8E")


# Plot
ggplot(age_pct, aes(x = AgeGroup, y = Percentage, fill = Year)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8),
           width = 0.7, alpha = 0.9, color = "white") +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)),
            position = position_dodge(width = 0.8),
            vjust = -0.4, size = 3.5, color = "#444444") +
  labs(
    title = "Voter Age Distribution by Year",
    subtitle = "Comparing 2023 and 2025 voter data (percentages by age group)",
    x = "Age Group",
    y = "Percentage of Voters",
    fill = "Year"
  ) +
  scale_fill_manual(values = neutral_colors) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    plot.subtitle = element_text(hjust = 0.5, color = "#555555", size = 12),
    axis.text = element_text(color = "#333333"),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "top",
    legend.title = element_text(face = "bold")
  ) +
  coord_cartesian(ylim = c(0, max(age_pct$Percentage) * 1.15))
























