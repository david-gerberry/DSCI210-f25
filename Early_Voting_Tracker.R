#### Libraries ####

library(tidyverse)

#### Data #### 

hamilton_df <- read_csv("data/AbsenteeListExport-68f6d43fbaa62.csv")

hamilton_df_2021 <- read_csv("data/AbsenteeListExport-68f77d75ec2da.csv")

#### Creating Data Frames ####

judge_df <- hamilton_df %>% 
  filter(Judicial == "JDMC04")

cps_df <- hamilton_df %>% 
  filter(School == "SCCISD")

council_df <- hamilton_df %>% 
  filter(grepl("^CIN", PrecinctName, ignore.case = TRUE))

judge_df2 <- hamilton_df2 %>% 
  filter(Judicial == "JDMC04")

cps_df2 <- hamilton_df2 %>% 
  filter(School == "SCCISD")

council_df2 <- hamilton_df2 %>% 
  filter(grepl("^CIN", PrecinctName, ignore.case = TRUE))

judge_df_2021 <- data_2021 %>% 
  filter(Judicial == "JDMC04")

cps_df_2021 <- data_2021 %>% 
  filter(School == "SCCISD")

council_df_2021 <- data_2021 %>% 
  filter(grepl("^CIN", PrecinctName, ignore.case = TRUE))


#### Adding Rows ####

hamilton_df <- hamilton_df %>% 
  mutate(voted = if_else(!is.na(`Return Ballot Date`), 1, 0))

cps_df <- cps_df %>% 
  mutate(voted = if_else(!is.na(`Return Ballot Date`), 1, 0))

judge_df <- judge_df %>% 
  mutate(voted = if_else(!is.na(`Return Ballot Date`), 1, 0))

council_df <- council_df %>% 
  mutate(voted = if_else(!is.na(`Return Ballot Date`), 1, 0))



hamilton_df2 <- hamilton_df2 %>% 
  mutate(voted = if_else(!is.na(`Return Ballot Date`), 1, 0))

cps_df2 <- cps_df2 %>% 
  mutate(voted = if_else(!is.na(`Return Ballot Date`), 1, 0))

judge_df2 <- judge_df2 %>% 
  mutate(voted = if_else(!is.na(`Return Ballot Date`), 1, 0))

council_df2 <- council_df2 %>% 
  mutate(voted = if_else(!is.na(`Return Ballot Date`), 1, 0))



hamilton_df_2021 <- hamilton_df_2021 %>% 
  mutate(voted = if_else(!is.na(`Return Ballot Date`), 1, 0))

cps_df_2021 <- cps_df_2021 %>% 
  mutate(voted = if_else(!is.na(`Return Ballot Date`), 1, 0))

judge_df_2021 <- judge_df_2021 %>% 
  mutate(voted = if_else(!is.na(`Return Ballot Date`), 1, 0))

council_df_2021 <- council_df_2021 %>% 
  mutate(voted = if_else(!is.na(`Return Ballot Date`), 1, 0))

#### Current "Turnout" ####

votes_cast_ham <- sum(hamilton_df$voted)
votes_cast_cps <- sum(cps_df$voted)
votes_cast_judge <- sum(judge_df$voted)
votes_cast_council <- sum(council_df$voted)

votes_cast_ham_R <- hamilton_df %>%
  filter(VoterParty == "R") %>%
  summarise(total_votes = sum(voted, na.rm = TRUE)) %>%
  pull(total_votes)
votes_cast_cps_R <- cps_df %>%
  filter(VoterParty == "R") %>%
  summarise(total_votes = sum(voted, na.rm = TRUE)) %>%
  pull(total_votes)
votes_cast_judge_R <- judge_df %>%
  filter(VoterParty == "R") %>%
  summarise(total_votes = sum(voted, na.rm = TRUE)) %>%
  pull(total_votes)
votes_cast_council_R <- council_df %>%
  filter(VoterParty == "R") %>%
  summarise(total_votes = sum(voted, na.rm = TRUE)) %>%
  pull(total_votes)

votes_cast_ham_D <- hamilton_df %>%
  filter(VoterParty == "D") %>%
  summarise(total_votes = sum(voted, na.rm = TRUE)) %>%
  pull(total_votes)
votes_cast_cps_D <- cps_df %>%
  filter(VoterParty == "D") %>%
  summarise(total_votes = sum(voted, na.rm = TRUE)) %>%
  pull(total_votes)
votes_cast_judge_D <- judge_df %>%
  filter(VoterParty == "D") %>%
  summarise(total_votes = sum(voted, na.rm = TRUE)) %>%
  pull(total_votes)
votes_cast_council_D <- council_df %>%
  filter(VoterParty == "D") %>%
  summarise(total_votes = sum(voted, na.rm = TRUE)) %>%
  pull(total_votes)

votes_cast_ham_U <- hamilton_df %>%
  filter(VoterParty == "U") %>%
  summarise(total_votes = sum(voted, na.rm = TRUE)) %>%
  pull(total_votes)
votes_cast_cps_U <- cps_df %>%
  filter(VoterParty == "U") %>%
  summarise(total_votes = sum(voted, na.rm = TRUE)) %>%
  pull(total_votes)
votes_cast_judge_U <- judge_df %>%
  filter(VoterParty == "U") %>%
  summarise(total_votes = sum(voted, na.rm = TRUE)) %>%
  pull(total_votes)
votes_cast_council_U <- council_df %>%
  filter(VoterParty == "U") %>%
  summarise(total_votes = sum(voted, na.rm = TRUE)) %>%
  pull(total_votes)

# This data was collected on Oct 16

votes_cast_ham2 <- sum(hamilton_df2$voted)
votes_cast_cps2 <- sum(cps_df2$voted)
votes_cast_judge2 <- sum(judge_df2$voted)
votes_cast_council2 <- sum(council_df2$voted)

votes_cast_ham_R2 <- hamilton_df2 %>%
  filter(VoterParty == "R") %>%
  summarise(total_votes = sum(voted, na.rm = TRUE)) %>%
  pull(total_votes)
votes_cast_cps_R2 <- cps_df2 %>%
  filter(VoterParty == "R") %>%
  summarise(total_votes = sum(voted, na.rm = TRUE)) %>%
  pull(total_votes)
votes_cast_judge_R2 <- judge_df2 %>%
  filter(VoterParty == "R") %>%
  summarise(total_votes = sum(voted, na.rm = TRUE)) %>%
  pull(total_votes)
votes_cast_council_R2 <- council_df2 %>%
  filter(VoterParty == "R") %>%
  summarise(total_votes = sum(voted, na.rm = TRUE)) %>%
  pull(total_votes)

votes_cast_ham_D2 <- hamilton_df2 %>%
  filter(VoterParty == "D") %>%
  summarise(total_votes = sum(voted, na.rm = TRUE)) %>%
  pull(total_votes)
votes_cast_cps_D2 <- cps_df2 %>%
  filter(VoterParty == "D") %>%
  summarise(total_votes = sum(voted, na.rm = TRUE)) %>%
  pull(total_votes)
votes_cast_judge_D2 <- judge_df2 %>%
  filter(VoterParty == "D") %>%
  summarise(total_votes = sum(voted, na.rm = TRUE)) %>%
  pull(total_votes)
votes_cast_council_D2 <- council_df2 %>%
  filter(VoterParty == "D") %>%
  summarise(total_votes = sum(voted, na.rm = TRUE)) %>%
  pull(total_votes)

votes_cast_ham_U2 <- hamilton_df2 %>%
  filter(VoterParty == "U") %>%
  summarise(total_votes = sum(voted, na.rm = TRUE)) %>%
  pull(total_votes)
votes_cast_cps_U2 <- cps_df2 %>%
  filter(VoterParty == "U") %>%
  summarise(total_votes = sum(voted, na.rm = TRUE)) %>%
  pull(total_votes)
votes_cast_judge_U2 <- judge_df2 %>%
  filter(VoterParty == "U") %>%
  summarise(total_votes = sum(voted, na.rm = TRUE)) %>%
  pull(total_votes)
votes_cast_council_U2 <- council_df2 %>%
  filter(VoterParty == "U") %>%
  summarise(total_votes = sum(voted, na.rm = TRUE)) %>%
  pull(total_votes)

# This data was collected on Oct 20

turnout_ham <- votes_cast_ham/nrow(hamilton_df)
turnout_ham2 <- votes_cast_ham2/nrow(hamilton_df2)
turnout_cps <- votes_cast_cps/nrow(cps_df)
turnout_cps2 <- votes_cast_cps2/nrow(cps_df2)
turnout_judge <- votes_cast_judge/nrow(judge_df)
turnout_judge2 <- votes_cast_judge2/nrow(judge_df2)
turnout_council <- votes_cast_council/nrow(council_df)
turnout_council2 <- votes_cast_council2/nrow(council_df2)

turnout_ham_U <- votes_cast_ham_U/nrow(hamilton_df)
turnout_ham2_U <- votes_cast_ham_U2/nrow(hamilton_df2)
turnout_cps_U <- votes_cast_cps_U/nrow(cps_df)
turnout_cps2_U <- votes_cast_cps_U2/nrow(cps_df2)
turnout_judge_U <- votes_cast_judge_U/nrow(judge_df)
turnout_judge2_U <- votes_cast_judge_U2/nrow(judge_df2)
turnout_council_U <- votes_cast_council_U/nrow(council_df)
turnout_council2_U <- votes_cast_council_U2/nrow(council_df2)

turnout_ham_R <- votes_cast_ham_R/nrow(hamilton_df)
turnout_ham2_R <- votes_cast_ham_R2/nrow(hamilton_df2)
turnout_cps_R <- votes_cast_cps_R/nrow(cps_df)
turnout_cps2_R <- votes_cast_cps_R2/nrow(cps_df2)
turnout_judge_R <- votes_cast_judge_R/nrow(judge_df)
turnout_judge2_R <- votes_cast_judge_R2/nrow(judge_df2)
turnout_council_R <- votes_cast_council_R/nrow(council_df)
turnout_council2_R <- votes_cast_council_R2/nrow(council_df2)

turnout_ham_D <- votes_cast_ham_D/nrow(hamilton_df)
turnout_ham2_D <- votes_cast_ham_D2/nrow(hamilton_df2)
turnout_cps_D <- votes_cast_cps_D/nrow(cps_df)
turnout_cps2_D <- votes_cast_cps_D2/nrow(cps_df2)
turnout_judge_D <- votes_cast_judge_D/nrow(judge_df)
turnout_judge2_D <- votes_cast_judge_D2/nrow(judge_df2)
turnout_council_D <- votes_cast_council_D/nrow(council_df)
turnout_council2_D <- votes_cast_council_D2/nrow(council_df2)


#### Actually Good Code ####

hamilton_df <- hamilton_df %>%
  filter(!(`Return Ballot Date` %in% c("2025-10-02", "2025-10-03")))

hamilton_df_2021 <- hamilton_df_2021 %>%
  filter(is.na(`Return Ballot Date`) | !(`Return Ballot Date` > as.Date("2021-11-2")))

hamilton_df <- hamilton_df %>%
  mutate(days_before = as.numeric(as.Date("2025-11-04") - `Return Ballot Date`))

hamilton_df_2021 <- hamilton_df_2021 %>%
  mutate(days_before = as.numeric(as.Date("2021-11-02") - `Return Ballot Date`))


# This will give the turnout based on how many days before the election the
# "date" is.

turnout_by_date_2025 <- function(date=28){
  
  df <- hamilton_df %>%
    filter(days_before >= !!date)
  
  votes_cast <- nrow(df)
  
  turnout <- votes_cast/nrow(hamilton_df)
    
  return(turnout)
  
}


turnout_by_date_2021 <- function(date){
  
  df <- hamilton_df_2021 %>%
    filter(days_before >= !!date)
  
  votes_cast <- nrow(df)
  
  turnout <- votes_cast/nrow(hamilton_df_2021)
  
  return(turnout)
}

#### Making Data Frame ####

days_2025 <- c(18:28)

days_2021 <- c(15:0)

dates <- c(
  seq(as.Date("2025-10-07"), as.Date("2025-10-17"), by = "day"),
  seq(as.Date("2021-10-18"), as.Date("2021-11-02"), by = "day"))

df <- data.frame(
  date = dates,
  turnout = NA_real_
)

i <- 11

for(date in days_2025){

  turnout_curr <- turnout_by_date_2025(date)
  
  df$turnout[i] <- turnout_curr
  
  i <- i-1
  
}

i <- 12

for(date in days_2021){
  
  turnout_curr <- turnout_by_date_2021(date)
  
  df$turnout[i] <- turnout_curr
  
  i <- i+1
  
}




df <- df %>%
  mutate(is_2025 = ifelse(date > as.Date("2024-12-31"), 1, 0))

df <- df %>%
  mutate(days_before = ifelse(is_2025, 
                              as.numeric(as.Date("2025-11-04") - date),
                              as.numeric(as.Date("2021-11-02") - date)))


#### Graphs ####

ggplot(df, aes(x = days_before, y = turnout, color = factor(is_2025))) +
  geom_line(size = 1) +
  scale_x_reverse(
    breaks = seq(0, max(df$days_before, na.rm = TRUE), by = 5),  # tick marks every 5 days
    labels = function(x) ifelse(x == 0, "Election Day", x)       # rename 0
  ) +
  labs(
    color = "Election Year",
    x = "Days Before Election",
    y = "Absentee Ballot Return Rate"
  ) +
  scale_color_manual(
    values = c("0" = "blue", "1" = "red"),
    labels = c("2021", "2025")
  ) +
  theme_minimal()












