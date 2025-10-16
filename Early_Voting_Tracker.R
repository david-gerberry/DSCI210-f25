#### Libraries ####

library(tidyverse)

#### Data #### 

hamilton_df <- read_csv("data/AbsenteeListExport-68f0f2f025fea.csv")

#### Creating Data Frames ####

judge_df <- hamilton_df %>% 
  filter(Judicial == "JDMC04")

cps_df <- hamilton_df %>% 
  filter(School == "SCCISD")

council_df <- hamilton_df %>% 
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

#### Current "Turnout" ####

votes_cast_ham <- sum(hamilton_df$voted)

votes_cast_cps <- sum(cps_df$voted)

votes_cast_judge <- sum(judge_df$voted)

votes_cast_council <- sum(council_df$voted)















