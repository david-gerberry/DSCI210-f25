library(tidyverse)
library(tidyr)
library(dplyr)



#### Votes_Function ####


simulate_election <- function(candidates_df = candidates_df, race = "City Council") {
  
  if (race == "City Council"){
    
  #This adds a row weight at a natural state of no influence
    
  candidates_df$weight <- 1
    
  #This upticks the status of incumbants by 10%  
  candidates_df$weight[candidates_df$Incumbancy_Status == "I"] <-
    candidates_df$weight[candidates_df$Incumbancy_Status == "I"] * 1.1
    
  #This drops republican likelyhood by 20%  
  candidates_df$weight[candidates_df$Party_Affiliation == "R"] <-
    candidates_df$weight[candidates_df$Party_Affiliation == "R"] * .80
  
  #This increases democrat liklyhood by 20%  
  candidates_df$weight[candidates_df$Party_Affiliation == "D"] <-
    candidates_df$weight[candidates_df$Party_Affiliation == "D"] * 1.2
  
  #This decreases third party  
  candidates_df$weight[candidates_df$Party_Affiliation == "Third"] <-
    candidates_df$weight[candidates_df$Party_Affiliation == "Third"] * .5
  
  #This decreases unafilliated
  candidates_df$weight[candidates_df$Party_Affiliation == "Un"] <-
    candidates_df$weight[candidates_df$Party_Affiliation == "Un"] * .4
  
  #makes the weights standardized or something like that so there is no total probability imbalance
  candidates_df$weight <- candidates_df$weight / sum(candidates_df$weight)  
  
  #percentage of ballots from total that will be cast
  
  percentage_of_ballots_cast <- .2994
  
  
  #final number of ballots in given election
  Ballots <- as.integer(percentage_of_ballots_cast  * (length(count.fields("data/VoterListExport-20251021-CICINC-CI-no.csv", sep = ",")) - 1))

  #people who vote for six candidates
  six_votes_on_ballot <- trunc(Ballots * .25)
  
  #people who vote for seven candidates
  seven_votes_on_ballot <- trunc(Ballots * .75)
  
  
  Total_ballots <- seven_votes_on_ballot + six_votes_on_ballot
  ballots <- vector("list", Total_ballots)
  
  for (i in 1:six_votes_on_ballot) {
    ballots[[i]] <- sample(candidates_df$Name, 6, prob = candidates_df$weight)
  }
  
  for (i in (six_votes_on_ballot + 1):Total_ballots) {
    ballots[[i]] <- sample(candidates_df$Name, 7, prob = candidates_df$weight)
  }
  
  
  # Count votes for each candidate
  vote_counts <- table(unlist(ballots))
  vote_counts[is.na(vote_counts)] <- 0
  
  #assigns all the votes to a df with names of candidates
  vote_counts_df <- data.frame(Name = names(vote_counts), Votes = as.numeric(vote_counts))
  
  #returns a df
  return(vote_counts_df)
  }
  
  if (race == "Munciple Court"){
    
    
    #This adds a row weight at a natural state of no influence
    
    candidates_df$weight <- 1
    
    #This upticks the status of incumbants by 10%  
    candidates_df$weight[candidates_df$Incumbancy_Status == "I"] <-
      candidates_df$weight[candidates_df$Incumbancy_Status == "I"] * 1.1
    
    #This drops republican likelyhood by 20%  
    candidates_df$weight[candidates_df$Party_Affiliation == "R"] <-
      candidates_df$weight[candidates_df$Party_Affiliation == "R"] * .80
    
    #This increases democrat liklyhood by 20%  
    candidates_df$weight[candidates_df$Party_Affiliation == "D"] <-
      candidates_df$weight[candidates_df$Party_Affiliation == "D"] * 1.2
    
    #This decreases third party  
    candidates_df$weight[candidates_df$Party_Affiliation == "Third"] <-
      candidates_df$weight[candidates_df$Party_Affiliation == "Third"] * .5
    
    #This decreases unafilliated
    candidates_df$weight[candidates_df$Party_Affiliation == "Un"] <-
      candidates_df$weight[candidates_df$Party_Affiliation == "Un"] * .4

    #makes the weights standardized or something like that so there is no total probability imbalance        
    candidates_df$weight <- candidates_df$weight / sum(candidates_df$weight)
    
    percentage_of_ballots_cast <- .32
    
    #final number of ballots in given election
    Ballots <- as.integer(percentage_of_ballots_cast  * (length(count.fields("data/VoterListExport-20251021-CICINC-CI-no.csv", sep = ",")) - 1))
    
    #people who vote for six candidates
    six_votes_on_ballot <- trunc(Ballots * .25)
    
    #people who vote for seven candidates
    seven_votes_on_ballot <- trunc(Ballots * .75)
    
    
    Total_ballots <- seven_votes_on_ballot + six_votes_on_ballot
    ballots <- vector("list", Total_ballots)
    
    for (i in 1:six_votes_on_ballot) {
      ballots[[i]] <- sample(candidates_df$Name, 6, prob = candidates_df$weight)
    }
    
    for (i in (six_votes_on_ballot + 1):Total_ballots) {
      ballots[[i]] <- sample(candidates_df$Name, 7, prob = candidates_df$weight)
    }
    
    # Count votes for each candidate
    vote_counts <- table(unlist(ballots))
    vote_counts[is.na(vote_counts)] <- 0
    
    #assigns all the votes to a df with names of candidates
    vote_counts_df <- data.frame(Name = names(vote_counts), Votes = as.numeric(vote_counts))
    
    #returns a df to the environment
    return(vote_counts_df)
  }
  
  if (race == "CPS"){
    
    #This adds a row weight at a natural state of no influence
    
    candidates_df$weight <- 1
    
    #This upticks the status of incumbants by 10%  
    candidates_df$weight[candidates_df$Incumbancy_Status == "I"] <-
      candidates_df$weight[candidates_df$Incumbancy_Status == "I"] * 1.1
    
    #This drops republican likelyhood by 20%  
    candidates_df$weight[candidates_df$Party_Affiliation == "R"] <-
      candidates_df$weight[candidates_df$Party_Affiliation == "R"] * .80
    
    #This increases democrat liklyhood by 20%  
    candidates_df$weight[candidates_df$Party_Affiliation == "D"] <-
      candidates_df$weight[candidates_df$Party_Affiliation == "D"] * 1.2
    
    #This decreases third party  
    candidates_df$weight[candidates_df$Party_Affiliation == "Third"] <-
      candidates_df$weight[candidates_df$Party_Affiliation == "Third"] * .5
    
    #This decreases unafilliated
    candidates_df$weight[candidates_df$Party_Affiliation == "Un"] <-
      candidates_df$weight[candidates_df$Party_Affiliation == "Un"] * .4
    
    
    #makes the weights standardized or something like that so there is no total probability imbalance    
    candidates_df$weight <- candidates_df$weight / sum(candidates_df$weight)
    
    percentage_of_ballots_cast <- .35

    #final number of ballots in given election
    Ballots <- as.integer(percentage_of_ballots_cast  * (length(count.fields("data/VoterListExport-20251021-CICINC-CI-no.csv", sep = ",")) - 1))
    
    #people who vote for six candidates
    six_votes_on_ballot <- trunc(Ballots * .25)
    
    #people who vote for seven candidates
    seven_votes_on_ballot <- trunc(Ballots * .75)
    
    
    Total_ballots <- seven_votes_on_ballot + six_votes_on_ballot
    ballots <- vector("list", Total_ballots)
    
    for (i in 1:six_votes_on_ballot) {
      ballots[[i]] <- sample(candidates_df$Name, 6, prob = candidates_df$weight)
    }
    
    for (i in (six_votes_on_ballot + 1):Total_ballots) {
      ballots[[i]] <- sample(candidates_df$Name, 7, prob = candidates_df$weight)
    }
    
    # Count votes for each candidate
    vote_counts <- table(unlist(ballots))
    vote_counts[is.na(vote_counts)] <- 0
    
    #assigns all the votes to a df with names of candidates
    vote_counts_df <- data.frame(Name = names(vote_counts), Votes = as.numeric(vote_counts))
    
    #returns a df to the environment
    return(vote_counts_df)
  }
  
}



#### Candidates Data Frame ####

candidates_df <- data.frame(Name = character(), 
                           Incumbancy_Status = character(), 
                           Party_Affiliation = character())



#### adding Candidates to Data Frame ####

#This essentially becomes useless as a function and instead just becomes a loop
# in the next function


add_candidate <- function(candidates_df){
  name <- readline(prompt = "Enter the Candidate's first and last name: ")
  Incumbancy_Status <- readline("Enter the Candidate's Incumbancy Status (I/NI): ")
  Party_Affiliation <- readline("Enter the Candidates's Party Affiliation (R/D/Third/Un): ")
  
  new_row <- data.frame(
    Name = name,
    Incumbancy_Status = Incumbancy_Status,
    Party_Affiliation = Party_Affiliation
    
  )
  
  candidates_df <- rbind(candidates_df, new_row)
  return(candidates_df)
}



#### Candidates and candidates_df combined ####


create_race_slate <- function(){
  
  number_of_candidates <- readline(promp = "Enter the Number of Candidates in your Race: ")
  
  # make the Candidate Data Frame
    candidates_df <- data.frame(Name = character(), 
                             Incumbancy_Status = character(), 
                             Party_Affiliation = character())
  
  # repeat the function asking for the candidate data
  
  
    for (i in 1:number_of_candidates) {
      cat("\n--- Enter details for Candidate", i, "---\n")
      
      name <- readline(prompt = "Enter the Candidate's first and last name: ")
      Incumbancy_Status <- readline("Enter the Candidate's Incumbancy Status (I/NI): ")
      Party_Affiliation <- readline("Enter the Candidates's Party Affiliation (R/D/Third/Un): ")
      
      new_row <- data.frame(
        Name = name,
        Incumbancy_Status = Incumbancy_Status,
        Party_Affiliation = Party_Affiliation
      )
      
      candidates_df <- rbind(candidates_df, new_row)
    }
    
  
  return(candidates_df)
}

candidates_df <- create_race_slate()
















































































#### Messing with Real Data ####



City_of_CIn_City_Council_2021_cleaned <- read.csv("data/City_of_CIn_City_Council_2021.csv")

City_of_CIn_City_Council_2021_cleaned <- City_of_CIn_City_Council_2021_cleaned[-1, ]


write.csv(City_of_CIn_City_Council_2021_cleaned, "data/City_of_CIn_City_Council_2021_cleaned.csv")
City_of_CIn_City_Council_2017_cleaned <- City_of_CIn_City_Council_2017_cleaned[-1, ]

colnames(City_of_CIn_City_Council_2021_cleaned) <- as.character(unlist(City_of_CIn_City_Council_2021_cleaned[1, ]))

City_of_CIn_City_Council_2017_cleaned <- City_of_CIn_City_Council_2017_cleaned[-1, ]

City_of_CIn_City_Council_2017_cleaned <- subset(City_of_CIn_City_Council_2017_cleaned, select = -1)




df_2021 <- read_csv("data/City_of_CIn_City_Council_2021_cleaned.csv") 
df_2021 <- df_2021[1:190, ]
df_2021 <- subset(df_2021, select = -1)
write.csv(df_2021, "data/City_of_CIn_City_Council_2021_cleaned.csv", row.names = FALSE)


df_2017 <- read_csv("data/City_of_CIn_City_Council_2017_cleaned.csv")
df_2017 <- df_2017[1:188, ]
df_2017 <- subset(df_2017, select = -1)
write.csv(df_2017, "data/City_of_CIn_City_Council_2017_cleaned.csv", row.names = FALSE)


df_2013 <- read_csv("data/City_of_CIn_City_Council_2013_cleaned.csv")
df_2013 <- df_2013[1:175, ]
df_2013 <- subset(df_2013, select = -5)
write.csv(df_2013, "data/City_of_CIn_City_Council_2013_cleaned.csv", row.names = FALSE)


df_2021 <- subset(df_2021, select = -1)



df_long <- df_2021 %>%
  pivot_longer(
    cols = c(2,3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20,21,22,23,24,25,26,27, 28, 29,30,31,32,33,34,35,36,37,38),  
    names_to = "Candidate",
    values_to = "Votes"
  )

df_long <- subset(df_long, select = -3)
df_long <- subset(df_long, select = -3)
df_long <- subset(df_long, select = -1)

df_wide <- df_long %>%
  pivot_wider(
    names_from = PRECINCT,
    values_from = Votes
  )

df_wide <- read.csv("data/2013_Cincinnati_city_council_wide.csv")
df_wide <-  df_wide[-1,]
write.csv(df_wide, "data/2021_Cincinnati_city_council_wide.csv", row.names = FALSE)


#### Real clean data ####

df_2013 <- read.csv("data/2013_Cincinnati_city_council_wide.csv")

df_2013 <- df_2013 %>% 
  mutate(Total_votes_for = rowSums(across(2:177))) %>% 
  select(1, Total_votes_for, everything())

df_2017 <- read.csv("data/2017_Cincinnati_city_council_wide.csv")

df_2017 <- df_2017 %>% 
  mutate(Total_votes_for = rowSums(across(2:189))) %>% 
  select(1, Total_votes_for, everything())

df_2021 <- read.csv("data/2021_Cincinnati_city_council_wide.csv")

df_2021 <- df_2021 %>% 
  mutate(Total_votes_for = rowSums(across(2:191))) %>% 
  select(1, Total_votes_for, everything())
