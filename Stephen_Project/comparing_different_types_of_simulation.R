library(tidyverse)
library(dplyr)



#### Real clean data ####

df_2013 <- read.csv("data/2013_Cincinnati_city_council_wide.csv")

df_2013 <- df_2013 %>% 
  mutate(Total_votes_for = rowSums(across(2:176))) %>% 
  select(1, Total_votes_for, everything()) 


df_2013 <- df_2013[-1,]
write.csv(df_2013, "data/2013_Cincinnati_city_council_wide.csv", row.names = FALSE)

df_2017 <- read.csv("data/2017_Cincinnati_city_council_wide.csv")

df_2017 <- df_2017 %>% 
  mutate(Total_votes_for = rowSums(across(2:189))) %>% 
  select(1, Total_votes_for, everything())

df_2021 <- read.csv("data/2021_Cincinnati_city_council_wide.csv")

df_2021 <- df_2021 %>% 
  mutate(Total_votes_for = rowSums(across(2:191))) %>% 
  select(1, Total_votes_for, everything())



#### Candidate_DF Set ups ####



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

slate_2021 <- create_race_slate()

write.csv(slate_2021, "slate_2021.csv")

slate_2017 <- create_race_slate()

write.csv(slate_2017, "slate_2017.csv")


slate_2013 <- create_race_slate()

#slate_2013[18,3] <- "R"

write.csv(slate_2013, "data/slate_2013.csv", row.names = FALSE)


#### Election Simulation Without bias ####

#changed the City council section to more closely match reality of the 2013 election

simulate_election <- function(candidates_df = candidates_df, race = "City Council") {
  
  if (race == "City Council"){
    
    #This adds a row weight at a natural state of no influence
    
    candidates_df$weight <- 1
    
    #This upticks the status of incumbants by 10%  
    candidates_df$weight[candidates_df$Incumbancy_Status == "I"] <-
      candidates_df$weight[candidates_df$Incumbancy_Status == "I"] * 1
    
    #This drops republican likelyhood by 20%  
    candidates_df$weight[candidates_df$Party_Affiliation == "R"] <-
      candidates_df$weight[candidates_df$Party_Affiliation == "R"] * 1
    
    #This increases democrat liklyhood by 20%  
    candidates_df$weight[candidates_df$Party_Affiliation == "D"] <-
      candidates_df$weight[candidates_df$Party_Affiliation == "D"] * 1
    
    #This decreases third party  
    candidates_df$weight[candidates_df$Party_Affiliation == "Third"] <-
      candidates_df$weight[candidates_df$Party_Affiliation == "Third"] * 1
    
    #This decreases unafilliated
    candidates_df$weight[candidates_df$Party_Affiliation == "Un"] <-
      candidates_df$weight[candidates_df$Party_Affiliation == "Un"] * 1
    
    #makes the weights standardized or something like that so there is no total probability imbalance
    candidates_df$weight <- candidates_df$weight / sum(candidates_df$weight)  
    
    #percentage of ballots from total that will be cast
    
    percentage_of_ballots_cast <- .249
    
    
    #final number of ballots in given election
    Ballots <- 215985
    
    Total_Ballots <- Ballots * percentage_of_ballots_cast
    
    #vote probabiliy 
    
    vote_prob <- c(0.00, 0.00, 0.01, 0.02, 0.03, 0.10, 0.15, 0.25, 0.25, 0.19)
    
    vote_prob <- vote_prob / sum(vote_prob) 
    
    
    sum(0:9 * vote_prob)
    
    ballots <- vector("list", Total_Ballots)
    
    for(i in 1:Total_Ballots){
      num_votes <- sample(0:9, size = 1, replace = TRUE, prob = vote_prob)
      
      
      if(num_votes > 0){
        ballots[[i]] <- sample(candidates_df$Name, num_votes, prob = candidates_df$weight)
        
      }
      else{
        ballots[[i]] <- character[0]
      }
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



election_with_no_bias <- simulate_election(candidates_df = slate_2013, race = "City Council")

write.csv(election_with_no_bias, "data/election_with_no_bias.csv")

#### Simulation with bias ####

simulate_election_with_bias <- function(candidates_df = candidates_df, race = "City Council") {
  
  if (race == "City Council"){
    
    #This adds a row weight at a natural state of no influence
    
    candidates_df$weight <- 1
    
    #This upticks the status of incumbants by 30%  
    candidates_df$weight[candidates_df$Incumbancy_Status == "I"] <-
      candidates_df$weight[candidates_df$Incumbancy_Status == "I"] * 1.3
    
    
    candidates_df$weight[candidates_df$Party_Affiliation == "R"] <-
      candidates_df$weight[candidates_df$Party_Affiliation == "R"] * 1
    
    
    candidates_df$weight[candidates_df$Party_Affiliation == "D"] <-
      candidates_df$weight[candidates_df$Party_Affiliation == "D"] * 1
    
    
    candidates_df$weight[candidates_df$Party_Affiliation == "Third"] <-
      candidates_df$weight[candidates_df$Party_Affiliation == "Third"] * 1
    
    
    candidates_df$weight[candidates_df$Party_Affiliation == "Un"] <-
      candidates_df$weight[candidates_df$Party_Affiliation == "Un"] * 1
    
    #makes the weights standardized or something like that so there is no total probability imbalance
    candidates_df$weight <- candidates_df$weight / sum(candidates_df$weight)  
    
    #percentage of ballots from total that will be cast
    
    percentage_of_ballots_cast <- .249
    
    
    #final number of ballots in given election
    Ballots <- 215985
    
    Total_Ballots <- Ballots * percentage_of_ballots_cast
    
    #vote probabiliy 
    
    vote_prob <- c(0.00, 0.00, 0.01, 0.02, 0.03, 0.10, 0.15, 0.25, 0.25, 0.19)
    
    vote_prob <- vote_prob / sum(vote_prob) 
    
    
    sum(0:9 * vote_prob)
    
    ballots <- vector("list", Total_Ballots)
    
    for(i in 1:Total_Ballots){
      num_votes <- sample(0:9, size = 1, replace = TRUE, prob = vote_prob)
      
      
      if(num_votes > 0){
        ballots[[i]] <- sample(candidates_df$Name, num_votes, prob = candidates_df$weight)
        
      }
      else{
        ballots[[i]] <- character[0]
      }
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


simulated_election_with_bias <- simulate_election_with_bias(candidates_df = slate_2013, race = "City Council")

write.csv(simulated_election_with_bias, "data/simulated_election_with_bias.csv")



#### Merging Things ####

merged_df <- merge(simulated_election_with_bias, slate_2013, by = "Name")

merged_df[18, 1] <- "Timothy Joseph Dornbusch"

df_2013 <- df_2013 %>%
  left_join(merged_df, by = c("Candidate" = "Name")) %>%
  select(1, Votes, Incumbancy_Status, Party_Affiliation, everything())
  