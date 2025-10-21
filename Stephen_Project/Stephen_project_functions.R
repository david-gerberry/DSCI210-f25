library(tidyverse)





#### Votes_Function ####


simulate_election <- function(candidates_df, race) {
  
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


create_race_slate <- function(number_of_candidates){
  
  
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
    
  
  candidates_df <<- candidates_df
}
















































































