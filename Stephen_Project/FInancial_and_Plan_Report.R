library(tidyverse)
library(dplyr)




absentee_df <- read_csv("data/AbsenteeListExport.csv")

not_yet_voted <- data.frame()

for (i in nrow(absentee_df)) {
  
  if(is.na(absentee_df[i, 2])){
    
    not_yet_voted <- rbind(not_yet_voted, absentee_df[i, ])
  }
  
}

not_voted <- absentee_df %>% 
  filter(is.na(absentee_df$`Return Ballot Date`))


total_voter_list <- read_csv("data/latest_voter_export.csv")
total_voter_list$VoterIdent <- as.integer(total_voter_list$VoterIdent)

not_voted$VoterId <- as.integer(not_voted$VoterId)

completed_data_for_full_voter_list <- semi_join(total_voter_list, not_voted, by = c("VoterIdent" = "VoterId"))

total_voter_list_minus_absentee$BirthYear <- as.integer(total_voter_list_minus_absentee$BirthYear)
total_voter_list_minus_absentee$age <- 2025 - total_voter_list_minus_absentee$BirthYear


old_voters <- total_voter_list_minus_absentee %>% filter(total_voter_list_minus_absentee$age > 64)

young_voters <- total_voter_list_minus_absentee %>% filter(total_voter_list_minus_absentee$age < 26)


total_voter_list_minus_absentee <- anti_join(total_voter_list, completed_data_for_full_voter_list, by = "VoterIdent")



total_voter_list_minus_absentee_filtered_for_people_who_vote <- total_voter_list_minus_absentee %>% 
  filter(if_any(c("2023 General Election", "GENERAL_NOV_2021", "GENERAL_NOV_2022"), ~ !is.na(.)) & age > 64)


old_people_who_have_voted_inperson_at_primaries <- total_voter_list_minus_absentee_filtered_for_people_who_vote %>% 
  filter((!is.na(`AUG PRIMARY ELECTION 2022`) & grepl("-D", `AUG PRIMARY ELECTION 2022`))| 
           (!is.na(`2024 Primary Election`) & grepl("-D", `2024 Primary Election`))| 
           (!is.na(`2025 Primary Election`) & grepl("-D", `2025 Primary Election`))
        )

write.csv(old_people_who_have_voted_inperson_at_primaries, "data/old_dems_who_vote.csv")


young_people_in_person_or_early <- total_voter_list_minus_absentee %>% 
  filter(age < 26)


young_dems_early_or_election_day <-  young_people_in_person_or_early %>% 
  filter((!is.na(`AUG PRIMARY ELECTION 2022`) & grepl("-D", `AUG PRIMARY ELECTION 2022`))| 
            (!is.na(`2024 Primary Election`) & grepl("-D", `2024 Primary Election`))| 
            (!is.na(`2025 Primary Election`) & grepl("-D", `2025 Primary Election`))
         )
young_unaffiliated <- young_people_in_person_or_early %>% 
  filter((is.na(`AUG PRIMARY ELECTION 2022`) & is.na(`2024 Primary Election`) & is.na(`2025 Primary Election`)))


write.csv(old_people_who_have_voted_inperson_at_primaries, "data/old_dems.csv")




df1 <- read_csv("data/old_dems.csv")

df2 <- read_csv("data/old_voters_absentee.csv")

df3 <- read_csv("data/young_dems.csv")

df4 <- read_csv("data/young_voters_absentee.csv")

df5 <- read_csv("data/young_unaffiliated.csv")


combined_df <- bind_rows(df1, df2, df3, df4)


write.csv(combined_df, "data/Absentee_Early_Election_Day_voters.csv")

combined_df2 <- bind_rows(combined_df, df5)


write.csv(combined_df2, "data/Absentee_Early_Election_Day_Voters_including_unaffilliated.csv")
