library(tidyverse)
voters_sos <- 
  read_csv("data/registered voters/voters_SOS/voters_SOS.csv")

# voters_sos <- voters_sos %>% 
#   mutate(party = PARTY_AFFILIATION) %>% 
#   if(is.na(party)) {
#     if((`PRIMARY.03/19/2024` == "R") |
#        (`PRIMARY.03/19/2024` == "o" & `PRIMARY.03/17/2020` == "R") |
#        (`PRIMARY.03/19/2024` == "o" & `PRIMARY.03/17/2020` == "o" &
#                       `PRIMARY.03/15/2016` == "R")) {
#       party = "R"
#     } else if((`PRIMARY.03/19/2024` == "D") |
#               (`PRIMARY.03/19/2024` == "o" & `PRIMARY.03/17/2020` == "D") |
#               (`PRIMARY.03/19/2024` == "o" & `PRIMARY.03/17/2020` == "o" &
#                `PRIMARY.03/15/2016` == "D")) {
#       party = "D"
#     } else {
#       party = party
#     }
#   }

voters_sos <- voters_sos %>%
  mutate(party = NA) %>% 
  mutate(
    party = ifelse(!is.na(PARTY_AFFILIATION), 
                   PARTY_AFFILIATION,
                   case_when(
                     (`PRIMARY.03/19/2024` == "R") |
                       (`PRIMARY.03/19/2024` == "o" & `PRIMARY.03/17/2020` == "R") |
                       (`PRIMARY.03/19/2024` == "o" & `PRIMARY.03/17/2020` == "o" &
                          `PRIMARY.03/15/2016` == "R") ~ "R",
                     
                     (`PRIMARY.03/19/2024` == "D") |
                       (`PRIMARY.03/19/2024` == "o" & `PRIMARY.03/17/2020` == "D") |
                       (`PRIMARY.03/19/2024` == "o" & `PRIMARY.03/17/2020` == "o" &
                          `PRIMARY.03/15/2016` == "D") ~ "D",
                     
                     TRUE ~ party  # Retain existing value if no condition is met
                   )
    )
  ) %>% 
  mutate(party = ifelse(is.na(party),
                        "U",
                        party))

table(voters_sos$party)
table(voters_sos$PARTY_AFFILIATION)


age_summary <- voters_sos %>%
  filter(AGE < 111) %>% 
  group_by(AGE, party) %>%
  summarise(count = n()) %>%
  ungroup()

# Create the line graph
ggplot(age_summary, aes(x = AGE, y = count, color = party)) +
  geom_line() +
  scale_color_manual(values = c("D" = "blue", "R" = "red", "U" = "green")) +
  labs(title = "Number of People by Age and Party",
       x = "Age (Years)",
       y = "Number of People") +
  theme_minimal()
