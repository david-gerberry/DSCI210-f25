library(tidyverse)


# Define constants

final_registered_voters <- 216982
total_projected_votes <- 60625  #Actual number of total ballots (projected)
#total_votes <- 405825 #Actual number of total ballots (official)       43,034 less than projected
total_ballots <- 60625   #Use smaller number so simulation actually finishes

percentages <- c(0.25, 0.75)  # Votes distribution

# Calculate the number of votes
six_votes_on_ballot <- round(total_ballots * percentages[1])  # Votes for six candidate
seven_votes_on_ballot <- round(total_ballots * percentages[2])  # Votes for seven candidates

# Define candidates
candidates <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "21", "22", "23", "24", "25", "26", "27")

# Function to simulate the election
simulate_election <- function(candidates, six_votes_on_ballot, seven_votes_on_ballot) {
  # Create a ballot list
  total_ballots = six_votes_on_ballot + seven_votes_on_ballot
  ballots <- vector("list", total_ballots)
  
  
  for (i in 1:six_votes_on_ballot) {
    ballots[[i]] <- sample(candidates, 6)  # Six Votes
  }
  

  for (i in (six_votes_on_ballot + 1):total_ballots) {
    ballots[[i]] <- sample(candidates, 7)  # Seven Votes
  }
  
  # Count votes for each candidate
  vote_counts <- table(unlist(ballots))
  vote_counts[is.na(vote_counts)] <- 0
  
  return(as.numeric(vote_counts))
}

# Store results
results <- replicate(100, simulate_election(candidates, six_votes_on_ballot, seven_votes_on_ballot))

# Convert results to a data frame
results_df <- as.data.frame(t(results))



num_winners <- 9  # how many winners you want

results_long <- results_df %>%
  mutate(Election = row_number()) %>%
  pivot_longer(
    cols = starts_with("V"),   # candidate columns
    names_to = "Candidate",
    values_to = "Votes"
  )

results_long <- results_long %>% 
  mutate(Total_Votes_cast = seven_votes_on_ballot * 7 + six_votes_on_ballot *6)

# Get winners per election
winners <- results_long %>%
  group_by(Election) %>%
  arrange(desc(Votes)) %>%
  mutate(Rank = row_number()) %>%
  filter(Rank <= num_winners) %>%
  ungroup()

# Add MagicNumber = (next-place votes + 1) / total_votes
magic_numbers <- winners %>%
  group_by(Election) %>%
  filter(Rank == 9) %>% 
  mutate(magic_percent = Votes / Total_Votes_cast)




# Display results
head(results_df)

# Summary of winning candidates
winner_summary <- table(results_df$Winner1, results_df$Winner2)
winner_summary

magic_number_average <- mean(results_df$MagicNumber)
needed_votes <- magic_number_average*448859
needed_votes

# Basic boxplot without notches and jitter points
boxplot(
  results_df$MagicNumber, 
  main = "Magic Number Box Plot", 
  xlab = "Magic Number Percentage",
  col = "lightblue",
  horizontal = TRUE,
  border = "black",   # Change border color
  outline = TRUE          # Keep outliers visible (optional)
)

# Add the mean point
points(magic_number_average, 1, pch = 19, col = "black", cex = 1.5)  # Adjust '1' based on your y-axis

# Annotate the mean above the boxplot
text(magic_number_average, 1.3, labels = paste("Mean:", round(magic_number_average, 5)), col = "black", cex = 0.8)




# Create the histogram
hist(
  results_df$MagicNumber, 
  main = "Percentage of Votes Needed to Win Field Race", 
  xlab = "Magic Number Percentage",
  ylab = "Number of Simulations",
  col = "grey", 
  border = "black",
  breaks = 25,
  cex.main = 1.75,
  cex.lab = 1.25
)

# Add a vertical line for the mean
abline(v = mean_value, col = "black", lwd = 2)


# Add text for the mean
text(x = mean_value + 0.008, y = 80, 
     labels = paste("Mean:", round(mean_value*100, 2),"%"), 
     col = "black", cex = 2) 
