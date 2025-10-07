library(tidyverse)


# Define constants

#final registered voters <- 603958
#total_projected_votes <- 448859.  #Actual number of total ballots (projected)
#total_votes <- 405825 #Actual number of total ballots (official)       43,034 less than projected
total_votes <- 100   #Use smaller number so simulation actually finishes
percentages <- c(0.313, 0.044, 0.6426)  # Votes distribution

# Calculate the number of votes
no_votes <- round(total_votes * percentages[1])  # No votes
one_vote <- round(total_votes * percentages[2])  # Votes for one candidate
two_votes <- round(total_votes * percentages[3])  # Votes for two candidates

# Define candidates
candidates <- c("A", "B", "C", "D", "E")

# Function to simulate the election
simulate_election <- function(candidates, one_vote, two_votes) {
  # Create a ballot list
  ballots <- vector("list", total_votes)
  
  # Assign ballots for no votes
  for (i in 1:no_votes) {
    ballots[[i]] <- c()  # No votes
  }
  
  # Assign ballots for one candidate
  for (i in (no_votes + 1):(no_votes + one_vote)) {
    ballots[[i]] <- sample(candidates, 1)  # Vote for one candidate
  }
  
  # Assign ballots for two candidates
  for (i in (no_votes + one_vote + 1):total_votes) {
    ballots[[i]] <- sample(candidates, 2)  # Vote for two candidates
  }
  
  # Count votes for each candidate
  vote_counts <- table(unlist(ballots))
  return(as.numeric(vote_counts))
}

# Store results
results <- replicate(1000, simulate_election(candidates, one_vote, two_votes))

# Convert results to a data frame
results_df <- as.data.frame(t(results))
colnames(results_df) <- candidates

# Determine winners (top 2 candidates)
results_df$Election <- 1:nrow(results_df)
results_df$Winner1 <- apply(results_df[, 1:5], 1, function(x) names(sort(x, decreasing = TRUE)[1]))
results_df$Winner2 <- apply(results_df[, 1:5], 1, function(x) names(sort(x, decreasing = TRUE)[2]))
results_df$MagicNumber <- apply(results_df[, 1:5], 1, function(x) (sort(x, decreasing = TRUE)[3]+1)/total_votes)

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
abline(v = magic_number_average, col = "black", lwd = 2)


# Add text for the mean
text(x = magic_number_average + 0.008, y = 80, 
     labels = paste("Mean:", round(magic_number_average*100, 2),"%"), 
     col = "black", cex = 2) 

