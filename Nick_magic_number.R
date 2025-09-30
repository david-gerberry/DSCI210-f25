library(tidyverse)
##run election results firts

# Define constants
dropOffE = 0.2365
#final registered voters <- 603958
#total_projected_votes <- 448859.  #Actual number of total ballots (projected)
#total_votes <- 405825 #Actual number of total ballots (official)       43,034 less than projected
total_votes <- 1000   #Use smaller number so simulation actually finishes
percentages <- c(dropOffE, 0, 0, 0, 0.7635)  # Votes distribution

# Calculate the number of votes
no_votes <- round(total_votes * percentages[1])  # No votes
one_vote <- round(total_votes * percentages[2])  # Votes for one candidate
two_votes <- round(total_votes * percentages[3])  # Votes for two candidates
three_votes <- round(total_votes * percentages[4]) #Votes for three candidates
four_votes <- round(total_votes * percentages[5]) #Votes for four candidates

# Define candidates
candidates <- c("A", "B", "C", "D", "E")

# Function to simulate the election
simulate_election <- function(candidates, no_votes, four_votes) {
  # Create a ballot list
  ballots <- vector("list", total_votes)
  for (i in 1:no_votes) {
    ballots[[i]] <- c()  # No votes
  }
  for (i in (no_votes + one_vote + two_votes + three_votes + 1):total_votes) {
    ballots[[i]] <- sample(candidates, 2)  # Vote for two candidates
  }
  # Count votes for each candidate
  vote_counts <- table(unlist(ballots))
  return(as.numeric(vote_counts))
}

# Store results
results <- replicate(1000, simulate_election(candidates, no_votes, four_votes))

# Convert results to a data frame
results_df <- as.data.frame(t(results))
colnames(results_df) <- candidates

# Determine winners (top 4 candidates)
results_df$Election <- 1:nrow(results_df)
results_df$Winner1 <- apply(results_df[, 1:5], 1, function(x) names(sort(x, decreasing = TRUE)[1]))
results_df$Winner2 <- apply(results_df[, 1:5], 1, function(x) names(sort(x, decreasing = TRUE)[2]))
results_df$Winner3 <- apply(results_df[, 1:5], 1, function(x) names(sort(x, decreasing = TRUE)[3]))
results_df$Winner4 <- apply(results_df[, 1:5], 1, function(x) names(sort(x, decreasing = TRUE)[4]))
results_df$Winner4Votes <- apply(results_df[, 1:5], 1, function(x) sort(x, decreasing = TRUE)[4])
results_df <- results_df %>% 
  mutate(MagicNumber = (Winner4Votes+1)/total_votes)

# Display results
head(results_df)



magic_number_average <- mean(results_df$MagicNumber)
needed_votes <- magic_number_average * (((BallotSum19 + BallotSum21)/2)*dropOffE)
print(needed_votes)
mean_value <- magic_number_average

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

