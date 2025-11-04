


simulate_election_DG <- function(candidates, winners, j, total_voters) {
  # Create a ballot list
  ballots <- vector("list", total_voters)
  
  for (i in 1:total_voters) {
    # Count votes for each candidate so far
    vote_counts <- table(factor(unlist(ballots), levels = candidates))
    p <- as.numeric(vote_counts)
    
    # Sample with reinforcement
    ballots[[i]] <- sample(
      x = candidates,
      size = winners,
      prob = (p + j) / sum(p + j)
    )
  }
  
  # Final vote tally
  vote_counts <- table(factor(unlist(ballots), levels = candidates))
  
  # Return named numeric vector
  return(as.numeric(vote_counts))
}


# Dave Gerberry - Function to simulate the election with candidates bias
total_voters <- 100
C = 12   ## number of candidates
winners=9
j=2

# Store results
results <- replicate(500, simulate_election_DG(candidates = LETTERS[1:C],winners=winners,j=j,total_voters = total_voters ))

# Convert results to a data frame
results_df <- as.data.frame(t(results))
colnames(results_df) <- LETTERS[1:C]

# Determine winners (top 2 candidates)
results_df$Election <- 1:nrow(results_df)
results_df$LastWinner <- apply(results_df[, 1:C], 1, function(x) names(sort(x, decreasing = TRUE)[winners]))
results_df$FirstLoser <- apply(results_df[, 1:C], 1, function(x) names(sort(x, decreasing = TRUE)[winners+1]))
results_df$MagicNumber <- apply(results_df[, 1:C], 1, function(x) (sort(x, decreasing = TRUE)[winners]+sort(x, decreasing = TRUE)[winners+1])/(2*total_voters))

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
