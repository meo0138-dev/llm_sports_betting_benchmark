# calculate_bootstrap.R

# Install jsonlite if you haven't already
# install.packages("jsonlite")

library(jsonlite)

# --- Configuration ---
INITIAL_BANKROLL <- 1000
BOOTSTRAP_REPS <- 10000 # Number of bootstrap iterations
CONFIDENCE_LEVEL <- 0.95

# --- Load Data ---
# Read the predictions from the JSON file
data <- fromJSON("predictions.json")
games <- data$games
results <- data$results # The results object from your JSON

# --- Calculate Profit/Loss for each Bet ---
# This part will work once you populate the 'results' object in your JSON
# For now, if a result is missing, we assume the bet is pending and profit is 0.
games$profit <- sapply(games$match, function(match_name) {
  result <- results[[match_name]]
  stake <- games$stake[games$match == match_name]
  odd <- games$odd[games$match == match_name]

  if (is.null(result)) {
    # If no result, the stake is committed but profit is not yet realized
    return(-stake) # We only account for the money out (stake)
  } else if (result == "W") {
    return(stake * (odd - 1))
  } else { # Assumes "L" for loss
    return(-stake)
  }
})

# --- Process Each LLM ---
llms <- unique(games$llm)
leaderboard_data <- lapply(llms, function(current_llm) {
  
  # Filter bets for the current LLM
  llm_bets <- games[games$llm == current_llm, ]
  
  # Get the vector of profits for bootstrapping
  profit_vector <- llm_bets$profit
  
  # Perform bootstrapping
  bootstrapped_profits <- replicate(BOOTSTRAP_REPS, {
    # Sample with replacement from the profit vector
    resampled_profits <- sample(profit_vector, length(profit_vector), replace = TRUE)
    # Sum the profits of the resampled bets
    sum(resampled_profits)
  })
  
  # Calculate bootstrapped final bankrolls
  bootstrapped_bankrolls <- INITIAL_BANKROLL + bootstrapped_profits
  
  # Calculate the confidence interval
  lower_bound <- quantile(bootstrapped_bankrolls, (1 - CONFIDENCE_LEVEL) / 2)
  upper_bound <- quantile(bootstrapped_bankrolls, 1 - (1 - CONFIDENCE_LEVEL) / 2)
  
  # Calculate current bankroll and ROI
  total_staked <- sum(llm_bets$stake)
  total_profit <- sum(profit_vector)
  current_bankroll <- INITIAL_BANKROLL + total_profit
  roi <- ifelse(total_staked > 0, (total_profit / total_staked) * 100, 0)
  
  # Return a structured list for this LLM
  list(
    llm = current_llm,
    bankroll = round(current_bankroll, 2),
    roi = round(roi, 2),
    bankroll_ci = paste0("[", round(lower_bound), "€ - ", round(upper_bound), "€]")
  )
})

# --- Write Output JSON ---
# Convert the list to a JSON string and write to file
json_output <- toJSON(leaderboard_data, pretty = TRUE, auto_unbox = TRUE)
write(json_output, "leaderboard_data.json")

cat("✅ Successfully generated leaderboard_data.json\n")
