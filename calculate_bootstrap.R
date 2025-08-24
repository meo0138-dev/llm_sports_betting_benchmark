library(jsonlite)

# --- Configuration ---
INITIAL_BANKROLL <- 1000
BOOTSTRAP_REPS <- 10000
CONFIDENCE_LEVEL <- 0.95

# --- Data Loading ---
data <- fromJSON("predictions.json")
games <- data$games
results <- data$results

# --- Helper Function to Determine Profit for a Single Bet ---
# Renamed 'bet' to 'bet_row' to clarify it's a vector representing a row
determine_profit <- function(bet_row, all_results) {
  # Access elements using [] with names, as 'bet_row' is a named vector
  match_name <- bet_row["match"]
  
  if (!match_name %in% names(all_results)) {
    return(-as.numeric(bet_row["stake"])) # Pending bet, return negative stake
  }
  
  prediction <- bet_row["prediction"]
  stake <- as.numeric(bet_row["stake"]) # Explicit conversion to numeric
  odd <- as.numeric(bet_row["odd"])     # Explicit conversion to numeric
  
  score_str <- all_results[[match_name]]$score
  scores <- as.numeric(strsplit(score_str, "-")[[1]])
  home_goals <- scores[1]
  away_goals <- scores[2]
  total_goals <- home_goals + away_goals
  
  is_win <- FALSE
  
  # Evaluate prediction against score
  if (prediction == "1") { if (home_goals > away_goals) is_win <- TRUE }
  else if (prediction == "X") { if (home_goals == away_goals) is_win <- TRUE }
  else if (prediction == "2") { if (away_goals > home_goals) is_win <- TRUE }
  else if (prediction == "1X") { if (home_goals >= away_goals) is_win <- TRUE }
  else if (prediction == "X2") { if (away_goals >= home_goals) is_win <- TRUE }
  else if (prediction == "12") { if (home_goals != away_goals) is_win <- TRUE }
  else if (prediction == "GG") { if (home_goals > 0 && away_goals > 0) is_win <- TRUE }
  else if (prediction == "NG") { if (home_goals == 0 || away_goals == 0) is_win <- TRUE }
  else if (startsWith(prediction, "Over")) {
    goal_line <- as.numeric(sub("Over ", "", prediction))
    if (total_goals > goal_line) is_win <- TRUE
  } else if (startsWith(prediction, "Under")) {
    goal_line <- as.numeric(sub("Under ", "", prediction))
    if (total_goals < goal_line) is_win <- TRUE
  }
  
  if (is_win) {
    return(stake * (odd - 1))
  } else {
    return(-stake)
  }
}

# Calculate profit for every single bet
# The apply function passes each row as a vector (bet_row) to determine_profit
games$profit <- apply(games, 1, determine_profit, all_results = results)
games
# --- Main Processing ---
llms <- unique(games$llm)
leaderboard_data <- lapply(llms, function(current_llm) {
  
  llm_bets <- games[games$llm == current_llm, ]
  profit_vector <- llm_bets$profit
  
  # Handle cases where there might be no bets for an LLM or only pending bets
  if (length(profit_vector) == 0) {
    current_bankroll <- INITIAL_BANKROLL
    roi <- 0
    lower_bound <- INITIAL_BANKROLL
    upper_bound <- INITIAL_BANKROLL
  } else {
    bootstrapped_profits <- replicate(BOOTSTRAP_REPS, {
      resampled_profits <- sample(profit_vector, length(profit_vector), replace = TRUE)
      sum(resampled_profits)
    })
    
    bootstrapped_bankrolls <- INITIAL_BANKROLL + bootstrapped_profits
    
    # Use na.rm = TRUE in case of any NA values from empty profit_vector (though handled above)
    lower_bound <- quantile(bootstrapped_bankrolls, (1 - CONFIDENCE_LEVEL) / 2, na.rm = TRUE)
    upper_bound <- quantile(bootstrapped_bankrolls, 1 - (1 - CONFIDENCE_LEVEL) / 2, na.rm = TRUE)
    
    total_staked_resolved <- sum(llm_bets$stake[llm_bets$profit != -llm_bets$stake]) # Sum stakes for bets that are NOT just pending stakes
    total_profit_resolved <- sum(llm_bets$profit[llm_bets$profit != -llm_bets$stake]) # Sum profits for bets that are NOT just pending stakes
    
    current_bankroll <- INITIAL_BANKROLL + sum(llm_bets$profit) # Total bankroll includes pending bet stakes
    roi <- ifelse(total_staked_resolved > 0, (total_profit_resolved / total_staked_resolved) * 100, 0)
  }
  
  list(
    llm = current_llm,
    bankroll = round(current_bankroll, 2),
    roi = round(roi, 2),
    bankroll_ci = paste0("[", round(lower_bound), "€ - ", round(upper_bound), "€]")
  )
})

# --- Write Output JSON ---
json_output <- toJSON(leaderboard_data, pretty = TRUE, auto_unbox = TRUE)
write(json_output, "leaderboard_data.json")

cat("✅ Successfully generated leaderboard_data.json with detailed results logic.\n")
