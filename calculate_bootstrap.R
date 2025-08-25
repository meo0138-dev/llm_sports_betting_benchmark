library(jsonlite)
library(purrr) # Used for the %||% operator for cleaner default values

# --- Configuration ---
INITIAL_BANKROLL <- 1000
BOOTSTRAP_REPS <- 10000
CONFIDENCE_LEVEL <- 0.95

# --- Data Loading (THE FIX IS ON THIS LINE) ---
# We add simplifyDataFrame = FALSE to preserve the JSON's nested list structure
data <- fromJSON("predictions.json", simplifyDataFrame = FALSE)
# --- END OF FIX ---

# Initialize empty containers for all games and results from the entire season
all_games <- data.frame()
all_results <- list()

# Loop through each round in the JSON and combine the data
# This loop will now work correctly because data$rounds is a list of lists.
for (i in 1:length(data$rounds)) {
  round_data <- data$rounds[[i]]
  # Convert the 'games' list within the round to a data frame and append it
  if (length(round_data$games) > 0) {
    # The `do.call(rbind, ...)` is a robust way to handle the list of lists
    games_df <- do.call(rbind, lapply(round_data$games, as.data.frame))
    all_games <- rbind(all_games, games_df)
  }
  # Combine the results from the current round into the master list of all results
  if (length(round_data$results) > 0) {
    all_results <- c(all_results, round_data$results)
  }
}
cat("Found", nrow(all_games), "total bets across", length(data$rounds), "rounds.\n")


# --- Helper Function to Determine Profit (UPDATED FOR NEW BET TYPE) ---
determine_profit <- function(bet_row, results_list) {
  # This function now expects a data frame row, so we use '$' or '[[]]'
  match_name <- bet_row$match
  
  if (!match_name %in% names(results_list)) {
    return(-as.numeric(bet_row$stake)) # Pending bet
  }
  
  prediction <- bet_row$prediction
  stake <- as.numeric(bet_row$stake)
  odd <- as.numeric(bet_row$odd)
  
  score_str <- results_list[[match_name]]$score
  # Add a check for empty or invalid scores
  if (is.null(score_str) || !grepl("-", score_str)) return(-stake)
  
  scores <- as.numeric(strsplit(score_str, "-")[[1]])
  home_goals <- scores[1]
  away_goals <- scores[2]
  total_goals <- home_goals + away_goals
  
  # Pre-calculate common outcomes to simplify logic
  home_win <- home_goals > away_goals
  draw <- home_goals == away_goals
  away_win <- away_goals > home_goals
  is_gg <- home_goals > 0 && away_goals > 0
  
  is_win <- FALSE
  
  # Evaluate prediction against score
  if (prediction == "1") { if (home_win) is_win <- TRUE }
  else if (prediction == "X") { if (draw) is_win <- TRUE }
  else if (prediction == "2") { if (away_win) is_win <- TRUE }
  else if (prediction == "1X") { if (home_win || draw) is_win <- TRUE }
  else if (prediction == "X2") { if (away_win || draw) is_win <- TRUE }
  else if (prediction == "12") { if (home_win || away_win) is_win <- TRUE }
  else if (prediction == "GG") { if (is_gg) is_win <- TRUE }
  else if (prediction == "NG") { if (!is_gg) is_win <- TRUE }
  else if (startsWith(prediction, "Over")) {
    goal_line <- as.numeric(sub("Over ", "", prediction))
    if (total_goals > goal_line) is_win <- TRUE
  } else if (startsWith(prediction, "Under")) {
    goal_line <- as.numeric(sub("Under ", "", prediction))
    if (total_goals < goal_line) is_win <- TRUE
  }
  else if (prediction == "1+GG") { if (home_win && is_gg) is_win <- TRUE }
  else if (prediction == "1+NG") { if (home_win && !is_gg) is_win <- TRUE }
  else if (prediction == "X+GG") { if (draw && is_gg) is_win <- TRUE }
  else if (prediction == "X+NG") { if (draw && !is_gg) is_win <- TRUE }
  else if (prediction == "2+GG") { if (away_win && is_gg) is_win <- TRUE }
  else if (prediction == "2+NG") { if (away_win && !is_gg) is_win <- TRUE }
  
  if (is_win) {
    return(stake * (odd - 1))
  } else {
    return(-stake)
  }
}

# Use a loop that is safer than apply for heterogeneous data frames
all_games$profit <- sapply(1:nrow(all_games), function(i) determine_profit(all_games[i,], all_results))


# --- Main Processing (Now uses 'all_games') ---
llms <- unique(all_games$llm)
leaderboard_data <- lapply(llms, function(current_llm) {
  
  llm_bets <- all_games[all_games$llm == current_llm, ]
  profit_vector <- llm_bets$profit
  
  if (length(profit_vector) == 0) {
    # ... (rest of the script is unchanged) ...
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
    
    lower_bound <- quantile(bootstrapped_bankrolls, (1 - CONFIDENCE_LEVEL) / 2, na.rm = TRUE)
    upper_bound <- quantile(bootstrapped_bankrolls, 1 - (1 - CONFIDENCE_LEVEL) / 2, na.rm = TRUE)
    
    is_resolved <- llm_bets$match %in% names(all_results)
    
    total_staked_resolved <- sum(llm_bets$stake[is_resolved])
    total_profit_resolved <- sum(llm_bets$profit[is_resolved])
    
    current_bankroll <- INITIAL_BANKROLL + sum(llm_bets$profit)
    roi <- ifelse(total_staked_resolved > 0, (total_profit_resolved / total_staked_resolved) * 100, 0)
  }
  
  list(
    llm = current_llm,
    bankroll = round(current_bankroll, 2),
    roi = round(roi, 2),
    bankroll_ci = paste0("[",max(0 ,round(lower_bound)), "€ - ", round(upper_bound), "€]")
  )
})

# --- Write Output JSON ---
json_output <- toJSON(leaderboard_data, pretty = TRUE, auto_unbox = TRUE)
write(json_output, "leaderboard_data.json")


cat("✅ Successfully generated cumulative leaderboard_data.json.\n")