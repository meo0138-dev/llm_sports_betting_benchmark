library(jsonlite)
library(purrr)

# --- Configuration ---
INITIAL_BANKROLL <- 1000
BOOTSTRAP_REPS <- 10000
CONFIDENCE_LEVEL <- 0.95

# --- AI Name Mapping ---
# Must match the mapping in index.html
apply_llm_mapping <- function(raw_name) {
  mapping <- list(
    "Claude Opus 4.1" = "Claude Sonnet 4.5",
    "Claude Sonnet 4.5" = "Claude Sonnet 4.5",
    "Gemini 2.5 Pro" = "Gemini 3.0 Pro",
    "Deepseek V3.1" = "Deepseek V3.2",
    "GPT5" = "GPT 5.1",
    "Grok 4" = "Grok 4.1",
    "Grok 4.1" = "Grok 4.1",
    "Qwen 3" = "Qwen 3"
  )
  
  if (raw_name %in% names(mapping)) {
    return(mapping[[raw_name]])
  } else {
    return(raw_name)
  }
}

# --- Data Loading ---
data <- fromJSON("predictions.json", simplifyDataFrame = FALSE)

# --- Robust Data Frame Creation ---
all_games <- data.frame()
all_results <- list()
all_possible_cols <- c("match", "prediction", "stake", "odd", "llm", "reasoning")

for (round_data in data$rounds) {
  if (length(round_data$games) > 0) {
    for (game in round_data$games) {
      game_df <- as.data.frame(game, stringsAsFactors = FALSE)
      
      # Normalize columns
      missing_cols <- setdiff(all_possible_cols, names(game_df))
      if (length(missing_cols) > 0) { game_df[, missing_cols] <- NA }
      game_df <- game_df[, all_possible_cols]
      
      # Apply Name Mapping Here
      game_df$llm <- apply_llm_mapping(game_df$llm)
      
      all_games <- rbind(all_games, game_df)
    }
  }
  if (length(round_data$results) > 0) { 
    all_results <- c(all_results, round_data$results) 
  }
}
cat("Found", nrow(all_games), "total bets across", length(data$rounds), "rounds.\n")

# --- Helper Function (Profit Calculation) ---
determine_profit <- function(bet_row, results_list) {
  match_name <- bet_row$match
  if (!match_name %in% names(results_list)) return(NA)  # unresolved bet
  
  prediction <- bet_row$prediction
  stake <- as.numeric(bet_row$stake)
  odd <- as.numeric(bet_row$odd)
  
  score_str <- results_list[[match_name]]$score
  if (is.null(score_str) || score_str == "" || !grepl("-", score_str)) return(NA)
  
  scores <- as.numeric(strsplit(score_str, "-")[[1]])
  home_goals <- scores[1]; away_goals <- scores[2]; total_goals <- home_goals + away_goals
  
  home_win <- home_goals > away_goals
  draw <- home_goals == away_goals
  away_win <- away_goals > home_goals
  is_gg <- home_goals > 0 && away_goals > 0
  
  is_win <- FALSE
  
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
  }
  else if (startsWith(prediction, "Under")) { 
    goal_line <- as.numeric(sub("Under ", "", prediction)) 
    if (total_goals < goal_line) is_win <- TRUE 
  }
  else if (prediction == "1+GG") { if (home_win && is_gg) is_win <- TRUE }
  else if (prediction == "1+NG") { if (home_win && !is_gg) is_win <- TRUE }
  else if (prediction == "X+GG") { if (draw && is_gg) is_win <- TRUE }
  else if (prediction == "X+NG") { if (draw && !is_gg) is_win <- TRUE }
  else if (prediction == "2+GG") { if (away_win && is_gg) is_win <- TRUE }
  else if (prediction == "2+NG") { if (away_win && !is_gg) is_win <- TRUE }
  
  if (is_win) return(stake * (odd - 1)) else return(-stake)
}

# Calculate profit for all games
all_games$profit <- sapply(1:nrow(all_games), function(i) determine_profit(all_games[i,], all_results))

# --- Main Processing ---
llms <- unique(all_games$llm)
leaderboard_data <- lapply(llms, function(current_llm) {
  
  llm_bets <- all_games[all_games$llm == current_llm, ]
  profit_vector <- na.omit(llm_bets$profit)  # only resolved bets
  
  if (length(profit_vector) == 0) {
    current_bankroll <- INITIAL_BANKROLL
    roi <- 0
    lower_bound <- INITIAL_BANKROLL
    upper_bound <- INITIAL_BANKROLL
  } else {
    # Bootstrap
    bootstrapped_profits <- replicate(BOOTSTRAP_REPS, { sum(sample(profit_vector, length(profit_vector), replace = TRUE)) })
    bootstrapped_bankrolls <- INITIAL_BANKROLL + bootstrapped_profits
    lower_bound <- quantile(bootstrapped_bankrolls, (1 - CONFIDENCE_LEVEL) / 2, na.rm = TRUE)
    upper_bound <- quantile(bootstrapped_bankrolls, 1 - (1 - CONFIDENCE_LEVEL) / 2, na.rm = TRUE)
    
    # ROI
    resolved_bets <- llm_bets[!is.na(llm_bets$profit), ]
    total_staked_resolved <- sum(as.numeric(resolved_bets$stake))
    total_profit_resolved <- sum(as.numeric(resolved_bets$profit))
    roi <- ifelse(total_staked_resolved > 0, (total_profit_resolved / total_staked_resolved) * 100, 0)
    
    # Bankroll
    current_bankroll <- INITIAL_BANKROLL + sum(resolved_bets$profit)
  }
  
  list(
    llm = current_llm,
    bankroll = round(current_bankroll, 2),
    roi = round(roi, 2),
    bankroll_ci = paste0("[", max(0, round(lower_bound)), "€ - ", round(upper_bound), "€]")
  )
})

# --- Write Output ---
json_output <- toJSON(leaderboard_data, pretty = TRUE, auto_unbox = TRUE)
write(json_output, "leaderboard_data.json")

cat("✅ Successfully generated cumulative leaderboard_data.json with name mapping.\n")
