library(jsonlite)
library(purrr)

# --- Configuration ---
INITIAL_BANKROLL <- 1000
BOOTSTRAP_REPS <- 10000
CONFIDENCE_LEVEL <- 0.95

# --- AI Name Mapping ---
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

# --- Global Results Map ---
all_results <- list()
for (round_data in data$rounds) {
  if (length(round_data$results) > 0) { 
    all_results <- c(all_results, round_data$results) 
  }
}

# --- Helper Function (Profit Calculation) ---
determine_profit <- function(match_name, prediction, stake, odd, results_list) {
  if (is.null(match_name) || is.null(prediction) || is.null(stake) || is.null(odd)) return(0) # Safety
  if (!match_name %in% names(results_list)) return(0)  # Unresolved bet = 0 change in bankroll for chart
  
  score_str <- results_list[[match_name]]$score
  if (is.null(score_str) || score_str == "" || !grepl("-", score_str)) return(0)
  
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
  
  stake_val <- as.numeric(stake)
  odd_val <- as.numeric(odd)
  
  if (is_win) return(stake_val * (odd_val - 1)) else return(-stake_val)
}

# ==========================================
# PART 1: Generate Leaderboard Data (Bootstrap)
# ==========================================

# Flatten data for bootstrap analysis
all_games_df <- data.frame()
all_possible_cols <- c("match", "prediction", "stake", "odd", "llm")

for (round_data in data$rounds) {
  if (length(round_data$games) > 0) {
    for (game in round_data$games) {
      game_df <- as.data.frame(game, stringsAsFactors = FALSE)
      # Normalize columns
      missing_cols <- setdiff(all_possible_cols, names(game_df))
      if (length(missing_cols) > 0) { game_df[, missing_cols] <- NA }
      game_df <- game_df[, all_possible_cols]
      
      # Apply Name Mapping
      game_df$llm <- apply_llm_mapping(game_df$llm)
      
      # Calculate profit immediately (returns NA if unresolved, for filtering later)
      # Note: We use a slightly different logic here to detect NAs explicitly for ROI calc
      profit <- determine_profit(game_df$match, game_df$prediction, game_df$stake, game_df$odd, all_results)
      
      # Determine if resolved for ROI calculation
      is_resolved <- game_df$match %in% names(all_results) && 
                     !is.null(all_results[[game_df$match]]$score) && 
                     all_results[[game_df$match]]$score != ""
      
      game_df$profit <- if(is_resolved) profit else NA
      
      all_games_df <- rbind(all_games_df, game_df)
    }
  }
}

llms <- unique(all_games_df$llm)
leaderboard_data <- lapply(llms, function(current_llm) {
  llm_bets <- all_games_df[all_games_df$llm == current_llm, ]
  profit_vector <- na.omit(llm_bets$profit) # Only resolved
  
  if (length(profit_vector) == 0) {
    current_bankroll <- INITIAL_BANKROLL
    roi <- 0
    lower_bound <- INITIAL_BANKROLL
    upper_bound <- INITIAL_BANKROLL
  } else {
    bootstrapped_profits <- replicate(BOOTSTRAP_REPS, { sum(sample(profit_vector, length(profit_vector), replace = TRUE)) })
    bootstrapped_bankrolls <- INITIAL_BANKROLL + bootstrapped_profits
    lower_bound <- quantile(bootstrapped_bankrolls, (1 - CONFIDENCE_LEVEL) / 2, na.rm = TRUE)
    upper_bound <- quantile(bootstrapped_bankrolls, 1 - (1 - CONFIDENCE_LEVEL) / 2, na.rm = TRUE)
    
    resolved_bets <- llm_bets[!is.na(llm_bets$profit), ]
    total_staked_resolved <- sum(as.numeric(resolved_bets$stake))
    total_profit_resolved <- sum(as.numeric(resolved_bets$profit))
    roi <- ifelse(total_staked_resolved > 0, (total_profit_resolved / total_staked_resolved) * 100, 0)
    current_bankroll <- INITIAL_BANKROLL + total_profit_resolved
  }
  
  list(
    llm = current_llm,
    bankroll = round(current_bankroll, 2),
    roi = round(roi, 2),
    bankroll_ci = paste0("[", max(0, round(lower_bound)), "€ - ", round(upper_bound), "€]")
  )
})

write(toJSON(leaderboard_data, pretty = TRUE, auto_unbox = TRUE), "leaderboard_data.json")

# ==========================================
# PART 2: Generate Chart Data (Time Series)
# ==========================================

# Structure: { labels: ["Start", "Round 1"...], series: { "GPT 5": [1000, 1020...], ... } }
chart_labels <- c("Start")
chart_series <- list()

# Initialize all models with starting bankroll
for (model in llms) {
  chart_series[[model]] <- c(INITIAL_BANKROLL)
}

# Track running bankrolls
current_bankrolls <- setNames(rep(INITIAL_BANKROLL, length(llms)), llms)

# Iterate strictly by round order
for (round_data in data$rounds) {
  chart_labels <- c(chart_labels, paste("Round", round_data$round))
  
  # Track profit for this specific round per model
  round_profits <- setNames(rep(0, length(llms)), llms)
  
  if (length(round_data$games) > 0) {
    for (game in round_data$games) {
      mapped_name <- apply_llm_mapping(game$llm)
      
      # Only process if model exists in our list (safety)
      if (mapped_name %in% names(round_profits)) {
        p <- determine_profit(game$match, game$prediction, game$stake, game$odd, all_results)
        round_profits[mapped_name] <- round_profits[mapped_name] + p
      }
    }
  }
  
  # Update totals and append to history
  for (model in names(chart_series)) {
    current_bankrolls[model] <- current_bankrolls[model] + round_profits[model]
    # Append formatted value
    chart_series[[model]] <- c(chart_series[[model]], round(current_bankrolls[model], 2))
  }
}

# Construct final object
chart_json <- list(
  labels = chart_labels,
  series = chart_series
)

write(toJSON(chart_json, pretty = TRUE, auto_unbox = TRUE), "chart_data.json")

cat("✅ Successfully generated leaderboard_data.json and chart_data.json.\n")
