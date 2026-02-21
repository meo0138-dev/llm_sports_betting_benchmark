library(jsonlite)
library(purrr)

# --- Configuration ---
INITIAL_BANKROLL <- 1000
BOOTSTRAP_REPS <- 10000
CONFIDENCE_LEVEL <- 0.95

# --- AI Name Mapping ---
apply_llm_mapping <- function(raw_name) {
  mapping <- list(
    "Claude"="Claude",
    "Claude Opus 4.1" = "Claude",
    "Claude Sonnet 4.5" = "Claude",
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

# --- Prediction Normalizer ---
normalize_prediction <- function(pred) {
  pred <- trimws(pred)
  # Normalize spaces around "+" : "1 + OV" → "1+OV", "2 + UN" → "2+UN"
  pred <- gsub("\\s*\\+\\s*", "+", pred)
  return(pred)
}

# --- Helper Function (Profit Calculation) ---
determine_profit <- function(match_name, prediction, stake, odd, results_list) {
  if (is.null(match_name) || is.null(prediction) || is.null(stake) || is.null(odd)) return(0)
  if (!match_name %in% names(results_list)) return(0)
  
  score_str <- results_list[[match_name]]$score
  if (is.null(score_str) || score_str == "" || !grepl("-", score_str)) return(0)
  
  scores <- as.numeric(strsplit(score_str, "-")[[1]])
  home_goals <- scores[1]; away_goals <- scores[2]; total_goals <- home_goals + away_goals
  
  home_win <- home_goals > away_goals
  draw <- home_goals == away_goals
  away_win <- away_goals > home_goals
  is_gg <- home_goals > 0 && away_goals > 0
  is_over_2_5 <- total_goals > 2.5
  is_under_2_5 <- total_goals < 2.5
  
  is_win <- FALSE
  
  # Normalize the prediction string
  prediction <- normalize_prediction(prediction)
  
  # --- 1X2 ---
  if (prediction == "1") { if (home_win) is_win <- TRUE }
  else if (prediction == "X") { if (draw) is_win <- TRUE }
  else if (prediction == "2") { if (away_win) is_win <- TRUE }
  
  # --- Double Chance ---
  else if (prediction == "1X") { if (home_win || draw) is_win <- TRUE }
  else if (prediction == "X2") { if (away_win || draw) is_win <- TRUE }
  else if (prediction == "12") { if (home_win || away_win) is_win <- TRUE }
  
  # --- GG / NG ---
  else if (prediction == "GG") { if (is_gg) is_win <- TRUE }
  else if (prediction == "NG") { if (!is_gg) is_win <- TRUE }
  
  # --- Over / Under (bare, default = 2.5) ---
  else if (prediction == "Over" || prediction == "OV") { if (is_over_2_5) is_win <- TRUE }
  else if (prediction == "Under" || prediction == "UN") { if (is_under_2_5) is_win <- TRUE }
  
  # --- Over X.X / Under X.X (with explicit line) ---
  else if (startsWith(prediction, "Over")) { 
    goal_line <- as.numeric(sub("Over\\s*", "", prediction)) 
    if (!is.na(goal_line) && total_goals > goal_line) is_win <- TRUE 
  }
  else if (startsWith(prediction, "Under")) { 
    goal_line <- as.numeric(sub("Under\\s*", "", prediction)) 
    if (!is.na(goal_line) && total_goals < goal_line) is_win <- TRUE 
  }
  
  # --- 1X2 + GG/NG combos ---
  else if (prediction == "1+GG") { if (home_win && is_gg) is_win <- TRUE }
  else if (prediction == "1+NG") { if (home_win && !is_gg) is_win <- TRUE }
  else if (prediction == "X+GG") { if (draw && is_gg) is_win <- TRUE }
  else if (prediction == "X+NG") { if (draw && !is_gg) is_win <- TRUE }
  else if (prediction == "2+GG") { if (away_win && is_gg) is_win <- TRUE }
  else if (prediction == "2+NG") { if (away_win && !is_gg) is_win <- TRUE }
  
  # --- 1X2 + Over 2.5 combos ---
  else if (prediction == "1+OV") { if (home_win && is_over_2_5) is_win <- TRUE }
  else if (prediction == "X+OV") { if (draw && is_over_2_5) is_win <- TRUE }
  else if (prediction == "2+OV") { if (away_win && is_over_2_5) is_win <- TRUE }
  
  # --- 1X2 + Under 2.5 combos ---
  else if (prediction == "1+UN") { if (home_win && is_under_2_5) is_win <- TRUE }
  else if (prediction == "X+UN") { if (draw && is_under_2_5) is_win <- TRUE }
  else if (prediction == "2+UN") { if (away_win && is_under_2_5) is_win <- TRUE }
  
  # --- Double Chance + Over/Under combos ---
  else if (prediction == "1X+OV") { if ((home_win || draw) && is_over_2_5) is_win <- TRUE }
  else if (prediction == "1X+UN") { if ((home_win || draw) && is_under_2_5) is_win <- TRUE }
  else if (prediction == "X2+OV") { if ((away_win || draw) && is_over_2_5) is_win <- TRUE }
  else if (prediction == "X2+UN") { if ((away_win || draw) && is_under_2_5) is_win <- TRUE }
  else if (prediction == "12+OV") { if ((home_win || away_win) && is_over_2_5) is_win <- TRUE }
  else if (prediction == "12+UN") { if ((home_win || away_win) && is_under_2_5) is_win <- TRUE }
  
  # --- Double Chance + GG/NG combos ---
  else if (prediction == "1X+GG") { if ((home_win || draw) && is_gg) is_win <- TRUE }
  else if (prediction == "1X+NG") { if ((home_win || draw) && !is_gg) is_win <- TRUE }
  else if (prediction == "X2+GG") { if ((away_win || draw) && is_gg) is_win <- TRUE }
  else if (prediction == "X2+NG") { if ((away_win || draw) && !is_gg) is_win <- TRUE }
  else if (prediction == "12+GG") { if ((home_win || away_win) && is_gg) is_win <- TRUE }
  else if (prediction == "12+NG") { if ((home_win || away_win) && !is_gg) is_win <- TRUE }
  
  # --- GG/NG + Over/Under combos ---
  else if (prediction == "GG+OV") { if (is_gg && is_over_2_5) is_win <- TRUE }
  else if (prediction == "GG+UN") { if (is_gg && is_under_2_5) is_win <- TRUE }
  else if (prediction == "NG+OV") { if (!is_gg && is_over_2_5) is_win <- TRUE }
  else if (prediction == "NG+UN") { if (!is_gg && is_under_2_5) is_win <- TRUE }
  
  stake_val <- as.numeric(stake)
  odd_val <- as.numeric(odd)
  
  if (is_win) return(stake_val * (odd_val - 1)) else return(-stake_val)
}

# ==========================================
# PART 1: Generate Leaderboard Data (Bootstrap)
# ==========================================

all_games_df <- data.frame()
all_possible_cols <- c("match", "prediction", "stake", "odd", "llm")

for (round_data in data$rounds) {
  if (length(round_data$games) > 0) {
    for (game in round_data$games) {
      game_df <- as.data.frame(game, stringsAsFactors = FALSE)
      missing_cols <- setdiff(all_possible_cols, names(game_df))
      if (length(missing_cols) > 0) { game_df[, missing_cols] <- NA }
      game_df <- game_df[, all_possible_cols]
      
      game_df$llm <- apply_llm_mapping(game_df$llm)
      
      profit <- determine_profit(game_df$match, game_df$prediction, game_df$stake, game_df$odd, all_results)
      
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
  profit_vector <- na.omit(llm_bets$profit)
  
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

chart_labels <- c("Start")
chart_series <- list()

for (model in llms) {
  chart_series[[model]] <- c(INITIAL_BANKROLL)
}

current_bankrolls <- setNames(rep(INITIAL_BANKROLL, length(llms)), llms)

for (round_data in data$rounds) {
  chart_labels <- c(chart_labels, paste("Round", round_data$round))
  
  round_profits <- setNames(rep(0, length(llms)), llms)
  
  if (length(round_data$games) > 0) {
    for (game in round_data$games) {
      mapped_name <- apply_llm_mapping(game$llm)
      
      if (mapped_name %in% names(round_profits)) {
        p <- determine_profit(game$match, game$prediction, game$stake, game$odd, all_results)
        round_profits[mapped_name] <- round_profits[mapped_name] + p
      }
    }
  }
  
  for (model in names(chart_series)) {
    current_bankrolls[model] <- current_bankrolls[model] + round_profits[model]
    chart_series[[model]] <- c(chart_series[[model]], round(current_bankrolls[model], 2))
  }
}

chart_json <- list(
  labels = chart_labels,
  series = chart_series
)

write(toJSON(chart_json, pretty = TRUE, auto_unbox = TRUE), "chart_data.json")

cat("✅ Successfully generated leaderboard_data.json and chart_data.json.\n")
