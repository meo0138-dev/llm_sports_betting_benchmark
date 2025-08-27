# create_results_template.R

# This script generates a JSON template for you to fill in match results.
# It reads your predictions.json, finds all unique matches that were bet on
# in the MOST RECENT round, and creates a new file called results_template.json.

library(jsonlite)

# --- Configuration ---
INPUT_FILE <- "predictions.json"
OUTPUT_FILE <- "results_template.json"

# --- Main Script ---

# Check if the input file exists to prevent errors
if (!file.exists(INPUT_FILE)) {
  stop(paste("Error: Input file not found. Make sure", INPUT_FILE, "is in the same directory."))
}

cat("Reading", INPUT_FILE, "to find all unique matches for the last round...\n")

# --- THE FIX: Load data as a nested list, preventing automatic simplification ---
data <- fromJSON(INPUT_FILE, simplifyDataFrame = FALSE)
# --- END OF FIX ---

# Find the current/last round (the last one in the list)
# Check if the 'rounds' array exists and is not empty
if (is.null(data$rounds) || length(data$rounds) == 0) {
  stop("Error: The 'predictions.json' file does not contain a valid 'rounds' array.")
}

current_round_index <- length(data$rounds)
current_round_data <- data$rounds[[current_round_index]]

# Handle case where the last round might not have any games yet
if (is.null(current_round_data$games) || length(current_round_data$games) == 0) {
  cat("The last round (Round", current_round_data$round, ") has no bets. No template will be generated.\n")
  # Use q() for quitting in a non-interactive script
  q(save = "no")
}

# Extract the match names from the list of games in the current round
# The `sapply` function iterates through each game (which is a list) and pulls out the 'match' element.
match_names <- sapply(current_round_data$games, function(game) game$match)

# Get a vector of all unique match names ONLY from the current round
unique_matches <- unique(match_names)

# Create a named list structure which will be converted to the JSON object.
# The structure is: "Match Name" = list(score = "")
results_template <- lapply(unique_matches, function(match) {
  return(list(score = ""))
})
names(results_template) <- unique_matches

# Convert the R list to a pretty JSON string
json_output <- toJSON(results_template, pretty = TRUE, auto_unbox = TRUE)

# Write the JSON string to the output file
write(json_output, OUTPUT_FILE)

cat("✅ Successfully created", OUTPUT_FILE, "for Round", current_round_data$round, "with", length(unique_matches), "matches ready for you to update.\n")


