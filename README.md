Of course. Here is a well-structured `README.md` file for your GitHub repository based on the rules you provided.

---

# 🎯 LLM Sports Betting Benchmark 2025

This repository hosts the official LLM Sports Betting Benchmark, a competition designed to evaluate the ability of Large Language Models (LLMs) and other AI systems to function as profitable sports bettors over a simulated football season.

The primary goal of this benchmark is to assess how well different models can analyze sports match data, interpret odds, and apply effective bankroll management strategies to maximize returns in a realistic betting environment.

## 📜 Rules of the Competition

### 1. Objective
The primary objective is for each AI system to act as a sports bettor, competing to finish a full football season with the largest possible bankroll. The benchmark simulates realistic betting scenarios, requiring participants to choose which matches to bet on, determine stake amounts, and manage risk over an extended period.

### 2. Competition Setup
*   **Leagues Covered**: The competition includes major European football leagues and tournaments:
    *   Serie A
    *   Premier League
    *   La Liga
    *   Bundesliga
    *   Ligue 1
    *   Champions League
    *   Europa League
    *   Conference League
*   **Season Length**: The benchmark will run for the duration of a full football season (approximately one year of play).
*   **Initial Bankroll**: Each AI participant begins with a starting bankroll of **€1000**.
*   **Odds Format**: All odds are provided in the **Decimal (European)** format. The available betting markets include:
    *   **Match Outcome**: Home Win (`1`), Draw (`X`), Away Win (`2`).
    *   **Double Chance**: Home or Draw (`1X`), Home or Away (`12`), Draw or Away (`X2`).
    *   **Totals**: Over/Under a specific goal line (e.g., Over 2.5, Under 1.5).
    *   **Both Teams to Score**: Yes (`GG`) or No (`NG`).
    *   **Combo Bets**: Match outcome combined with a GG/NG condition (e.g., `1+GG`, `X+NG`).

### 3. AI Input & Output
For each round of matches, the AI will receive and must produce the following:

*   **Input (Weekly)**: A list of upcoming matches, including the participating teams and the decimal odds for all available markets.
*   **Output (for each chosen match)**:
    1.  **Bet Decision**: A clear choice of whether to place a bet on a match or to skip it.
    2.  **Outcome Chosen**: The specific market and outcome to bet on (e.g., `1`, `X`, `Over 2.5`, `GG`).
    3.  **Stake Amount**: The amount in Euros (€) to wager. This amount cannot exceed the AI's current bankroll.
    4.  **Reasoning**: A brief, human-readable justification explaining why the model chose that specific bet.
    5.  **Probability (Optional but Recommended)**: The AI's estimated probability for the chosen outcome. This is not used for scoring but is encouraged for deeper analytical insights.

### 4. Betting Rules
*   All bets for a given round must be placed before the match results are known.
*   AIs are free to place any number of bets per week.
*   An AI may stake any portion of its current bankroll on a single bet, up to and including the entire amount ("all-in").
*   The bankroll is updated automatically after each round of matches based on the bet outcomes.
*   If an AI's bankroll drops to **€0 or less**, it is considered **bankrupt** and is eliminated from placing further bets in the competition.

### 5. Scoring and Evaluation
*   **Primary Metric**: The final leaderboard ranking is determined solely by the **final bankroll amount** at the end of the season. The AI with the highest bankroll wins.
*   **Secondary Metrics (For Analysis, NOT Ranking)**:
    *   **Return on Investment (ROI)**: `(Total Profit / Total Staked Amount) * 100`
    *   **Bankruptcy Status**: A binary indicator (Yes/No).
    *   **Volatility of Returns**: The standard deviation of weekly profit/loss.
    *   **Maximum Drawdown**: The largest percentage drop from a peak bankroll value to a subsequent low.
    *   **Probability Calibration**: An analysis of how well the AI's stated probabilities align with actual outcomes (if provided).

### 6. Strategy Notes
AI participants are encouraged to develop sophisticated strategies. A balance must be struck between:
*   **Aggressive strategies**: High-stakes betting that can lead to rapid bankroll growth but carries a significant risk of bankruptcy.
*   **Conservative strategies**: Lower-stakes betting that aims for steady, incremental growth with a lower risk of elimination.

While "all-in" bets are permitted, they represent a high-risk, high-reward approach that could lead to early elimination. A successful AI will demonstrate intelligent risk management over the long term.

### 7. Example Round
Here is a simple walkthrough of a single bet:

1.  **Input**:
    *   Match: Inter vs. Milan
    *   Odds: `1` = 2.20, `X` = 3.40, `2` = 3.10

2.  **AI Output**:
    *   **Bet Decision**: Place a bet.
    *   **Outcome Chosen**: `1` (Inter to win).
    *   **Stake Amount**: €10.
    *   **Reasoning**: "Inter has strong home form and their key striker is not injured."

3.  **Result**:
    *   If Inter wins the match, the bet is won.
    *   Winnings = `Stake` × `Odds` = €10 × 2.20 = €22.
    *   Net Profit = `Winnings` - `Stake` = €22 - €10 = €12.
    *   The AI's bankroll increases by €12.

### 8. Leaderboard
At the conclusion of the season, the AI with the highest final bankroll will be declared the **Betting AI Benchmark 2025 Champion**.

Secondary leaderboards will also be published to highlight performance across other key metrics like ROI, survival rate (non-bankrupt AIs), and risk-adjusted returns. This format is designed to reward intelligent, sustainable betting strategies and penalize reckless gambling.
