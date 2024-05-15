# Load the rankings data from the CSV file
rankings <- read.csv("NBA_Rankings.csv")

# Number of simulations
num_simulations <- 30000 

# Create hypothetical matchups
matchups <- expand.grid(home_team = rankings$Team, away_team = rankings$Team)
matchups <- subset(matchups, home_team != away_team)

# Attach rankings to the matchups data frame
matchups <- merge(matchups, rankings, by.x = "home_team", by.y = "Team", all.x = TRUE)
matchups <- merge(matchups, rankings, by.x = "away_team", by.y = "Team", all.x = TRUE, suffixes = c(".home", ".away"))

# Prepare to store simulation results
total_wins_results <- vector("list", num_simulations)

# Simulate multiple seasons
set.seed(123)
start_time <- Sys.time()
for (sim in 1:num_simulations) {
    matchups$home_win_probability <- 1 / (1 + exp(-(matchups$Rating.home - matchups$Rating.away)))
    matchups$home_win <- rbinom(nrow(matchups), 1, matchups$home_win_probability)

    # Calculate the number of wins for each team
    home_wins <- aggregate(home_win ~ home_team, data = matchups, FUN = sum)
    away_losses <- aggregate(home_win ~ away_team, data = matchups, FUN = function(x) sum(1-x))

    # Combine home wins and away losses to get total wins
    total_wins <- merge(home_wins, away_losses, by.x = "home_team", by.y = "away_team")
    names(total_wins) <- c("Team", "HomeWins", "AwayWins")
    total_wins$TotalWins <- total_wins$HomeWins + total_wins$AwayWins

    # Store results from this simulation
    total_wins_results[[sim]] <- total_wins
}
end_time <- Sys.time()

print(total_wins_results[[2]])

time_difference <- end_time - start_time
seconds <- as.numeric(time_difference, units = "secs")
print(paste("Time taken: ", seconds, "seconds"))