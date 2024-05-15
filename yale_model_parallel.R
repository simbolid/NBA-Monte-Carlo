library(parallel)


num_cores <- detectCores() - 1  # Leave one core free for system tasks
cl <- makeCluster(num_cores)

# Load rankings and define conference information
rankings <- read.csv("NBA_Rankings.csv")
rankings$Conference <- ifelse(rankings$Team %in% c("List", "Of", "Eastern", "Conference", "Teams"), "East", "West")

# Create hypothetical matchups
matchups <- expand.grid(home_team = rankings$Team, away_team = rankings$Team)
matchups <- subset(matchups, home_team != away_team)
matchups <- merge(matchups, rankings, by.x = "home_team", by.y = "Team", all.x = TRUE)
matchups <- merge(matchups, rankings, by.x = "away_team", by.y = "Team", all.x = TRUE, suffixes = c(".home", ".away"))

# Export necessary data to the cluster
clusterExport(cl, varlist = c("matchups", "rankings"), envir = environment())


simulate_one_season <- function() {
    matchups$home_win_probability <- 1 / (1 + exp(-(matchups$Rating.home - matchups$Rating.away)))
    matchups$home_win <- rbinom(nrow(matchups), 1, matchups$home_win_probability)
  
    home_wins <- aggregate(home_win ~ home_team, data = matchups, FUN = sum)
    away_losses <- aggregate(home_win ~ away_team, data = matchups, FUN = function(x) sum(1 - x))
  
    total_wins <- merge(home_wins, away_losses, by.x = "home_team", by.y = "away_team")
    names(total_wins) <- c("Team", "HomeWins", "AwayWins")
    total_wins$TotalWins <- total_wins$HomeWins + total_wins$AwayWins
  
    return(total_wins)
}
clusterExport(cl, "simulate_one_season")

num_simulations <- 30000
start_time <- Sys.time()
simulation_results <- parLapply(cl, seq_len(num_simulations), function(x) simulate_one_season())

# Aggregate simulation results to get mean wins for each team
mean_wins <- do.call(rbind, lapply(simulation_results, function(x) {
    aggregate(TotalWins ~ Team, data = x, FUN = mean)
}))

# Merge mean wins data with conference information
results_with_conf <- merge(mean_wins, rankings, by = "Team")

# Select top 8 teams from each conference
eastern_playoffs <- results_with_conf[results_with_conf$Conference == "East",]
western_playoffs <- results_with_conf[results_with_conf$Conference == "West",]

top_east <- head(eastern_playoffs[order(-eastern_playoffs$TotalWins),], 8)
top_west <- head(western_playoffs[order(-western_playoffs$TotalWins),], 8)

end_time <- Sys.time()

# Print playoff teams
print("Eastern Conference Playoff Teams:")
print(top_east$Team)
print("Western Conference Playoff Teams:")
print(top_west$Team)

time_difference <- end_time - start_time
seconds <- as.numeric(time_difference, units = "secs")
print(paste("Time taken: ", seconds, "seconds"))