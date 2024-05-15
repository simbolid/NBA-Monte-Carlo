library(parallel)

# Define constants
initial_rating <- 1500
K <- 20
home_advantage <- 100
num_simulations <- 10000

# Function to calculate expected score
expected_score <- function(ra, rb) {
  1 / (1 + 10 ^ ((rb - ra) / 400))
}

# Function to update Elo ratings
update_elo <- function(ra, rb, score_a, score_b, home_field = FALSE) {
  Ea <- expected_score(ra + ifelse(home_field, home_advantage, 0), rb)
  S <- ifelse(score_a > score_b, 1, ifelse(score_a < score_b, 0, 0.5))
  
  ra <- ra + K * (S - Ea)
  rb <- rb - K * (S - Ea)
  
  return(c(ra, rb))
}

# Function to simulate a season and update ratings
simulate_season <- function(teams, ratings) {
  for (i in seq_along(teams)) {
    for (j in seq_along(teams)) {
      if (i != j) {
        home_team <- teams[i]
        away_team <- teams[j]
        home_score <- sample(80:120, 1)
        away_score <- sample(80:120, 1)
        
        new_ratings <- update_elo(ratings[home_team], ratings[away_team], home_score, away_score, TRUE)
        ratings[home_team] <- new_ratings[1]
        ratings[away_team] <- new_ratings[2]
      }
    }
  }
  return(ratings)
}

# List of all NBA teams
teams <- c("Atlanta Hawks", "Boston Celtics", "Brooklyn Nets", "Charlotte Hornets",
           "Chicago Bulls", "Cleveland Cavaliers", "Dallas Mavericks", "Denver Nuggets",
           "Detroit Pistons", "Golden State Warriors", "Houston Rockets", "Indiana Pacers",
           "LA Clippers", "Los Angeles Lakers", "Memphis Grizzlies", "Miami Heat",
           "Milwaukee Bucks", "Minnesota Timberwolves", "New Orleans Pelicans", "New York Knicks",
           "Oklahoma City Thunder", "Orlando Magic", "Philadelphia 76ers", "Phoenix Suns",
           "Portland Trail Blazers", "Sacramento Kings", "San Antonio Spurs", "Toronto Raptors",
           "Utah Jazz", "Washington Wizards")

# Initialize ratings
ratings <- setNames(rep(initial_rating, length(teams)), teams)

# Set up parallel cluster
num_cores <- detectCores() - 1
cl <- makeCluster(num_cores)

# Export necessary objects and functions to the cluster
clusterExport(cl, c("teams", "ratings", "expected_score", "update_elo", 
                    "simulate_season", "K", "home_advantage", "num_simulations"))

# Measure start time
start_time <- Sys.time()

# Run simulations in parallel
parallel_ratings <- parLapply(cl, seq_len(num_simulations), function(x) {
  simulate_season(teams, ratings)
})

# Measure end time
end_time <- Sys.time()

stopCluster(cl)

# Output results from one of the simulations
print("Ratings from one of the simulations:")
print(parallel_ratings[[1]])

time_difference <- end_time - start_time
seconds <- as.numeric(time_difference, units = "secs")
print(paste("Time taken: ", seconds, "seconds"))
