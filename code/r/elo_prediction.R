initial_rating <- 1500
K <- 20
home_advantage <- 100

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
        # Simulate game scores
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

# List of NBA teams
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

# Number of simulations (seasons)
num_simulations <- 100

start_time <- Sys.time()

for (s in 1:num_simulations) {
  ratings <- simulate_season(teams, ratings)
}

end_time <- Sys.time()

print("Final Ratings after all seasons:")
print(ratings)


time_difference <- end_time - start_time
seconds <- as.numeric(time_difference, units = "secs")
print(paste("Time taken: ", seconds, "seconds"))