library(XML)
library(RCurl)


#Generating the data
months <- c("october", "november", "december", "january", "february", "march", "april")

for(i in 1:length(months)){
  u <- paste0("https://www.basketball-reference.com/leagues/NBA_2018_games-",months[i],".html")
  newu <- getURL(u)
  data <- readHTMLTable(newu)
  
  if(i==1){
    season <- data$schedule
    season$`Visitor/Neutral` <- as.character(season$`Visitor/Neutral`)
    season$`Home/Neutral` <- as.character(season$`Home/Neutral`)
    season$PTSV <- as.numeric(as.character(season[[4]]))
    season$PTSH <- as.numeric(as.character(season[[6]]))
  }
  
  else{
    season1 <- data$schedule
    season1$`Visitor/Neutral` <- as.character(season1$`Visitor/Neutral`)
    season1$`Home/Neutral` <- as.character(season1$`Home/Neutral`)
    season1$PTSV <- as.numeric(as.character(season1[[4]]))
    season1$PTSH <- as.numeric(as.character(season1[[6]]))
    season <- rbind(season, season1)
    
  }
  if(i != 1 && i != length(months)){
    remove(season1)
  }
  
}
head(season)


# calculate ranking mathmatical model
# Calculate the point differential for each game
season$point_diff <- with(season, PTSH - PTSV)

if (!"point_diff" %in% names(season)) {
  stop("point_diff is not in the season dataframe")
}

# Aggregate point differentials by team
team_stats <- aggregate(point_diff ~ `Visitor/Neutral`, data=season, FUN=mean)
colnames(team_stats) <- c("Team", "AvgPointDiffVisitor")

home_stats <- aggregate(point_diff ~ `Home/Neutral`, data=season, FUN=mean)
colnames(home_stats) <- c("Team", "AvgPointDiffHome")

# Combine the stats
team_stats$AvgPointDiffVisitor <- -team_stats$AvgPointDiffVisitor  # Adjust visitor point diff to be positive for the model
all_stats <- merge(team_stats, home_stats, by="Team", all=TRUE)
all_stats$AvgPointDiff <- (na.omit(all_stats$AvgPointDiffVisitor) + na.omit(all_stats$AvgPointDiffHome)) / 2

# Create a logistic regression model for win probability based on point differential
model <- glm(I(PTSH > PTSV) ~ point_diff, family=binomial(), data=season)
summary(model)

# Use the model coefficient to adjust the rankings
all_stats$Rating <- all_stats$AvgPointDiff * coef(model)["point_diff"]


# Create a linear regression model for point differential based on rankings
rankings <- all_stats[, c("Team", "Rating")]

# merge season and all_stats
season_all_stats <- merge(season, all_stats, by.x="Visitor/Neutral", by.y="Team", all.x=TRUE)

if (!"point_diff" %in% names(season)) {
  stop("point_diff is not in the season dataframe")
}

lm.NBAhoops <- lm(point_diff ~ Rating, data=season_all_stats)

# Create a logistic regression model for win probability based on point differential
glm.pointspread <- glm(I(PTSH > PTSV) ~ point_diff, family=binomial(), data=season)

ptdif_call <- function(home, away, HNA) {
    r1 <- rankings$Rating[rankings$Team == home]
    r2 <- rankings$Rating[rankings$Team == away]

    # Debugging output
    cat("Rating for home team (", home, "): ", r1, "\n")
    cat("Rating for away team (", away, "): ", r2, "\n")

    # Check for NA values
    if (is.na(r1) || is.na(r2)) {
        cat("Error: Rating missing for teams - Home:", home, "Away:", away, "\n")
        return(c(NA, NA))  # Return NA values to handle missing ratings gracefully
    }

    
    # Home advantage adjustment
    home_advantage <- ifelse(HNA == "H", 3, ifelse(HNA == "A", -3, 0))  # Example home advantage
    
    pt_dif <- r1 - r2 + home_advantage
    
    prob <- 1 / (1 + exp(-pt_dif))
    
    return(c(pt_dif, prob))
}


inter <- data.frame(season$`Visitor/Neutral`, season$PTSV, season$`Home/Neutral`, season$PTSH)
inter$season..Visitor.Neutral. <- as.character(inter$season..Visitor.Neutral.)
inter$season..Home.Neutral. <- as.character(inter$season..Home.Neutral.)


inter$pt_dif <- inter$season.PTSH - inter$season.PTSV
head(inter)


team <- character(length = 2*length(inter$pt_dif))
opponent <- character(length = 2*length(inter$pt_dif))
location <- character(length = 2*length(inter$pt_dif))
ptdif <- vector(mode='numeric',length = 2*length(inter$pt_dif))


cleanse <- data.frame(team,opponent,location,ptdif)

cleanse$team <- as.character(cleanse$team)
cleanse$opponent <- as.character(cleanse$opponent)
cleanse$location <- as.character(cleanse$location)
cleanse$date <- character(length = 2*length(inter$pt_dif))

for(i in 1:length(inter$pt_dif)){
  
  #Here, we double count games so that each game has an entry for the home team being "team" and the away team being "team"
  cleanse$date[i] <- as.character(season$Date[i])
  cleanse$team[i] <- inter$season..Home.Neutral.[i]
  cleanse$opponent[i] <- inter$season..Visitor.Neutral.[i]
  cleanse$location[i] <- "H"
  cleanse$ptdif[i] <- inter$pt_dif[i]
  
  
  cleanse$date[i + length(inter$pt_dif)] <- as.character(season$Date[i]) 
  cleanse$team[i + length(inter$pt_dif)] <- inter$season..Visitor.Neutral.[i]
  cleanse$opponent[i + length(inter$pt_dif)] <- inter$season..Home.Neutral.[i]
  cleanse$location[i + length(inter$pt_dif)] <- "A"
  cleanse$ptdif[i + length(inter$pt_dif)] <- (-1)*inter$pt_dif[i]
  
}
head(cleanse)


cleanse$location[363] <- "N"
cleanse$location[376] <- "N"
cleanse$location[615] <- "N"
cleanse$location[363 + length(inter$pt_dif)] <- "N"
cleanse$location[376 + length(inter$pt_dif)] <- "N"
cleanse$location[615+ length(inter$pt_dif)] <- "N"

cleanse$win <- rep(NA, length(cleanse$date))

for(i in 1:length(inter$pt_dif)){
 
  if(!is.na(cleanse$ptdif[i])){
    
    if(cleanse$ptdif[i] > 0){
      cleanse$win[i] <- 1
    }
    else if(cleanse$ptdif[i] < 0){
      cleanse$win[i] <- 0
    }
    
    if(cleanse$ptdif[i + length(inter$pt_dif)] > 0){
      cleanse$win[i + length(inter$pt_dif)] <- 1
    }
    else if(cleanse$ptdif[i + length(inter$pt_dif)] < 0){
      cleanse$win[i + length(inter$pt_dif)] <- 0
    }  
  }
  
  
}
cleanse <- cleanse[c(5,1,2,3,4,6)]
head(cleanse)


# simulated score differential 
cleanse$sim_ptdif <- cleanse$ptdif
# simulated win probability
cleanse$win_prob <- cleanse$win


#predicting using linear models. 
ptdif_call <- function(home,away,HNA){
  
  arr <- c(0,0)
  
  r1 <- rankings$yusag_coeff[which(rankings$team == home)]
  r2 <- rankings$yusag_coeff[which(rankings$team == away)]
  
  
  if(HNA == "H"){
    pt_dif <- r1 - r2 - coefficients(lm.NBAhoops)[[1]]
  }
  
  if(HNA == "N"){
    pt_dif <- r1 - r2
  }
  
  if(HNA == "A"){
    pt_dif <- r1 - r2 + coefficients(lm.NBAhoops)[[1]]
  }
  
  arr[1] <- pt_dif
  
  prob <- 1 / (1+ exp(- coefficients(glm.pointspread)[[2]] * pt_dif))
  arr[2] <- prob
  
  return(arr)
}

# fill out empty entries for upcoming games
for(i in 1:nrow(cleanse)){
  if(is.na(cleanse$ptdif[i])){
    arr <- ptdif_call(cleanse$team[i],cleanse$opponent[i],cleanse$location[i])
    cleanse$sim_ptdif[i] <- arr[1]
    cleanse$win_prob[i] <- arr[2]
  }
}

# 
west <- c("Golden State Warriors", "Dallas Mavericks", "Memphis Grizzlies", "Phoenix Suns", "Sacramento Kings", "San Antonio Spurs", "Utah Jazz", "Los Angeles Lakers","Oklahoma City Thunder", "Minnesota Timberwolves", "New Orleans Pelicans", "Denver Nuggets", "Houston Rockets", "Los Angeles Clippers", "Portland Trail Blazers")
east <- setdiff(rankings$team, west) #throws error

wins_current <- rep(NA, 15)
wins_projected <- rep(NA, 15)
playoff_prob <- rep(NA, 15)
top_four_prob <- rep(NA, 15)
top_two_prob <- rep(NA, 15)
top_seed_prob <- rep(NA,15)

west_data <- data.frame(west,wins_current,wins_projected,playoff_prob, top_four_prob, top_two_prob, top_seed_prob, stringsAsFactors = FALSE)
east_data <- data.frame(east,wins_current,wins_projected,playoff_prob, top_four_prob, top_two_prob, top_seed_prob, stringsAsFactors = FALSE)
#This will all be filled in eventually
west_data

# new 
west_data$yusag_coeff <- rep(NA, 15)
east_data$yusag_coeff <- rep(NA, 15)

rankings$team <- as.character(rankings$team)

for(i in 1:15){
  
  west_data$wins_current[i] <- sum(cleanse$win[cleanse$team == west_data$west[i]], na.rm = TRUE)
  east_data$wins_current[i] <- sum(cleanse$win[cleanse$team == east_data$east[i]], na.rm = TRUE)
  
  west_data$wins_projected[i] <- sum(cleanse$win_prob[cleanse$team == west_data$west[i]], na.rm = TRUE)
  east_data$wins_projected[i] <- sum(cleanse$win_prob[cleanse$team == east_data$east[i]], na.rm = TRUE)
  
  west_data$yusag_coeff[i] <- rankings$yusag_coeff[which(rankings$team == west_data$west[i])]
  east_data$yusag_coeff[i] <- rankings$yusag_coeff[which(rankings$team == east_data$east[i])]
}

east_data <- east_data[c(1,2,3,8,4,5,6,7)]
west_data <- west_data[c(1,2,3,8,4,5,6,7)]

experimental_east <- data.frame(east, stringsAsFactors = FALSE)
experimental_west <- data.frame(west, stringsAsFactors = FALSE)
experimental_east$sample_wins <- rep(NA, 15)
experimental_west$sample_wins <- rep(NA, 15)
experimental_east$playoff_counts <- rep(0, 15)
experimental_west$playoff_counts <- rep(0, 15)
experimental_east$four_counts <- rep(0, 15)
experimental_west$four_counts <- rep(0, 15)
experimental_east$two_counts <- rep(0, 15)
experimental_west$two_counts <- rep(0, 15)
experimental_east$one_counts <- rep(0, 15)
experimental_west$one_counts <- rep(0, 15)

N <- 15000
cleanse$sim_win <- cleanse$win
min_game <- min(which(is.na(cleanse$sim_win)))

for(j in 1:N){
  #To simulate a season, we generate 1230 random numbers between 1 and 0, one for each game of the season
  play_games <- runif(1230)
  for(i in (min_game):1230){
    
    #For a given game, the winner is decided by wheather or not the random number generated is less than the win probability
    if(play_games[i] < cleanse$win_prob[i]){
      cleanse$sim_win[i] <- 1
      cleanse$sim_win[i + 1230] <- 0
    }
    else{
      cleanse$sim_win[i] <- 0
      cleanse$sim_win[i + 1230] <- 1
    }
    
  }
  for(k in 1:15){
     experimental_east$sample_wins[k] <- sum(cleanse$sim_win[cleanse$team == experimental_east$east[k]])
    experimental_west$sample_wins[k] <- sum(cleanse$sim_win[cleanse$team == experimental_west$west[k]])    
  }

  #The way we collect data is by sorting by wins in a simulated season, then adding a count to the team that finishes either 1, top 2, top 4, or top 8 in each conference
  experimental_east <- experimental_east[order(experimental_east$sample_wins,decreasing = TRUE),]
  experimental_west <- experimental_west[order(experimental_west$sample_wins,decreasing = TRUE),]
  
  experimental_east$playoff_counts[1:8] <- experimental_east$playoff_counts[1:8] + 1
  experimental_west$playoff_counts[1:8] <- experimental_west$playoff_counts[1:8] + 1
  
  experimental_east$four_counts[1:4] <- experimental_east$four_counts[1:4] + 1
  experimental_west$four_counts[1:4] <- experimental_west$four_counts[1:4] + 1
  
  experimental_east$two_counts[1:2] <- experimental_east$two_counts[1:2] + 1
  experimental_west$two_counts[1:2] <- experimental_west$two_counts[1:2] + 1
  
  experimental_east$one_counts[1] <- experimental_east$one_counts[1] + 1
  experimental_west$one_counts[1] <- experimental_west$one_counts[1] + 1

  if(j %% 1000 == 0){
    print(j)
  }
}

for(i in 1:15){
  west_data$playoff_prob[i] <- experimental_west$playoff_counts[which(experimental_west$west == west_data$west[i])] / N
  east_data$playoff_prob[i] <- experimental_east$playoff_counts[which(experimental_east$east == east_data$east[i])] / N
  
  west_data$top_four_prob[i] <- experimental_west$four_counts[which(experimental_west$west == west_data$west[i])] / N
  east_data$top_four_prob[i] <- experimental_east$four_counts[which(experimental_east$east == east_data$east[i])] / N
  
  west_data$top_two_prob[i] <- experimental_west$two_counts[which(experimental_west$west == west_data$west[i])] / N
  east_data$top_two_prob[i] <- experimental_east$two_counts[which(experimental_east$east == east_data$east[i])] / N
  
  west_data$top_seed_prob[i] <- experimental_west$one_counts[which(experimental_west$west == west_data$west[i])] / N
  east_data$top_seed_prob[i] <- experimental_east$one_counts[which(experimental_east$east == east_data$east[i])] / N
}

west_data <- west_data[order(west_data$wins_projected, decreasing = TRUE),]
east_data <- east_data[order(east_data$wins_projected, decreasing = TRUE),]
colnames(west_data)[1] <- "team"
colnames(east_data)[1] <- "team"

west_data$playoff_prob <- round(west_data$playoff_prob, digits = 3)
east_data$playoff_prob <- round(east_data$playoff_prob, digits = 3)

west_data$top_seed_prob <- round(west_data$top_seed_prob, digits = 3)
east_data$top_seed_prob <- round(east_data$top_seed_prob, digits = 3)

west_data$top_two_prob <- round(west_data$top_two_prob, digits = 3)
east_data$top_two_prob <- round(east_data$top_two_prob, digits = 3)

west_data$top_four_prob <- round(west_data$top_four_prob, digits = 3)
east_data$top_four_prob <- round(east_data$top_four_prob, digits = 3)

west_data$wins_projected <- round(west_data$wins_projected, digits = 1)
east_data$wins_projected <- round(east_data$wins_projected, digits = 1)

west_data$yusag_coeff <- round(west_data$yusag_coeff, digits = 2)
east_data$yusag_coeff <- round(east_data$yusag_coeff, digits = 2)

write.csv(west_data, "WEST_RANKINGS.csv", row.names = FALSE)
write.csv(east_data, "EAST_RANKINGS.csv", row.names = FALSE)

#results
east_data

west_data