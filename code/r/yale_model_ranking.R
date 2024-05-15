library(XML)
library(RCurl)

months <- c("october", "november", "december", "january", "february", "march", "april")

season <- data.frame()  # Initialize the season dataframe
for(i in 1:length(months)){
  u <- paste0("https://www.basketball-reference.com/leagues/NBA_2018_games-", months[i], ".html")
  newu <- getURL(u)
  data <- readHTMLTable(newu, which = 1)
  
  # Standardize column names in case they are not uniform across months
  colnames(data) <- c("Date", "Start (ET)", "Visitor/Neutral", "PTS", "Home/Neutral", "PTSH", "Attend.", "Notes")
  
  data$PTS <- as.numeric(as.character(data$PTS))
  data$PTSH <- as.numeric(as.character(data$PTSH))
  
  # Bind rows
  season <- rbind(season, data)
}

# Calculate the point differential and append it
season$point_diff <- with(season, PTSH - PTS)

# Aggregate point differentials by team
visitor_stats <- aggregate(point_diff ~ `Visitor/Neutral`, data=season, FUN=mean)
home_stats <- aggregate(point_diff ~ `Home/Neutral`, data=season, FUN=mean)
colnames(visitor_stats) <- c("Team", "AvgPointDiffVisitor")
colnames(home_stats) <- c("Team", "AvgPointDiffHome")

# Convert visitor point diff to positive for the model
visitor_stats$AvgPointDiffVisitor <- -visitor_stats$AvgPointDiffVisitor

# Merge and calculate average point differential
all_stats <- merge(visitor_stats, home_stats, by="Team", all=TRUE)
all_stats$AvgPointDiff <- rowMeans(subset(all_stats, select=c("AvgPointDiffVisitor", "AvgPointDiffHome")), na.rm=TRUE)

# Logistic regression model for win probability
model <- glm(I(PTSH > PTS) ~ point_diff, family=binomial(), data=season)
all_stats$Rating <- all_stats$AvgPointDiff * coef(model)["point_diff"]

# Create rankings based on model outputs
max_rating <- max(all_stats$Rating, na.rm=TRUE)
min_rating <- min(all_stats$Rating, na.rm=TRUE)
all_stats$yusag_coeff <- (all_stats$Rating - min_rating) / (max_rating - min_rating) * 16 - 8

# Export rankings
write.csv(all_stats, "NBA_Rankings.csv", row.names = FALSE)

# Print top rows of the all_stats dataframe
head(all_stats)