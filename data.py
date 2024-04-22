from nba_api.stats.static import teams
from nba_api.stats.endpoints import leaguedashteamstats
import pandas as pd

# get_teams returns a list of 30 dictionaries, each an NBA team
nba_teams = teams.get_teams()

# fetch team statistics for the 2023-2024 season
team_stats = leaguedashteamstats.LeagueDashTeamStats(season='2023-24', per_mode_detailed='PerGame')
df_team_stats = team_stats.get_data_frames()[0]

print(df_team_stats.head())

# save the data to a csv file
df_team_stats.to_csv('team_stats.csv', index=False)