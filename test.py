from nba_api.stats.endpoints import leaguegamefinder

season_data = leaguegamefinder.LeagueGameFinder(
    season_nullable='2023-24', 
)

season_df = season_data.get_data_frames()[0]
print(season_df.head())
