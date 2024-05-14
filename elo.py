from collections import defaultdict
from math import log
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
from nba_api.stats.endpoints import leaguegamefinder
from nba_api.stats.endpoints import leaguestandings


HOME_ADVANTAGE = 100
K = 24  # bias towards recent results


def calculate_elo_change(rating_winner, rating_loser, score_diff, base=400):
    """calculate Elo rating change post game, taking into account margin of victory (MOV)"""
    # formula taken from https://fivethirtyeight.com/features/how-we-calculate-nba-elo-ratings/
    MOV_multiplier = ((abs(score_diff) + 3) ** 0.8) / (((rating_winner - rating_loser) * 0.006) + 7.5)
    expected_win = 1 / (1 + 10 ** ((rating_loser - rating_winner + HOME_ADVANTAGE) / base))
    change_in_elo = K * MOV_multiplier * (1 - expected_win)
    return change_in_elo


def update_ratings(row, ratings, base=400):
    """Update ratings based on the game results"""
    home_team = row['home_team']
    away_team = row['away_team']
    home_score = row['home_score']
    away_score = row['away_score']
    
    home_rating = ratings.get(home_team, 1500)
    away_rating = ratings.get(away_team, 1500)
    
    if home_score > away_score:
        elo_change = calculate_elo_change(home_rating, away_rating, home_score - away_score, base)
        ratings[home_team] = home_rating + elo_change
        ratings[away_team] = away_rating - elo_change
    else:
        # note: home advantage would be negative for an away team
        elo_change = calculate_elo_change(away_rating, home_rating, away_score - home_score, base)
        ratings[away_team] = away_rating + elo_change
        ratings[home_team] = home_rating - elo_change
        
    return ratings


def process_historical_data(games_df):
    """Process past games to get Elo ratings"""
    ratings = {}
    for index, row in games_df.iterrows():
        ratings = update_ratings(row, ratings)
    return ratings


def get_season_data(season, season_type):
    """returns a dataframe containing data on games from the most recent NBA season"""
    season_data = leaguegamefinder.LeagueGameFinder(
        season_nullable=season, 
        season_type_nullable=season_type,
        league_id_nullable='00'  # nba only
    )
    season_df = season_data.get_data_frames()[0]

    home_games_df = season_df[season_df['MATCHUP'].str.contains('vs.')].copy()
    home_games_df.loc[:, 'away_team'] = home_games_df['MATCHUP'].apply(lambda m: m.partition('vs.')[2].strip())
    home_games_df['away_score'] = home_games_df['PTS'] - home_games_df['PLUS_MINUS']
    home_games_df['away_score'] = home_games_df['away_score'].astype(int)

    home_games_df.rename(
        columns={
            'TEAM_ABBREVIATION': 'home_team',
            'GAME_DATE': 'date', 
            'PTS': 'home_score'
        }, 
        inplace=True
    )

    columns_of_interest = ['date', 'home_team', 'home_score', 'away_team', 'away_score']

    final_games_df = home_games_df[columns_of_interest].copy()
    final_games_df = final_games_df.sort_values('date')
    return final_games_df


def get_playoff_teams():
    team_abbreviations = {
        'Celtics': 'BOS',
        'Cavaliers': 'CLE',
        'Mavericks': 'DAL',
        'Nuggets': 'DEN',
        'Pacers': 'IND',
        'Clippers': 'LAC',
        'Lakers': 'LAL',
        'Heat': 'MIA',
        'Bucks': 'MIL',
        'Timberwolves': 'MIN',
        'Pelicans': 'NOP',
        'Knicks': 'NYK',
        'Thunder': 'OKC',
        'Magic': 'ORL',
        '76ers': 'PHI',
        'Suns': 'PHX',
    }

    standings = leaguestandings.LeagueStandings()
    standings_df = standings.get_data_frames()[0]

    eastern_conf_teams = list(standings_df[standings_df['Conference'] == 'East'].iloc[:8]['TeamName'])
    eastern_conf_teams = [team_abbreviations[t] for t in eastern_conf_teams]

    western_conf_teams = list(standings_df[standings_df['Conference'] == 'West'].iloc[:8]['TeamName'])
    western_conf_teams = [team_abbreviations[t] for t in western_conf_teams]

    return eastern_conf_teams, western_conf_teams


def simulate_game(home_team, away_team, ratings):
    home_elo = ratings[home_team]    
    away_elo = ratings[away_team]
    elo_diff = home_elo - away_elo + HOME_ADVANTAGE
    prob_home_wins = 1 / (1 + 10 ** (-elo_diff / 400))
    home_wins = np.random.rand() < prob_home_wins
    return home_wins


def simulate_series(high_seed, low_seed, ratings):
    high_seed_wins = 0
    low_seed_wins = 0
    game = 1

    while high_seed_wins < 4 and low_seed_wins < 4:
        if game < 3 or game == 5 or game == 7:  
            winner = simulate_game(high_seed, low_seed, ratings)
        else:  
            winner = simulate_game(low_seed, high_seed, ratings)

        if winner:
            high_seed_wins += 1
        else:
            low_seed_wins += 1
        
        game += 1
    
    return high_seed if high_seed_wins == 4 else low_seed


def simulate_playoffs(east_teams, west_teams, ratings):
    playoff_progress = {}

    # first round
    winners_east = [simulate_series(east_teams[i], east_teams[-i-1], ratings) for i in range(4)]
    winners_west = [simulate_series(west_teams[i], west_teams[-i-1], ratings) for i in range(4)]

    # conference semi-finals
    for team in winners_east + winners_west:
        playoff_progress[team] = 'Conference Semi-Finals'
    semi_finals_east = [simulate_series(winners_east[i], winners_east[-i-1], ratings) for i in range(2)]
    semi_finals_west = [simulate_series(winners_west[i], winners_west[-i-1], ratings) for i in range(2)]

    # conference finals
    for team in semi_finals_east + semi_finals_west:
        playoff_progress[team] = 'Conference Finals'
    east_champion = simulate_series(semi_finals_east[0], semi_finals_east[1], ratings)
    west_champion = simulate_series(semi_finals_west[0], semi_finals_west[1], ratings)

    # NBA finals
    playoff_progress[east_champion] = 'Finals'
    playoff_progress[west_champion] = 'Finals'
    champion = simulate_series(east_champion, west_champion, ratings)

    playoff_progress[champion] = 'Champion'
    return playoff_progress


def get_playoff_odds(num_simulations, east_teams, west_teams, ratings):
    stage_counts = defaultdict(lambda: defaultdict(int))
    
    for _ in range(num_simulations):
        progress = simulate_playoffs(east_teams, west_teams, ratings)
        for team, stage_reached in progress.items():
            if stage_reached == 'Champion':
                stage_counts[team]['Champion'] += 1
                stage_counts[team]['Finals'] += 1
                stage_counts[team]['Conference Finals'] += 1
                stage_counts[team]['Conference Semifinals'] += 1
            elif stage_reached == 'Finals':
                stage_counts[team]['Finals'] += 1
                stage_counts[team]['Conference Finals'] += 1
                stage_counts[team]['Conference Semifinals'] += 1
            elif stage_reached == 'Conference Finals':
                stage_counts[team]['Conference Finals'] += 1
                stage_counts[team]['Conference Semifinals'] += 1
            else:
                stage_counts[team]['Conference Semifinals'] += 1
    
    # Convert counts to probabilities
    playoff_odds = defaultdict(dict)

    for team, stages in stage_counts.items():
        for stage, count in stages.items():
            playoff_odds[team][stage] = count / num_simulations
    
    return playoff_odds


# games_2022_23 = get_season_data('2022-23', 'Regular Season')
# games_2022_23_playoffs = get_season_data('2022-23', 'Playoffs')
games_2023_24 = get_season_data('2023-24', 'Regular Season')

# final_games_df = pd.concat([games_2022_23, games_2022_23_playoffs, games_2023_24], ignore_index=True)
elo_ratings = process_historical_data(games_2023_24)

num_simulations = 10000
east_teams, west_teams = get_playoff_teams()
playoff_data = get_playoff_odds(num_simulations, east_teams, west_teams, elo_ratings)

df = pd.DataFrame.from_dict(playoff_data, orient='index', columns=['Conference Semifinals', 'Conference Finals', 'Finals', 'Champion'])
df.fillna(0, inplace=True) 
df.sort_values(by=['Conference Semifinals'], ascending=False, inplace=True)
df = df * 100  # use percentages for readability

# visualize using seaborn
plt.figure(figsize=(12, 8))
sns.heatmap(
    df, 
    annot=True, 
    fmt='.1f', 
    linewidths=.5, 
    cmap='Blues'
)
plt.title(f'Elo-Based NBA Playoff Probabilities for {num_simulations} Simulations')
plt.ylabel('Team')
plt.xlabel('Stage Reached')
plt.show()
