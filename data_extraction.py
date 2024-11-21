import pandas as pd 
import numpy as np 
from statsbombpy import sb

# Potential Title of the Study
# Did Peru actually deserve to advance to Round of 16 based on xG?

# Statsbombpy API - Github link: https://github.com/statsbomb/statsbombpy
# Fortunately, the API follows similar syntax for manipulation as pandas

# See competitions available in statsboms free version
# print(sb.competitions()) # - Interested in WorldCup from 2018. Want to see who deserved to win and subsequently to advance ahead to the next round.

# Competition_id = 3 -> WorldCup, season_id=3 -> 2018
a = sb.matches(competition_id=43, season_id=3)

# Only keeping data from the group stage
a = a[a['competition_stage'] == 'Group Stage']

# Keeping records of teams that were part of the group
# [7530, 7532, 7546, 7547, 7562, 7563]
list_of_interest = ['Peru', 'France', 'Denmark', 'Australia']
a = a[a['home_team'].isin(list_of_interest)]

# Order it in ascending order to reflect the order of the games in reality
a = a.sort_values(by='match_id', ascending=True)

# Extracting match_id for each game and storing them into a list
matches_list = a['match_id'].to_list()

# Build a function to cleanse data programatically
def data_cleansing(match_id):

    df = sb.events(match_id = match_id)

    # Only keeping records where 'shot_statsbomb_xg' is not null
    df = df[df['shot_statsbomb_xg'].notnull()]
    
    # Adding the identifier for the game
    df['match_id_script'] = match_id

    # Keeping only needed columns
    columns = ['match_id', 'location', 'minute', 'player', 'position',
                 'possession_team', 'shot_statsbomb_xg', 'shot_outcome', 'team', 'timestamp', 'type']

    df = df[columns]

    return df

# create an empty dataframe to store the data from multiple games
matches_df = pd.DataFrame()

# Looping applying the function and appending the output in the final dataframe
for matches in matches_list:

    # Applying the function
    match_data = data_cleansing(matches)

    # Concatenate into a single dataframe
    matches_df = pd.concat([matches_df, match_data], ignore_index=True)
    

# Save the dataframe into a  csv
matches_df.to_csv('WC_Group_C_2018.csv', index=False)


# To run: python data_extraction.py