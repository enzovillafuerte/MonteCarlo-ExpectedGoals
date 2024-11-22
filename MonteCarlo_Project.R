# MonteCarlo Project
# Define Scope and Objective
# Use of xG to predict the outcome of a game based on xG instead of Poisson
# https://github.com/sonofacorner/matplotlib-tutorials/blob/main/intro_monte_carlo/main_notebook.ipynb
# setwd('Documents/Github/MonteCarlo-ExpectedGoals')

# To-do Next: Automate extraction for other matches using dictionary and concatenate all into a consolidated dataset

# Only then start coding in R
#       - Probabilities of each team winning
#       - xG Flowchart + Probability visualization
#       - 3 points for the team that deserved to win
#       - Expected Points Model to see if we should have passed
# Peru vs Denmark: 7532


# Import libraries
library(readr)



# Data is being extracted using Statsbomb's Python API
# See data_extraction.py script for more reference

# Read the dataset with the consolidated data
data <- read.csv('WC_Group_C_2018.csv')

# Get a list of the column names
colnames(data)

#############################################
# SIMULATING ONE GAME SECTION
#############################################

# Filtering to keep only the data for the game I need: Peru vs Denmark
# match_id = 7532
simulate_game <- function(match_id) {
  
  # Filtering the game to account only for the game of interest
  game_data <- data[data$match_id == match_id,]
  
  # Getting a list of the two teams involved -distinct Python/SQL | unique R
  teams <- unique(game_data$team)
  
  # Indexing the list and assigning the values to both home and away_teams
  home_team <- teams[1] # i.e. "Peru"
  away_team <- teams[2] # i.e. "Denmark"
  
  # Filtering the dataframe to two different dataframes for each team
  home_shots <- game_data[game_data$team == home_team,]
  away_shots <- game_data[game_data$team == away_team,]
  
  # Simulating -- Home Team -- Goals
  # Defining a counter to count the number of goals
  home_goals <- 0 
  
  # Making sure we only execute if there are records (shots) in the dataframe
  # Some teams may have done so bad that did not shoot in the entire game. Rare but possible.
  if (nrow(home_shots) > 0) {
    
    # For loop to iterate over the shots in the game
    for (shot in home_shots$shot_statsbomb_xg) {
      
      # Sampling a random number between 0 and 1
      # We use runif following a uniform distribution
      prob <- runif(1)
      
      # If the random number is less than the Expected Goals (xG) then it counts as a goal
      # Explain and Expand in report the reasoning behind this
      if (prob < shot) {
        home_goals <- home_goals + 1
      }
    }
  }
  
  # Simulating -- Away Team -- Goals
  # Defining a counter for away team
  away_goals <- 0
  
  # Repeat the same block of code but for the away counter
  if (nrow(away_shots) > 0) {
    
    for (shot in away_shots$shot_statsbomb_xg){
      prob <- runif(1)
      
      if (prob < shot) {
        away_goals <- away_goals + 1
      }
    }
  }
  
  return(setNames(
    list(home_goals, away_goals),
    c(home_team, away_team)
  ))
}

# match_id of the game we are interested
a = 7532

result <- simulate_game(a)
print(result[[1]] > result[[2]])
print(result$home_team)
# Function simulating a match


#############################################
# ITERATING K TIMES - MONTECARLO EXPECTED GOALS
#############################################

k_simulations <- function(match_id, k=10000){
  
  # Getting the team names section -- Keeping comments out for readability
  game_data <- data[data$match_id == match_id,]
  teams <- unique(game_data$team)
  home_team <- teams[1] # i.e. "Peru"
  away_team <- teams[2] # i.e. "Denmark"
  
  # H2H (Head to Head) Ocurrences
  home <- 0
  draw <- 0
  away <- 0
  
  # Over 2.5 Goals Ocurrences
  o_2_5 <- 0
  u_2_5 <- 0
  
  # Creating the for loop to iterate the previous function k times and store results
  for (i in 1:k) {
    
    # Apply the function
    simulation <- simulate_game(a)
    
    # If statements to populate the H2H and O2.5 variables based on simulation
    if (simulation[[1]] > simulation[[2]]) {
      home <- home + 1
      if (simulation[[1]] + simulation[[2]] > 2.5) {
        o_2_5 <- o_2_5 + 1
      }
      else {
        u_2_5 <- u_2_5 + 1
      }
    }
    
    else if (simulation[[1]] < simulation[[2]]) {
      away <- away + 1
      if (simulation[[1]] + simulation[[2]] > 2.5) {
        o_2_5 <- o_2_5 + 1
      }
      else {
        u_2_5 <- u_2_5 + 1
      }
    }
    
    else {
      draw <- draw + 1
      if (simulation[[1]] + simulation[[2]] > 2.5) {
        o_2_5 <- o_2_5 + 1
      }
      else {
        u_2_5 <- u_2_5 + 1
      }
    }
  }
  
  # Calculate probabilities for each outcome
  # H2H
  home_prob <- home / k
  draw_prob <- draw / k
  away_prob <- away / k
  
  # O/U
  o2_5_prob <- o_2_5 / k
  u2_5_prob <- u_2_5 / k
    
  
  return(setNames(
    list(home_prob, draw_prob, away_prob, o2_5_prob, u2_5_prob),
    c(home_team, 'Draw', away_team, '+2.5', '-2.5')
  ))
}

final_montecarlo <- k_simulations(a)
print(final_montecarlo)
