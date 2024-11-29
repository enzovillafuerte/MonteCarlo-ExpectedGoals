# MonteCarlo Project

# setwd('Documents/Github/MonteCarlo-ExpectedGoals')
# Peru vs Denmark: 7532

# Import libraries
library(readr)
library(ggplot2)
library(RColorBrewer)
library(viridis)


##########################################################################################
# DATA IMPORT SECTION
##########################################################################################

# Data is being extracted using Statsbomb's Python API
# See data_extraction.py script for more reference

# Read the dataset with the consolidated data
data <- read.csv('WC_Group_C_2018.csv')

# Get a list of the different matches_id in the dataset
games_id <- unique(data$match_id)

# Iterate the list using a for loop referencing the match_id for each game as the main parameter

# match_id of the game we are interested
a = 7532 # Peru vs Denmark
# a = 7546 # Peru vs France
# a = 7562 # Peru vs Australia

#######################################################################################################################################################
# MAIN FUNCTION GENERATION

montecarlo_simulation <- function(match_id, k = 10000, output_dir = 'plots'){
  ##########################################################################################
  # SIMULATING ONE GAME SECTION
  ##########################################################################################
  
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
  
  result <- simulate_game(match_id)
  #print(result[[1]] > result[[2]])
  #print(result$home_team)
  # Function simulating a match
  
  
  ##########################################################################################
  # ITERATING K TIMES - MONTECARLO EXPECTED GOALS
  ##########################################################################################
  
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
      simulation <- simulate_game(match_id)
      
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
  
  final_montecarlo <- k_simulations(match_id)
  
  
  ##########################################################################################
  # GRAPH SECTION FOR REPORT
  ##########################################################################################
  # color pallettes for brewer: https://r-graph-gallery.com/38-rcolorbrewers-palettes.html
  
  # Setting output directory to save the plots for report
  # output_dir <- 'plots'
  
  # Extracting names and probabilities
  team_names <- names(final_montecarlo)
  probabilities <- unlist(final_montecarlo)  # Converting into a vector
  
  # Head-to-Head -- Graph 1
  df_h2h <- data.frame(
    Outcome = team_names[1:3],   # Only getting Home,Draw, and Away
    Probability = probabilities[1:3]
  )
  
  h2h_plot <- ggplot(df_h2h, aes(x = Outcome, y = Probability, fill = Outcome)) +
    geom_bar(stat = "identity") +
    ggtitle("Head-to-Head Probabilities") +
    xlab("Outcome") +
    ylab("Probability") +
    scale_y_continuous(limits = c(0, 1)) +
    scale_fill_brewer(palette = "PuBu") + 
    theme_minimal()
  
  # Saving the plot
  ggsave(filename= file.path(output_dir, sprintf('h2h_probabilities_%s.png', match_id)),
         plot = h2h_plot, width = 6, height = 4, dpi =300)
  
  # Over/Under Goals -- Graph 2
  df_ou <- data.frame(
    Outcome = team_names[4:5],
    Probability = probabilities[4:5]
  )
  
  ou_plot <- ggplot(df_ou, aes(x = Outcome, y = Probability, fill = Outcome)) +
    geom_bar(stat = "identity") +
    ggtitle("Over/Under 2.5 Goals Probabilities") +
    xlab("Outcome") +
    ylab("Probability (%)") +
    scale_y_continuous(limits = c(0, 1)) +
    scale_fill_brewer(palette = "PuBu") + # PuBu
    theme_minimal()
  
  # Saving the plot
  ggsave(filename= file.path(output_dir, sprintf('o_u_probabilities_%s.png', match_id)),
         plot = ou_plot, width = 4, height = 4, dpi = 300)
  
  ##########################################################################################
  # EXPECTED POINTS SECTION
  ##########################################################################################
  # Using the paper Monte Carlo Simulations and Applications in Sports as Reference
  # Expected Points Formula:
  # xPts = 3 * P(win) + 1 * P(tie) + 0 * P(loss)
  
  calculate_xpts <- function(montecarlo_output){
    
    final_montecarlo <- montecarlo_output
    
    # Accessing the data from the input
    # Extracting names and probability
    team_names <- names(final_montecarlo)
    probabilities <- unlist(final_montecarlo)  # Converting into a vector
    
    # Only keeping variables of interest for the xPTS formula (Home, Draw, Away)
    outcomes_of_interest <- team_names[1:3]
      
    # Assignings Probabilities
    home_prob <-probabilities[1]
    draw_prob <- probabilities[2]
    away_prob <- probabilities[3]
    
    # Expected Points Formula - Vould great a function here but it would make it messier imo
    home_xPTS <- 3 * home_prob + 1 * draw_prob + 0 * away_prob
    away_xPTS <- 3 * away_prob + 1 * draw_prob + 0 * home_prob
      
    # Removing the inherited names from the unlist of probabilities
    home_xPTS <- unname(home_xPTS)
    away_xPTS <- unname(away_xPTS)
    
    # return(c(home_xPTS, away_xPTS))
    return(setNames(
      list(home_xPTS, away_xPTS),
      c(team_names[1], team_names[3])
    ))
    
  }
  
  xPTS_output <- calculate_xpts(final_montecarlo)
  
  # Combining the results into a single row for dataframe export into csv
  result <- data.frame(
    MatchID = match_id,
    HomeTeam = names(final_montecarlo)[1],
    AwayTeam = names(final_montecarlo)[3],
    HomeWinProb = final_montecarlo[[1]],
    DrawProb = final_montecarlo[[2]],
    AwayWinProb = final_montecarlo[[3]],
    Over2_5Prob = final_montecarlo[[4]],
    Under2_5Prob = final_montecarlo[[5]],
    Home_xPTS = xPTS_output[[1]],
    Away_xPTS = xPTS_output[[2]]
  )
  
  
  # return(list(Probabilities = final_montecarlo, ExpectedPoints = xPTS_output)) # Uncomment to keep output within R only
  return(result)
}
 
# MAIN FUNCTION END
#######################################################################################################################################################

# Running the final simulation function through all the games in the list
# final_output_results <- lapply(games_id, montecarlo_simulation) # Uncomment to keep output within R only
# Store into dataframe and export as csv 
final_output_results <- do.call(rbind, lapply(games_id, montecarlo_simulation))

write.csv(final_output_results, "output_analysis/final_output_results.csv", row.names = FALSE)

