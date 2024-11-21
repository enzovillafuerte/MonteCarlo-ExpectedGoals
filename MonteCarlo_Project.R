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



## 1. API Connection Section



## 2. Function Generating based on a sample (Barca vs Man Utd 2011.csv)

# Read the dataset
data <- read.csv('BarcavsManUtd2011.csv')

head(data)
colnames(data)


# Filter dataframe
# 1st Filter -> Keep Only shots data
# Where shot_statsbomb_xg IS NOT NULL





head(data)
length(data)
# Function simulating a match

# kee