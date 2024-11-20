# MonteCarlo Project
# Define Scope and Objective
# Use of xG to predict the outcome of a game based on xG instead of Poisson
# https://github.com/sonofacorner/matplotlib-tutorials/blob/main/intro_monte_carlo/main_notebook.ipynb
# setwd('Documents/Github/MonteCarlo-ExpectedGoals')


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