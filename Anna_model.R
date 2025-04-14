#R Code for analysis
#Libraries
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyverse)

#Load in the data
getwd()
#adjust path for each individual 
setwd("~/Desktop/Bayesian/project")

numsheets <- length(excel_sheets("survivoR.xlsx")) 

# Get the sheet names
sheet_names <- excel_sheets("survivoR.xlsx")

# Read each sheet into a list of data frames
df_list <- lapply(sheet_names, function(x) read_excel("survivoR.xlsx", 
                                                      sheet = x))

head(df_list)
# Assign each data frame to the global environment using the sheet names
names(df_list) <- sheet_names
list2env(df_list, envir = .GlobalEnv)


#Want everyone separated by castaway score
cast_detail<-df_list[["Castaway Details"]]
identifiers<-cast_detail$castaway_id

#Remove non-US castaways
uscast_detail <- cast_detail[grepl("^US", cast_detail$castaway_id), ]
#uscast_detail

#verifies that it is only US castaways
#head(uscast_detail)

#####################################

## New Column: 

#Finalists (numerical values of same Tribe members in Jury)

###################
# Get relevant data frames
castaways <- df_list[["Castaways"]] %>% 
  filter(str_starts(castaway_id, "US"))  # Ensure US-only

#colnames(castaways)
jury_votes <- df_list[["Jury Votes"]]
tribe_mapping <- df_list[["Tribe Mapping"]]

## Filter for finalists
finalists <- castaways %>%
  filter(finalist == TRUE) %>%
  select(season, finalist_id = castaway_id, 
         finalist_name = castaway, 
         finalist_tribe = original_tribe,
         winner)

# 2. Get jury members with their original tribes
jury_members <- castaways %>%
  filter(jury == TRUE) %>%
  select(season, jury_id = castaway_id, jury_name = castaway, 
         jury_tribe = original_tribe)


## 3. Count same-tribe jury members for each finalist
finalist_counts <- finalists %>%
  # Explicit many-to-many join
  left_join(
    jury_members,
    by = "season",
    relationship = "many-to-many"
  ) %>%
  # Calculate same-tribe matches
  mutate(
    same_tribe = finalist_tribe == jury_tribe
  ) %>%
  group_by(season, finalist_id, finalist_name, finalist_tribe, winner) %>%
  summarise(
    same_tribe_jury = sum(same_tribe, na.rm = TRUE),
    total_jury = n_distinct(jury_id, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    #proportion (not sure if needed)
    pct_same_tribe = same_tribe_jury / total_jury
  ) %>%
  rename(
    castaway_id = finalist_id,
    castaway = finalist_name,
    original_tribe = finalist_tribe
  )

view(finalist_counts)

# 4. Analyze distribution for prior specification
summary_stats <- finalist_counts %>%
  summarise(
    mean_count = mean(same_tribe_jury),
    sd_count = sd(same_tribe_jury),
    min_count = min(same_tribe_jury),
    max_count = max(same_tribe_jury),
    mean_prop = mean(pct_same_tribe),
    sd_prop = sd(pct_same_tribe)
  )

summary_stats

#To get Latex code for summary statistics: 
library(knitr)
kable(summary_stats, format = "latex", caption = "Summary Statistics")

########################

## Challenge results: combine outcome_type + 
##                  challenge_type + result _> to showcase individual wins, 
##                  tribe wins, which result in rewards or immunity 

########################

#Get challenge Results data
# First ensure we have only US castaways (one-time filter)
us_challenge_results <- df_list[["Challenge Results"]] %>% 
  filter(str_starts(castaway_id, "US"))

# Calculate wins - using precise categorization
challenge_wins <- us_challenge_results %>%
  mutate(
    # Categorize win types
    individual_win = outcome_type == "Individual" & result == "Won",
    tribal_win = outcome_type == "Tribal" & result == "Won",
    
    # Further break down by challenge purpose
    individual_reward = individual_win & challenge_type == "Reward",
    individual_immunity = individual_win & challenge_type == "Immunity",
    tribal_immunity = tribal_win & challenge_type %in% c("Immunity", "Immunity and Reward")
  ) %>%
  group_by(castaway_id) %>%
  summarise(
    # Individual wins
    total_individual_wins = sum(individual_win, na.rm = TRUE),
    reward_challenge_wins = sum(individual_reward, na.rm = TRUE),
    immunity_challenge_wins = sum(individual_immunity, na.rm = TRUE),
    
    # Tribal wins
    total_tribal_wins = sum(tribal_win, na.rm = TRUE),
    tribal_immunity_wins = sum(tribal_immunity, na.rm = TRUE),
    
    # Combined metrics
    total_wins = total_individual_wins + total_tribal_wins,
    .groups = "drop"
  )

# Merge with castaway details
castaway_performance <- df_list[["Castaway Details"]] %>%
  filter(str_starts(castaway_id, "US")) %>%
  left_join(challenge_wins, by = "castaway_id") %>%
  mutate(across(contains("wins"), ~replace_na(., 0))) %>%
  select(
    castaway_id, 
    full_name,
    contains("individual"),
    contains("tribal"),
    contains("reward"),
    contains("immunity"),
    total_wins
  )

# View results
castaway_performance %>% arrange(desc(total_wins)) %>% head(10)
  
# Calculate total times chosen for reward 
chosen_reward_totals <- df_list[["Challenge Results"]] %>%
  filter(str_starts(castaway_id, "US")) %>%  # US seasons only
  group_by(castaway_id) %>%
  summarise(
    times_chosen_for_reward = sum(chosen_for_reward == TRUE, na.rm = TRUE),
    .groups = "drop"
  )

# Merge with existing performance data
castaway_performance <- castaway_performance %>%
  left_join(chosen_reward_totals, by = "castaway_id") %>%
  mutate(times_chosen_for_reward = replace_na(times_chosen_for_reward, 0))

# Verify new column
view(castaway_performance)

############## Summary stats
# Summary Statistics Table
# Season-Specific Summary Statistics
season_stats <- castaway_performance %>%
  left_join(
    df_list[["Castaways"]] %>% 
      filter(str_starts(castaway_id, "US")) %>%
      distinct(castaway_id, season),
    by = "castaway_id"
  ) %>%
  group_by(season) %>%
  summarise(
    # Individual Wins
    Mean_Individual = mean(total_individual_wins, na.rm = TRUE),
    Median_Individual = median(total_individual_wins, na.rm = TRUE),
    SD_Individual = sd(total_individual_wins, na.rm = TRUE),
    
    # Tribal Wins
    Mean_Tribal = mean(total_tribal_wins, na.rm = TRUE),
    Median_Tribal = median(total_tribal_wins, na.rm = TRUE),
    SD_Tribal = sd(total_tribal_wins, na.rm = TRUE),
    
    # Total Wins
    Mean_Total = mean(total_wins, na.rm = TRUE),
    Median_Total = median(total_wins, na.rm = TRUE),
    SD_Total = sd(total_wins, na.rm = TRUE),
    
    # Chosen for Reward
    Mean_Chosen = mean(times_chosen_for_reward, na.rm = TRUE),
    Median_Chosen = median(times_chosen_for_reward, na.rm = TRUE),
    SD_Chosen = sd(times_chosen_for_reward, na.rm = TRUE),
    
    # Counts
    N_Castaways = n(),
    .groups = "drop"
  )

# View season-specific results
season_stats %>%
  gt::gt() %>%
  gt::fmt_number(decimals = 2) %>%
  gt::tab_header(
    title = "Season-by-Season Challenge Performance",
    subtitle = "Average wins per castaway by season"
  )

# Seasons Summary : Winner_ID

#########################

#### Remove unnecessary columns 



