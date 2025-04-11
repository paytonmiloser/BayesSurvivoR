#R Code for analysis
#Libraries
library(readxl)
library(ggplot2)
library(dplyr)

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

# Assign each data frame to the global environment using the sheet names
names(df_list) <- sheet_names
list2env(df_list, envir = .GlobalEnv)

sheet_names
#EDA before modeling
library(tidyverse)

# Load jury votes and castaway data
jury_votes <- df_list[["Jury Votes"]]
castaways <- df_list[["Castaways"]]

#Checking structure: # Check jury_votes$vote (the column causing issues)
head(jury_votes$vote)  # Is this a name or an ID?

# Check castaways$castaway (the target column)
head(castaways$castaway)  # Is this a name or an ID?

colnames(castaways)

head(castaways$castaway_id)

# Check for duplicate castaway_num_id within seasons in castaways
castaways %>%
  mutate(castaway_num_id = as.numeric(sub("^[A-Za-z]+", "", castaway_id))) %>%
  count(season, castaway_num_id) %>%
  filter(n > 1)

# Check for duplicate votes in jury_votes
jury_votes %>%
  count(season, vote) %>%
  filter(n > 1)

#--Errors--
jury_analysis <- jury_votes %>%
  left_join(
    castaways %>%
      mutate(
        # Extract numeric portion from castaway_id (e.g., "US0001" -> 1)
        castaway_num_id = as.numeric(sub("^[A-Za-z]+", "", castaway_id))
      ) %>%
      select(season, castaway_num_id, castaway, result),
    by = c("season", "vote" = "castaway_num_id")  # Join on numeric IDs
  ) %>%
  mutate(
    sole_survivor = ifelse(result == "Sole Survivor", 1, 0)
  )

# Distribution of votes for Sole Survivors vs. losers
vote_summary <- jury_analysis %>%
  group_by(vote, sole_survivor) %>%
  summarise(total_votes = n()) %>%
  ungroup()

ggplot(vote_summary, aes(x = factor(sole_survivor), y = total_votes)) +
  geom_boxplot(fill = "goldenrod") +
  labs(
    title = "Jury Votes for Sole Survivors vs. Runners-Up",
    x = "Sole Survivor (1 = Yes, 0 = No)",
    y = "Number of Jury Votes Received"
  )


# Win rate by season characteristics
Castaways %>%
  group_by(season, season_name) %>%
  summarise(win_rate = mean(result == "Sole Survivor")) %>%
  ggplot(aes(x = season, y = win_rate)) + geom_col()

#Looking at the Jury Votes
vote_distribution <- jury_votes %>%
  count(season, vote) %>%  # Count votes per finalist per season
  group_by(season) %>%
  mutate(total_votes = sum(n)) %>%
  ungroup()

# Plot histogram of votes per finalist
ggplot(vote_distribution, aes(x = n)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(
    title = "Distribution of Jury Votes Received per Finalist",
    x = "Number of Votes Received",
    y = "Frequency"
  ) +
  theme_minimal()

#Are certain seasons tighter in votes? 
vote_spreads <- jury_votes %>%
  group_by(season) %>%
  summarise(
    total_votes = n(),
    winner_votes = max(table(vote)),
    vote_spread = winner_votes / total_votes  # % of votes winner got
  )

# Plot vote spreads across seasons
ggplot(vote_spreads, aes(x = season, y = vote_spread)) +
  geom_col(fill = "purple") +
  labs(
    title = "How Dominant Are Sole Survivors?",
    subtitle = "Percentage of Jury Votes Won by the Winner",
    x = "Season",
    y = "% of Votes Received by Winner"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent)

