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
uscast_detail

#verifies that it is only US castaways
head(uscast_detail)

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

#########################

# New column: 
#             outcome type >- Sum of individual wins (for each castaway)
#             outcome type >- Sum of Tribe wins
#             Sum "chosen_for_reward" for the castaway
###################

########################


# Seasons Summary : Winner_ID
# From Jury Votes: Get Finalist_ID to use as an identifier. 

#########################






#### Remove unnecessary columns 



