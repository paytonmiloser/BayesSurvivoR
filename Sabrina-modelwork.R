#Change to your filepath for the excel document
survivoR_filepath <- "C:/Users/sabri/OneDrive - Iowa State University/Documents/BayesSurvivoR/survivoR.xlsx"
#Reading in Dataset
library(readxl)
sheet_names <- excel_sheets(survivoR_filepath)
# Read each sheet into a list of data frames
df_list <- lapply(sheet_names, function(x) read_excel("survivoR.xlsx", sheet = x))
# Assign each data frame to the global environment using the sheet names
names(df_list) <- sheet_names
list2env(df_list, envir = .GlobalEnv)
sheet_names





#EDA before modeling

#Want everyone separated by castaway score
cast_detail<-df_list[["Castaway Details"]]
identifiers<-cast_detail$castaway_id
#Remove non-US castaways
uscast_detail <- cast_detail[grepl("^US", cast_detail$castaway_id), ]
uscast_detail

##################################################

#Separating occupations

unique(uscast_detail$occupation)
#Check how many unknown collars there are
sum(uscast_detail$collar == "Unknown")
#Create new df for unknown collars
unknown_collar <- uscast_detail[uscast_detail$collar == "Unknown", ]
#unknown_collar$occupation-- found the occupation list for the unknown collars
library(dplyr)
#Replace unknown with collar category
uscast_detail <- uscast_detail %>%
  mutate(collar = case_when(
      collar == "Unknown" & grepl("ivy league graduate|harvard law student|law student|aspiring writer", occupation, ignore.case = TRUE) ~ "White collar",
      collar == "Unknown" & grepl("student|dental student|retired navy seal|retired police officer|retired teacher|former navy fighter pilot|single mom|aspiring writer|ex-nfl player|retired mlb player|iraq war veteran|army veteran|air force veteran|ex-nfl player's wife;homemaker", occupation, ignore.case = TRUE) ~ "No collar",
      TRUE ~ collar #keep original collar if not unknown
      ))

library(stringr)
#Standardize to existing collar categories
uscast_detail <- uscast_detail %>%
  mutate(collar = str_to_title(collar))

#Visualize for prior
#Calculate the proportions of each collar category
collar_distribution <- uscast_detail %>%
  group_by(collar) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count))

library(ggplot2)
ggplot(collar_distribution, aes(x = collar, y = proportion, fill = collar)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Distribution of Collar Categories", x = "Collar", y = "Proportion")


#########################################################

#Creating ethnicity column

uscast_detail <- uscast_detail %>%
  rowwise() %>%
  mutate(ethnicity = paste(c(
        if (african) "African" else NULL,
        if (asian) "Asian" else NULL,
        if (latin_american) "Latin American" else NULL,
        if (native_american) "Native American" else NULL
      ),
      collapse = "; "
    ),
    ethnicity = ifelse(ethnicity == "", "White", ethnicity)
  ) %>%
  ungroup()
head(uscast_detail$ethnicity) #categories including dual-combinations of each

#########################################################

#Create a column for total number of advantage for each US season

library(dplyr)
library(tidyr)
library(stringr)
advantage_details <- df_list[["Advantage Details"]]

#filter only US
us_advantages <- advantage_details %>%
  filter(str_starts(version_season, "US"))

advantage_counts <- us_advantages %>%
  count(version_season, name = "total_advantages")

all_us_seasons <- tibble(version_season = paste0("US", 1:48))

advantages_per_us_season <- all_us_seasons %>%
  left_join(advantage_counts, by = "version_season") %>%
  replace_na(list(total_advantages = 0))

#######################################################################

library(dplyr)
library(stringr)

# Load in data
castaways <- df_list[["Castaways"]]
advantage_movement <- df_list[["Advantage Movement"]]

# Filter advantage movement to only US seasons
us_advantage_movement <- advantage_movement %>%
  filter(str_starts(version_season, "US"))

# Define events of interest
events_of_interest <- c("Found", "Received", "Found (beware)", "Recieved", 
                        "Found (Beware)", "Bought", "Won", "Stolen")

# Get castaway IDs for those who had advantages
castawayid_advantages <- us_advantage_movement %>%
  filter(event %in% events_of_interest) %>%
  pull(castaway_id) %>%
  unique()

# Filter US castaways based on version_season (safer)
us_castaways <- castaways %>%
  filter(str_starts(version_season, "US")) %>%
  mutate(got_advantage = castaway_id %in% castawayid_advantages)

# View result
head(us_castaways %>% select(castaway_id, version_season, got_advantage))

#########################################################################

#T/F if they played an advantage for themselves

library(dplyr)

us_advantage_movement <- advantage_movement %>%
  filter(str_starts(version_season, "US")) %>%
  mutate(
    played_for_self = if_else(
      event == "Played" & castaway == played_for,
      TRUE,
      FALSE
    )
  )
us_advantage_movement$played_for_self

#########################################################################

#True/False for nickname

library(dplyr)
library(stringr)

uscast_detail <- uscast_detail %>%
  mutate(
    first_name = word(full_name, 1),
    nickname = first_name != castaway
  ) %>%
  select(-first_name)
uscast_detail$nickname

####################################################################

#Now pull other prior information we'll be using from Advantage Details, Advantage Movement, and Castaway Details

#Advantage Details

#Advantage Movement
advantage_success<-us_advantage_movement$success

#Castaway Details
gender<-uscast_detail$gender
lgbt<-uscast_detail$lgbt


#################################################################


#Creating a new Data Frame

library(dplyr)

advantage_summary <- us_advantage_movement %>%
  select(castaway_id, version_season, success, played_for_self)

cast_detail_reduced <- uscast_detail %>%
  select(castaway_id, full_name, lgbt, gender, collar, ethnicity, nickname)

castaways_reduced <- us_castaways %>%
  select(castaway_id, version_season, got_advantage)

merged_data <- castaways_reduced %>%
  left_join(cast_detail_reduced, by = "castaway_id") %>%
  left_join(advantage_summary, by = c("castaway_id", "version_season")) %>%
  left_join(advantages_per_us_season, by = "version_season")


head(merged_data)

library(writexl)
write_xlsx(merged_data, path = "merged_survivor_data.xlsx")
